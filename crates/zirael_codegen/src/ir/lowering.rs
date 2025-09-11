use crate::ir::{
  IrBlock, IrEnum, IrExpr, IrExprKind, IrField, IrFunction, IrFunctionExtern, IrItem, IrItemKind,
  IrMatchArm, IrModule, IrParam, IrPattern, IrStmt, IrStruct, IrVariant, IrVariantData,
};
use itertools::Itertools as _;
use std::{
  collections::{HashMap, HashSet},
  path::PathBuf,
  vec,
};
use zirael_hir::hir::{
  HirBody, HirEnum, HirFunction, HirItem, HirItemKind, HirModule, HirStruct, HirTypeExtension,
  expr::{AccessKind, FieldSymbol, HirExpr, HirExprKind, HirMatchArm, HirPattern, HirStmt},
};
use zirael_parser::{
  AstId, CallInfo, DropStackEntry, EnumVariantData, GenericParameter, MainFunction, MonomorphizationId,
  ScopeType, StructField, Symbol, SymbolId, SymbolKind, SymbolRelationNode, SymbolTable, Type,
  UnaryOp,
};
use zirael_type_checker::monomorphization::MonomorphizationTable;
use zirael_utils::prelude::{
  Colorize as _, Mode, ReportBuilder, ReportKind, Reports, SourceFileId, Sources, debug,
  get_or_intern, resolve, warn,
};

pub fn lower_hir_to_ir<'reports>(
  hir_modules: &mut Vec<HirModule>,
  symbol_table: &SymbolTable,
  mono_table: MonomorphizationTable,
  reports: &Reports<'reports>,
  sources: &Sources,
  mode: Mode,
  root: PathBuf,
  main_function_id: &mut Option<MainFunction>,
) -> Vec<IrModule> {
  let mut lowering = HirLowering::new(symbol_table, mono_table, reports, sources, mode, root);
  lowering.lower_modules(hir_modules, main_function_id)
}

pub struct HirLowering<'reports> {
  pub symbol_table: SymbolTable,
  pub mono_table: MonomorphizationTable,
  processed_file: Option<SourceFileId>,
  pub sources: Sources,
  pub mode: Mode,
  pub root: PathBuf,
  pub reports: Reports<'reports>,
  pub current_mono_id: Option<MonomorphizationId>,
  pub current_symbol_id: Option<SymbolId>,
  pub current_items: Vec<IrItem>,
}

impl<'reports> HirLowering<'reports> {
  pub fn new(
    symbol_table: &SymbolTable,
    mono_table: MonomorphizationTable,
    reports: &Reports<'reports>,
    sources: &Sources,
    mode: Mode,
    root: PathBuf,
  ) -> Self {
    Self {
      symbol_table: symbol_table.clone(),
      mono_table,
      processed_file: None,
      sources: sources.clone(),
      reports: reports.clone(),
      mode,
      root,
      current_mono_id: None,
      current_symbol_id: None,
      current_items: vec![],
    }
  }

  pub fn lower_modules(
    &mut self,
    lexed_modules: &mut Vec<HirModule>,
    main_function_id: &mut Option<MainFunction>,
  ) -> Vec<IrModule> {
    let mut ir_modules = lexed_modules
      .iter_mut()
      .map(|module| self.lower_module(module, main_function_id))
      .collect::<Vec<_>>();

    if !self.mono_table.entries.is_empty() {
      let all_modules = ir_modules.clone();
      let mut processed_monos = HashSet::new();

      for module in &mut ir_modules {
        self.process_monomorphization_entries(module, &all_modules, &mut processed_monos);
      }
    }

    ir_modules
  }

  pub fn push_scope(&mut self, scope_type: ScopeType) {
    let _ = self.symbol_table.push_scope(scope_type);
  }

  pub fn pop_scope(&mut self) -> Vec<DropStackEntry> {
    let pop = self.symbol_table.pop_scope().unwrap();
    self.symbol_table.get_scope_unchecked(pop).drop_stack
  }

  pub fn lower_type(&mut self, ty: Type) -> Type {
    match ty {
      Type::Reference(ty) => Type::Reference(Box::new(self.lower_type(*ty))),
      Type::MonomorphizedSymbol(sym) => self.handle_monomorphized_symbol(&sym, true),
      Type::Named { name, generics } => {
        let symbol = self.symbol_table.lookup_symbol(&name);

        if let Some(symbol) = symbol {
          let mangled = self.mangle_symbol(symbol.id);
          let string = if let SymbolKind::Struct { .. } = symbol.kind {
            format!("struct {mangled}")
          } else if let SymbolKind::EnumVariant { parent_enum, .. } = symbol.kind {
            let en = self.symbol_table.get_symbol_unchecked(&parent_enum);
            self.mangle_symbol(en.id)
          } else {
            mangled
          };
          Type::Named { name: get_or_intern(&string, None), generics }
        } else {
          Type::Named { name, generics }
        }
      }
      _ => ty,
    }
  }

  fn get_mono_variant_name(
    &mut self,
    variant_symbol_id: SymbolId,
    scrutinee_type: &Type,
  ) -> String {
    let variant_symbol = self.symbol_table.get_symbol_unchecked(&variant_symbol_id);

    match self.try_build_monomorphized_name(&variant_symbol, scrutinee_type) {
      Some(name) => name,
      None => self.mangle_symbol(variant_symbol_id),
    }
  }

  fn try_build_monomorphized_name(
    &mut self,
    variant_symbol: &Symbol,
    scrutinee_type: &Type,
  ) -> Option<String> {
    let parent_enum_id = self.get_parent_enum_id(variant_symbol)?;
    let enum_symbol = self.symbol_table.get_symbol_unchecked(&parent_enum_id);
    let generics = self.get_enum_generics(&enum_symbol)?;

    let base_name = self.build_base_variant_name(variant_symbol);
    let parent_name = self.mangle_symbol(enum_symbol.id);

    if self.should_use_monomorphized_name(variant_symbol.id, generics) {
      let type_suffix = self.extract_type_suffix(scrutinee_type)?;
      Some(format!("{parent_name}_{base_name}__{type_suffix}"))
    } else {
      Some(format!("{parent_name}_{base_name}"))
    }
  }

  fn get_parent_enum_id(&self, variant_symbol: &Symbol) -> Option<SymbolId> {
    match &variant_symbol.kind {
      SymbolKind::EnumVariant { parent_enum, .. } => Some(*parent_enum),
      _ => None,
    }
  }

  fn get_enum_generics<'a>(&self, enum_symbol: &'a Symbol) -> Option<&'a [GenericParameter]> {
    match &enum_symbol.kind {
      SymbolKind::Enum { generics, .. } => Some(generics),
      _ => None,
    }
  }

  fn build_base_variant_name(&mut self, variant_symbol: &Symbol) -> String {
    let canonical_symbol = self.symbol_table.get_symbol_unchecked(&variant_symbol.canonical_symbol);
    format!("zirael_{}", self.get_sym_name(&canonical_symbol, None))
  }

  fn should_use_monomorphized_name(
    &self,
    variant_id: SymbolId,
    generics: &[GenericParameter],
  ) -> bool {
    !generics.is_empty() && self.mono_table.has_entries(variant_id)
  }

  fn extract_type_suffix(&mut self, scrutinee_type: &Type) -> Option<String> {
    match scrutinee_type {
      Type::Named { generics: concrete_generics, .. } if !concrete_generics.is_empty() => {
        let suffix = concrete_generics
          .iter()
          .map(|ty| self.mangle_type_for_name(ty))
          .collect::<Vec<_>>()
          .join("_");
        Some(suffix)
      }
      _ => None,
    }
  }

  fn lower_module(
    &mut self,
    lexed_module: &mut HirModule,
    main_function_id: &mut Option<MainFunction>,
  ) -> IrModule {
    let mut ir_module = IrModule { items: vec![], mono_items: vec![], id: lexed_module.id };

    self.push_scope(ScopeType::Module(lexed_module.id));
    self.processed_file = Some(lexed_module.id);

    for item in &mut lexed_module.items.values_mut() {
      if let Some(ir_item) = self.lower_item(item) {
        ir_module.items.push(ir_item);
      }
    }
    ir_module.items.append(&mut self.current_items);

    if let Some(main_symbol_id) = main_function_id
      && let MainFunction::Symbol(main_symbol_id) = main_symbol_id
      && lexed_module.items.values().any(|item| item.symbol_id == *main_symbol_id)
    {
      *main_function_id = Some(MainFunction::Mangled(self.mangle_symbol(*main_symbol_id)));
    }
    self.pop_scope();

    ir_module
  }

  fn lower_item(&mut self, item: &mut HirItem) -> Option<IrItem> {
    let sym = self.symbol_table.get_symbol_unchecked(&item.symbol_id);
    let name = self.mangle_symbol(item.symbol_id);

    self.current_symbol_id = Some(item.symbol_id);
    match &mut item.kind {
      HirItemKind::Function(func) => {
        let ir_function = self.lower_function(resolve(&sym.name), name.clone(), func);

        Some(IrItem {
          name,
          kind: IrItemKind::Function(ir_function),
          sym_id: item.symbol_id,
          mono_id: None,
        })
      }
      HirItemKind::Struct(hir_struct) => {
        let fields = if let SymbolKind::Struct { fields, .. } = sym.kind {
          fields.clone()
        } else {
          unreachable!()
        };

        let ir_struct = self.lower_struct(name.clone(), hir_struct, fields);
        Some(IrItem {
          name,
          kind: IrItemKind::Struct(ir_struct),
          sym_id: item.symbol_id,
          mono_id: None,
        })
      }
      HirItemKind::Enum(hir_enum) => Some(IrItem {
        name: name.clone(),
        kind: IrItemKind::Enum(self.lower_enum(name, hir_enum)),
        sym_id: item.symbol_id,
        mono_id: None,
      }),
      HirItemKind::TypeExtension(ext) => {
        self.lower_type_extension(ext);
        None
      }
    }
  }

  fn lower_type_extension(&mut self, ext: &mut HirTypeExtension) {
    self.push_scope(ScopeType::TypeExtension(ext.id));
    self.lower_methods(&mut ext.methods);
    self.pop_scope();
  }

  fn lower_methods(&mut self, methods: &mut Vec<HirItem>) {
    for item in methods.iter_mut() {
      if let Some(item) = self.lower_item(item) {
        self.current_items.push(item);
      } else {
        warn!("failed to lower method");
      }
    }
  }

  fn lower_enum(&mut self, name: String, hir_enum: &mut HirEnum) -> IrEnum {
    self.push_scope(ScopeType::Enum(hir_enum.id));
    self.lower_methods(&mut hir_enum.methods);

    let variants = hir_enum
      .variants
      .iter()
      .map(|v| IrVariant {
        name: self.mangle_symbol(v.symbol_id),
        symbol_id: v.symbol_id,
        data: match &v.data {
          EnumVariantData::Unit => IrVariantData::Unit,
          EnumVariantData::Struct(fields) => {
            IrVariantData::Struct(self.lower_fields(fields.clone()))
          }
        },
      })
      .collect_vec();
    self.pop_scope();

    for variant in &variants {
      self.current_items.push(IrItem {
        name: variant.name.clone(),
        kind: IrItemKind::EnumVariant(variant.clone()),
        sym_id: variant.symbol_id,
        mono_id: None,
      });
    }

    IrEnum { variants, name }
  }

  fn lower_fields(&mut self, fields: Vec<StructField>) -> Vec<IrField> {
    fields
      .iter()
      .map(|field| IrField { name: resolve(&field.name), ty: self.lower_type(field.ty.clone()) })
      .collect_vec()
  }

  fn lower_struct(
    &mut self,
    name: String,
    hir_struct: &mut HirStruct,
    fields: Vec<StructField>,
  ) -> IrStruct {
    self.push_scope(ScopeType::Struct(hir_struct.id));
    self.lower_methods(&mut hir_struct.methods);
    let fields = self.lower_fields(fields);
    self.pop_scope();
    IrStruct { fields, name, id: hir_struct.id }
  }

  fn lower_function(
    &mut self,
    original_name: String,
    name: String,
    func: &mut HirFunction,
  ) -> IrFunction {
    self.push_scope(ScopeType::Function(func.id));
    let parameters = func
      .signature
      .parameters
      .iter()
      .map(|p| IrParam { name: self.mangle_symbol(p.symbol_id), ty: self.lower_type(p.ty.clone()) })
      .collect::<Vec<_>>();

    let mut body = self.lower_body(func.body.clone());
    let return_type = self.lower_type(func.signature.return_type.clone());
    let entries = self.pop_scope();
    if let Some(body) = &mut body {
      self.add_drop(entries, &mut body.stmts);
    }

    IrFunction {
      name,
      parameters,
      return_type,
      body,
      is_const: func.is_const,
      is_async: func.is_async,
      extern_: if func.is_extern {
        Some(IrFunctionExtern { original_name, abi: func.abi.clone() })
      } else {
        None
      },
      id: func.id,
    }
  }

  fn lower_body(&mut self, body: Option<HirBody>) -> Option<IrBlock> {
    let body = body?;

    if let HirExprKind::Block(stmts) = body.root_expr.kind {
      Some(self.lower_block(stmts, body.root_expr.id))
    } else {
      unreachable!()
    }
  }

  fn heap_variable(&mut self, name: String, ty: Type) -> IrStmt {
    let lowered_ty = self.lower_type(ty.clone());
    IrStmt::Var(
      name,
      IrExpr::new(
        self.lower_type(Type::Pointer(Box::new(ty.clone()))),
        IrExprKind::CCall(
          "malloc".to_owned(),
          vec![IrExpr::new(
            self.lower_type(Type::Int),
            IrExprKind::CCall(
              "sizeof".to_owned(),
              vec![IrExpr::new(lowered_ty.clone(), IrExprKind::Type(lowered_ty))],
            ),
          )],
        ),
      ),
    )
  }

  fn after_heap_assigment(&mut self, name: String, expr: IrExpr) -> IrStmt {
    IrStmt::Expr(IrExpr::new(
      self.lower_type(expr.ty.clone()),
      IrExprKind::Assign(
        Box::new(IrExpr::new(
          self.lower_type(Type::Inferred),
          IrExprKind::Unary(
            UnaryOp::Deref,
            Box::new(IrExpr::new(
              self.lower_type(expr.ty.clone()),
              IrExprKind::Symbol(name.clone()),
            )),
          ),
        )),
        Box::new(expr),
      ),
    ))
  }

  fn warn(&mut self, report: ReportBuilder<'reports>) {
    self.reports.add(self.processed_file.unwrap(), report);
  }

  fn lower_block(&mut self, block: Vec<HirStmt>, id: AstId) -> IrBlock {
    self.push_scope(ScopeType::Block(id));
    let mut ir_block = vec![];
    for stmt in block {
      let ir_stmt = match stmt {
        HirStmt::Var { symbol_id, init } => {
          let symbol = self.symbol_table.get_symbol_unchecked(&symbol_id);
          let SymbolKind::Variable { is_heap, .. } = symbol.kind else { unreachable!() };

          let var_name = resolve(&symbol.name);
          if !symbol.is_used && !var_name.starts_with('_') {
            debug!("eliminating unused variable: {var_name}");

            self.warn(
              ReportBuilder::builder(
                format!("unused variable: {}", var_name.dimmed().bold()),
                ReportKind::Warning,
              )
              .label("declared here", symbol.source_location.unwrap()),
            );

            continue;
          }

          let name = self.mangle_symbol(symbol_id);
          if is_heap {
            ir_block.push(self.heap_variable(name.clone(), init.ty.clone()));

            let expr = if let HirExprKind::Unary { op, operand } = init.kind.clone() {
              if op == UnaryOp::Box { *operand } else { init }
            } else {
              init
            };
            let ir_expr = self.lower_expr(expr);

            ir_block.push(self.after_heap_assigment(name, ir_expr));
          } else {
            ir_block.push(IrStmt::Var(name, self.lower_expr(init)));
          }

          continue;
        }
        HirStmt::Return(expr) => {
          let expr = expr.map(|expr| self.lower_expr(expr));

          IrStmt::Return(expr)
        }
        HirStmt::Expr(expr) => IrStmt::Expr(self.lower_expr(expr)),
      };

      ir_block.push(ir_stmt);
    }

    let entries = self.pop_scope();
    self.add_drop(entries, &mut ir_block);

    IrBlock::new(ir_block)
  }

  pub(crate) fn new_relation(&mut self, node: SymbolRelationNode) {
    let referrer = if let Some(mono) = self.current_mono_id {
      SymbolRelationNode::Monomorphization(mono)
    } else if let Some(sym_id) = self.current_symbol_id {
      SymbolRelationNode::Symbol(sym_id)
    } else {
      unreachable!()
    };

    self.symbol_table.new_relation(referrer, node);
  }

  fn lower_expr(&mut self, expr: HirExpr) -> IrExpr {
    let kind = self.lower_expr_kind(expr.kind, expr.id);
    let mut expr_type = self.lower_type(expr.ty.clone());
    let fixed_kind = self.fix_struct_init_if_needed(kind, &expr_type, &expr.ty);
    
    if let (IrExprKind::StructInit(_, _), Some(new_type)) = (&fixed_kind.0, fixed_kind.1) {
      expr_type = new_type;
    }
    
    IrExpr { ty: expr_type, kind: fixed_kind.0 }
  }

  fn lower_expr_kind(&mut self, kind: HirExprKind, id: AstId) -> IrExprKind {
    match kind {
      HirExprKind::Block(stmts) => IrExprKind::Block(self.lower_block(stmts, id)),
      HirExprKind::Literal(literal) => IrExprKind::Literal(literal),
      HirExprKind::Call { callee, args, call_info } => self.lower_call_expr(callee, args, call_info),
      HirExprKind::StructInit { name, fields, call_info } => self.lower_struct_init(name, fields, call_info),
      HirExprKind::Assign { lhs, rhs } => self.lower_assign(*lhs, *rhs),
      HirExprKind::Unary { op, operand } => IrExprKind::Unary(op, Box::new(self.lower_expr(*operand))),
      HirExprKind::Symbol(id) => IrExprKind::Symbol(self.mangle_symbol(id)),
      HirExprKind::Binary { left, right, op } => self.lower_binary(*left, *right, op),
      HirExprKind::FieldAccess { base_field, main_access, fields } => self.lower_field_access(base_field, main_access, fields),
      HirExprKind::Ternary { condition, true_expr, false_expr } => self.lower_ternary(*condition, *true_expr, *false_expr),
      HirExprKind::If { condition, then_branch, else_branch } => self.lower_if(*condition, *then_branch, else_branch.map(|e| *e)),
      HirExprKind::Match { arms, scrutinee } => self.lower_match(*scrutinee, arms),
      _ => {
        warn!("unhandled expression: {kind:?}");
        IrExprKind::Symbol(String::new())
      }
    }
  }

  fn lower_call_expr(&mut self, callee: Box<HirExpr>, args: Vec<HirExpr>, call_info: Option<CallInfo>) -> IrExprKind {
    let (identifier, symbol_id) = if let Some(call_info) = call_info {
      self.extract_call_identifier(call_info)
    } else if let HirExprKind::Symbol(id) = callee.kind {
      self.new_relation(SymbolRelationNode::Symbol(id));
      (self.mangle_symbol(id), id)
    } else {
      unreachable!("Invalid call expression structure")
    };
    
    let ir_args = args.iter().map(|arg| self.lower_expr(arg.clone())).collect::<Vec<_>>();
    let final_identifier = self.get_call_identifier(identifier, symbol_id);
    
    IrExprKind::Call(final_identifier, ir_args)
  }

  fn extract_call_identifier(&mut self, call_info: CallInfo) -> (String, SymbolId) {
    if let Some(mono_id) = call_info.monomorphized_id {
      self.new_relation(SymbolRelationNode::Monomorphization(mono_id));
      (self.get_monomorphized_name(mono_id), call_info.original_symbol)
    } else {
      self.new_relation(SymbolRelationNode::Symbol(call_info.original_symbol));
      (self.mangle_symbol(call_info.original_symbol), call_info.original_symbol)
    }
  }

  fn get_call_identifier(&mut self, identifier: String, symbol_id: SymbolId) -> String {
    if matches!(self.symbol_table.get_symbol_unchecked(&symbol_id).kind, SymbolKind::EnumVariant { .. }) {
      format!("{identifier}_constructor")
    } else {
      identifier
    }
  }

  fn lower_struct_init(&mut self, name: Box<HirExpr>, fields: HashMap<zirael_utils::prelude::Identifier, HirExpr>, call_info: Option<CallInfo>) -> IrExprKind {
    let constructor_identifier = if let Some(call_info) = &call_info {
      if let Some(mono_id) = call_info.monomorphized_id {
        self.get_monomorphized_name(mono_id)
      } else {
        self.mangle_symbol(call_info.original_symbol)
      }
    } else if let HirExprKind::Symbol(id) = name.kind {
      self.mangle_symbol(id)
    } else {
      unreachable!("Invalid struct init expression structure")
    };

    let mut ir_fields = HashMap::new();
    for (ident, expr) in fields {
      ir_fields.insert(resolve(&ident), self.lower_expr(expr.clone()));
    }

    IrExprKind::StructInit(constructor_identifier, ir_fields)
  }

  fn lower_assign(&mut self, lhs: HirExpr, rhs: HirExpr) -> IrExprKind {
    IrExprKind::Assign(Box::new(self.lower_expr(lhs)), Box::new(self.lower_expr(rhs)))
  }

  fn lower_binary(&mut self, left: HirExpr, right: HirExpr, op: zirael_parser::BinaryOp) -> IrExprKind {
    IrExprKind::Binary(Box::new(self.lower_expr(left)), op, Box::new(self.lower_expr(right)))
  }

  fn lower_field_access(&mut self, base_field: FieldSymbol, main_access: AccessKind, fields: Vec<(zirael_utils::prelude::Identifier, AccessKind)>) -> IrExprKind {
    let mut f = vec![
      match base_field {
        FieldSymbol::Symbol(sym_id) => IrExpr::new(
          self.lower_type(Type::Inferred),
          IrExprKind::Symbol(self.mangle_symbol(sym_id)),
        ),
        FieldSymbol::Expr(expr) => self.lower_expr(*expr),
      },
      IrExpr::sym(main_access.to_string()),
    ];

    for (i, (field, access)) in fields.iter().enumerate() {
      f.push(IrExpr::sym(resolve(field)));
      if i != fields.len() - 1 {
        f.push(IrExpr::sym(access.to_string()));
      }
    }

    IrExprKind::FieldAccess(f)
  }

  fn lower_ternary(&mut self, condition: HirExpr, true_expr: HirExpr, false_expr: HirExpr) -> IrExprKind {
    IrExprKind::Ternary(
      Box::new(self.lower_expr(condition)),
      Box::new(self.lower_expr(true_expr)),
      Box::new(self.lower_expr(false_expr)),
    )
  }

  fn lower_if(&mut self, condition: HirExpr, then_branch: HirExpr, else_branch: Option<HirExpr>) -> IrExprKind {
    IrExprKind::If {
      condition: Box::new(self.lower_expr(condition)),
      then_branch: Box::new(self.lower_expr(then_branch)),
      else_branch: else_branch.map(|e| Box::new(self.lower_expr(e))),
    }
  }

  fn lower_match(&mut self, scrutinee: HirExpr, arms: Vec<HirMatchArm>) -> IrExprKind {
    let ir_scrutinee = self.lower_expr(scrutinee);
    let scrutinee_type = ir_scrutinee.ty.clone();
    let ir_arms = arms
      .into_iter()
      .map(|arm| self.lower_match_arm(arm, &scrutinee_type))
      .collect::<Vec<_>>();

    IrExprKind::Match { scrutinee: Box::new(ir_scrutinee), arms: ir_arms }
  }

  fn fix_struct_init_if_needed(&mut self, kind: IrExprKind, expr_type: &Type, _original_ty: &Type) -> (IrExprKind, Option<Type>) {
    if let IrExprKind::StructInit(_constructor_name, fields) = &kind {
      if let Type::Named { name: struct_name, generics } = expr_type {
        if !generics.is_empty() && generics.iter().any(|g| matches!(g, Type::Variable { .. })) {
          for (mono_id, entry) in &self.mono_table.entries {
            let original_symbol = self.symbol_table.get_symbol_unchecked(&entry.original_id);
            if matches!(original_symbol.kind, SymbolKind::Struct { .. } | SymbolKind::Enum { .. }) 
               && original_symbol.name == *struct_name {
              let mono_name = self.get_monomorphized_name(*mono_id);
              let new_type = Type::Named {
                name: get_or_intern(&format!("struct {mono_name}"), None),
                generics: vec![],
              };
              return (IrExprKind::StructInit(mono_name, fields.clone()), Some(new_type));
            }
          }
        }
      }
    }
    (kind, None)
  }

  fn lower_match_arm(&mut self, arm: HirMatchArm, scrutinee_type: &Type) -> IrMatchArm {
    IrMatchArm {
      pattern: self.lower_pattern(arm.pattern, scrutinee_type),
      body: self.lower_expr(arm.body),
    }
  }

  fn lower_pattern(&mut self, pattern: HirPattern, scrutinee_type: &Type) -> IrPattern {
    match pattern {
      HirPattern::Wildcard => IrPattern::Wildcard,
      HirPattern::Identifier(symbol_id) => {
        let symbol = self.symbol_table.get_symbol_unchecked(&symbol_id);
        IrPattern::Variable(resolve(&symbol.name))
      }
      HirPattern::Literal(literal) => IrPattern::Literal(literal),
      HirPattern::EnumVariant { symbol_id, fields } => {
        let tag_name = self.get_mono_variant_name(symbol_id, scrutinee_type);

        let bindings = if let Some(fields) = fields {
          fields
            .into_iter()
            .filter_map(|field| {
              if let Some(symbol_id) = field.symbol_id {
                let field_name = self.mangle_symbol(symbol_id);

                Some((self.lower_type(field.ty), resolve(&field.name), field_name))
              } else {
                debug!("invalid field pattern: {field:?}");
                None
              }
            })
            .collect()
        } else {
          vec![]
        };

        IrPattern::EnumVariant { tag_name, bindings }
      }
      HirPattern::Struct { symbol_id: _, fields: _ } => {
        todo!("Struct pattern lowering not implemented")
      }
    }
  }
}
