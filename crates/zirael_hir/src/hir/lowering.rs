use crate::hir::{
  ExprContext, HirBody, HirEnum, HirFunction, HirFunctionSignature, HirItem, HirItemKind,
  HirModule, HirParam, HirStruct, HirTypeExtension, HirVariant,
  expr::{
    AccessKind, FieldSymbol, HirExpr, HirExprKind, HirMatchArm, HirPattern, HirPatternField,
    HirStmt,
  },
};
use id_arena::Arena;
use std::{collections::HashMap, ops::Range};
use zirael_parser::{ast::item::Item, monomorphized_symbol::MonomorphizedSymbol, *};
use zirael_type_checker::monomorphization::MonomorphizationTable;
use zirael_utils::prelude::*;

pub struct AstLowering<'reports> {
  pub symbol_table: SymbolTable,
  pub mono_table: MonomorphizationTable,
  reports: Reports<'reports>,
  processed_file: Option<SourceFileId>,
  pub folded_vars: HashMap<SymbolId, HirExprKind>,
  ast_id_arena: Arena<()>,
}

impl<'reports> AstLowering<'reports> {
  pub fn new(
    symbol_table: &SymbolTable,
    reports: &Reports<'reports>,
    mono_table: MonomorphizationTable,
  ) -> Self {
    Self {
      symbol_table: symbol_table.clone(),
      reports: reports.clone(),
      processed_file: None,
      folded_vars: HashMap::new(),
      ast_id_arena: Arena::new(),
      mono_table,
    }
  }

  pub fn lower_modules(&mut self, lexed_modules: &mut Vec<LexedModule>) -> Vec<HirModule> {
    lexed_modules.iter_mut().map(|module| self.lower_module(module)).collect()
  }

  fn push_scope(&mut self, scope_type: ScopeType) {
    let _ = self.symbol_table.push_scope(scope_type);
  }

  fn pop_scope(&mut self) {
    if let Err(err) = self.symbol_table.pop_scope() {
      self.error(&format!("Failed to pop scope: {err:?}"), vec![], vec![]);
    }
  }

  fn lower_module(&mut self, lexed_module: &mut LexedModule) -> HirModule {
    let mut hir_module = HirModule { items: HashMap::new(), id: lexed_module.file().unwrap() };
    let id = lexed_module.id.as_file().unwrap();
    self.processed_file = Some(id);

    self.push_scope(ScopeType::Module(id));
    for item in &mut lexed_module.ast.items {
      if let Some(hir_item) = self.lower_item(item) {
        hir_module.items.insert(hir_item.symbol_id, hir_item);
      }
    }
    self.pop_scope();

    hir_module
  }

  fn lower_item(&mut self, item: &mut Item) -> Option<HirItem> {
    if let ItemKind::Import(..) = &item.kind {
      return None;
    }

    let symbol_id = item.symbol_id.unwrap();
    let sym = self.symbol_table.get_symbol_unchecked(&symbol_id);
    if self.try_unused_symbol(&sym) {
      return None;
    }

    match &mut item.kind {
      ItemKind::Function(func) => {
        self.push_scope(ScopeType::Function(func.id));

        let hir_function = self.lower_function(func, symbol_id);
        self.pop_scope();
        Some(HirItem {
          symbol_id,
          kind: HirItemKind::Function(hir_function),
          span: item.span.clone(),
        })
      }
      ItemKind::Struct(struct_def) => {
        self.push_scope(ScopeType::Struct(struct_def.id));
        let hir_struct = self.lower_struct(struct_def, symbol_id);
        self.pop_scope();
        Some(HirItem { symbol_id, kind: HirItemKind::Struct(hir_struct), span: item.span.clone() })
      }
      ItemKind::TypeExtension(ty_ext) => {
        self.push_scope(ScopeType::TypeExtension(ty_ext.id));
        let ty_ext = self.lower_ty_ext(ty_ext, symbol_id);
        self.pop_scope();

        Some(HirItem {
          symbol_id,
          kind: HirItemKind::TypeExtension(ty_ext),
          span: item.span.clone(),
        })
      }
      ItemKind::Enum(enum_def) => {
        self.push_scope(ScopeType::Enum(enum_def.id));
        let hir_enum = self.lower_enum(enum_def, symbol_id);
        self.pop_scope();

        Some(HirItem { symbol_id, kind: HirItemKind::Enum(hir_enum), span: item.span.clone() })
      }
      ItemKind::Import(..) => None,
    }
  }

  fn lower_ty_ext(
    &mut self,
    struct_def: &mut TypeExtension,
    symbol_id: SymbolId,
  ) -> HirTypeExtension {
    HirTypeExtension {
      id: struct_def.id,
      symbol_id,
      methods: self.lower_methods(&mut struct_def.items),
    }
  }

  fn lower_methods(&mut self, methods: &mut Vec<Item>) -> Vec<HirItem> {
    methods.iter_mut().filter_map(|i| self.lower_item(i)).collect()
  }

  fn lower_enum(&mut self, enum_def: &mut EnumDeclaration, symbol_id: SymbolId) -> HirEnum {
    HirEnum {
      id: enum_def.id,
      symbol_id,
      methods: self.lower_methods(&mut enum_def.methods),
      variants: enum_def
        .variants
        .iter_mut()
        .map(|v| HirVariant { symbol_id: v.symbol_id.unwrap(), data: v.data.clone() })
        .collect::<Vec<_>>(),
    }
  }

  fn lower_struct(&mut self, struct_def: &mut StructDeclaration, symbol_id: SymbolId) -> HirStruct {
    HirStruct {
      id: struct_def.id,
      symbol_id,
      fields: struct_def.fields.clone(),
      methods: self.lower_methods(&mut struct_def.methods),
    }
  }

  fn lower_function(&mut self, func: &mut Function, symbol_id: SymbolId) -> HirFunction {
    let parameters = func
      .signature
      .parameters
      .iter_mut()
      .filter_map(|param| self.lower_parameter(param))
      .collect();

    let hir_signature =
      HirFunctionSignature { parameters, return_type: func.signature.return_type.clone() };

    let body = func.body.as_mut().map(|body_expr| self.lower_function_body(body_expr, symbol_id));

    HirFunction {
      id: func.id,
      symbol_id,
      signature: hir_signature,
      body,
      is_async: func.modifiers.is_async,
      is_const: func.modifiers.is_const,
      is_extern: func.modifiers.is_extern,
      abi: func.modifiers.abi.as_ref().map(|a| a.0.clone()),
    }
  }

  fn lower_parameter(&mut self, param: &mut Parameter) -> Option<HirParam> {
    let symbol_id = param.symbol_id.unwrap();
    let default_value = param.default_value.as_mut().map(|expr| self.lower_expr(expr));

    Some(HirParam {
      symbol_id,
      ty: param.ty.clone(),
      is_variadic: matches!(param.kind, ParameterKind::Variadic),
      default_value,
    })
  }

  fn lower_function_body(&mut self, body_expr: &mut Expr, _function_symbol: SymbolId) -> HirBody {
    let root_expr = self.lower_expr(body_expr);

    HirBody { root_expr }
  }

  fn lower_expr(&mut self, ast_expr: &mut Expr) -> HirExpr {
    self.lower_expr_impl(ast_expr, ExprContext::Expr)
  }

  fn lower_expr_stmt(&mut self, ast_expr: &mut Expr) -> HirExpr {
    self.lower_expr_impl(ast_expr, ExprContext::Stmt)
  }

  fn lower_expr_no_constant_fold(&mut self, ast_expr: &mut Expr) -> HirExpr {
    let hir_kind = match &mut ast_expr.kind {
      ExprKind::Identifier(_name, Some(symbol_id)) => HirExprKind::Symbol(*symbol_id),

      ExprKind::Identifier(name, None) => {
        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
          HirExprKind::Symbol(symbol.id)
        } else {
          self.error("Unresolved identifier", vec![], vec![]);
          HirExprKind::Error
        }
      }

      ExprKind::FieldAccess(fields) => {
        let base = &mut fields[0].clone();
        let sym_id = if let Some((_, symbol_id)) = self.symbol_table.symbol_from_expr(base) {
          FieldSymbol::Symbol(symbol_id)
        } else {
          FieldSymbol::Expr(Box::new(self.lower_expr_no_constant_fold(base)))
        };
        fields.remove(0);

        let mut indents = vec![];
        for field in fields {
          let (ident, _) = field.as_identifier().unwrap();
          indents.push((
            *ident,
            if field.ty.is_reference() { AccessKind::Pointer } else { AccessKind::Value },
          ));
        }
        let main_access =
          if base.ty.is_reference() { AccessKind::Pointer } else { AccessKind::Value };

        HirExprKind::FieldAccess { base_field: sym_id, main_access, fields: indents }
      }

      ExprKind::IndexAccess(object, index) => {
        let object_expr = Box::new(self.lower_expr_no_constant_fold(object));
        let index_expr = Box::new(self.lower_expr(index));
        HirExprKind::IndexAccess { object: object_expr, index: index_expr }
      }

      _ => {
        return self.lower_expr_impl_no_fold(ast_expr, ExprContext::Expr);
      }
    };

    HirExpr {
      kind: hir_kind,
      ty: ast_expr.ty.clone(),
      span: ast_expr.span.clone(),
      id: ast_expr.id,
    }
  }

  fn lower_expr_impl_no_fold(&mut self, ast_expr: &mut Expr, context: ExprContext) -> HirExpr {
    let hir_kind = match &mut ast_expr.kind {
      ExprKind::Identifier(_name, Some(symbol_id)) => HirExprKind::Symbol(*symbol_id),

      ExprKind::Identifier(name, None) => {
        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
          HirExprKind::Symbol(symbol.id)
        } else {
          self.error("Unresolved identifier", vec![], vec![]);
          HirExprKind::Error
        }
      }

      ExprKind::Literal(literal) => HirExprKind::Literal(literal.clone()),

      ExprKind::Binary { left, op, right } => {
        let left_expr = Box::new(self.lower_expr(left));
        let right_expr = Box::new(self.lower_expr(right));
        HirExprKind::Binary { left: left_expr, op: op.clone(), right: right_expr }
      }

      ExprKind::Unary(op, operand) => {
        let operand_expr = Box::new(self.lower_expr(operand));
        HirExprKind::Unary { op: *op.clone(), operand: operand_expr }
      }

      ExprKind::Assign(lhs, rhs) => {
        let lhs_expr = Box::new(self.lower_expr_no_constant_fold(lhs));
        let rhs_expr = Box::new(self.lower_expr(rhs));
        HirExprKind::Assign { lhs: lhs_expr, rhs: rhs_expr }
      }

      ExprKind::AssignOp(lhs, op, rhs) => {
        let lhs_expr = Box::new(self.lower_expr_no_constant_fold(lhs));
        let rhs_expr = Box::new(self.lower_expr(rhs));
        HirExprKind::Assign {
          lhs: lhs_expr.clone(),
          rhs: Box::new(HirExpr {
            kind: HirExprKind::Binary { left: lhs_expr, op: op.clone(), right: rhs_expr },
            ty: ast_expr.ty.clone(),
            span: ast_expr.span.clone(),
            id: ast_expr.id,
          }),
        }
      }

      _ => {
        return HirExpr {
          kind: self.lower_expr_impl(ast_expr, context).kind,
          ty: ast_expr.ty.clone(),
          span: ast_expr.span.clone(),
          id: ast_expr.id,
        };
      }
    };

    HirExpr {
      kind: hir_kind,
      ty: ast_expr.ty.clone(),
      span: ast_expr.span.clone(),
      id: ast_expr.id,
    }
  }

  fn lower_call_args(&mut self, args: &mut Vec<Expr>) -> Vec<HirExpr> {
    args.iter_mut().map(|arg| self.lower_expr(arg)).collect()
  }

  fn lower_expr_impl(&mut self, ast_expr: &mut Expr, context: ExprContext) -> HirExpr {
    let hir_kind = match &mut ast_expr.kind {
      ExprKind::Literal(lit) => HirExprKind::Literal(lit.clone()),

      ExprKind::Identifier(_name, Some(symbol_id)) => HirExprKind::Symbol(*symbol_id),

      ExprKind::Identifier(name, None) => {
        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
          HirExprKind::Symbol(symbol.id)
        } else {
          self.error("Unresolved identifier", vec![], vec![]);
          HirExprKind::Error
        }
      }

      ExprKind::Binary { left, op, right } => {
        let left_expr = Box::new(self.lower_expr(left));
        let right_expr = Box::new(self.lower_expr(right));
        HirExprKind::Binary { left: left_expr, op: op.clone(), right: right_expr }
      }

      ExprKind::Ternary { condition, true_expr, false_expr } => {
        let condition_expr = Box::new(self.lower_expr(condition));
        let true_expr_hir = Box::new(self.lower_expr(true_expr));
        let false_expr_hir = Box::new(self.lower_expr(false_expr));
        HirExprKind::Ternary {
          condition: condition_expr,
          true_expr: true_expr_hir,
          false_expr: false_expr_hir,
        }
      }

      ExprKind::Unary(op, operand) => {
        let operand_expr = Box::new(self.lower_expr(operand));
        HirExprKind::Unary { op: *op.clone(), operand: operand_expr }
      }

      ExprKind::Assign(lhs, rhs) => {
        let lhs_expr = Box::new(self.lower_expr_no_constant_fold(lhs));
        let rhs_expr = Box::new(self.lower_expr(rhs));
        HirExprKind::Assign { lhs: lhs_expr, rhs: rhs_expr }
      }

      ExprKind::AssignOp(lhs, op, rhs) => {
        let lhs_expr = Box::new(self.lower_expr_no_constant_fold(lhs));
        let rhs_expr = Box::new(self.lower_expr(rhs));
        HirExprKind::Assign {
          lhs: lhs_expr.clone(),
          rhs: Box::new(HirExpr {
            kind: HirExprKind::Binary { left: lhs_expr, op: op.clone(), right: rhs_expr },
            ty: ast_expr.ty.clone(),
            span: ast_expr.span.clone(),
            id: ast_expr.id,
          }),
        }
      }

      ExprKind::Call { callee, args, call_info, .. } => {
        let callee_expr = Box::new(self.lower_expr(callee));
        HirExprKind::Call {
          callee: callee_expr,
          args: self.lower_call_args(args),
          call_info: call_info.clone(),
        }
      }

      ExprKind::StaticCall { callee, args, call_info } => {
        let ExprKind::FieldAccess(fields) = &mut callee.kind else { unreachable!() };

        let expr = self.lower_expr(&mut fields[1]);
        HirExprKind::Call {
          callee: Box::new(expr),
          args: self.lower_call_args(args),
          call_info: call_info.clone(),
        }
      }

      ExprKind::MethodCall { chain, args, call_info } => {
        if chain.len() < 2 {
          self.error("Invalid method call chain", vec![], vec![]);
          return HirExpr {
            kind: HirExprKind::Error,
            ty: ast_expr.ty.clone(),
            span: ast_expr.span.clone(),
            id: ast_expr.id,
          };
        }

        let receiver = self.lower_expr(&mut chain[0]);
        let method_expr = self.lower_expr(chain.last_mut().unwrap());
        let method =
          self.symbol_table.get_symbol_unchecked(
            &if let HirExprKind::Symbol(id) = method_expr.kind { id } else { unreachable!() },
          );
        let SymbolKind::Function { signature, .. } = method.kind else { unreachable!() };

        let mut method_args = vec![if signature.parameters.first().unwrap().ty.is_reference() {
          HirExpr {
            kind: HirExprKind::Unary { op: UnaryOp::Ref, operand: Box::new(receiver) },
            ty: Type::Inferred,
            span: Default::default(),
            id: self.ast_id_arena.alloc(()),
          }
        } else {
          receiver
        }];
        method_args.extend(self.lower_call_args(args));

        HirExprKind::Call {
          callee: Box::new(method_expr),
          args: method_args,
          call_info: call_info.clone(),
        }
      }

      ExprKind::StructInit { name, fields, call_info } => {
        let name_expr = Box::new(self.lower_expr(name));

        let mut fields_map = HashMap::new();
        for (ident, val) in fields {
          fields_map.insert(*ident, self.lower_expr(val));
        }

        HirExprKind::StructInit {
          name: name_expr,
          fields: fields_map,
          call_info: call_info.clone(),
        }
      }

      ExprKind::FieldAccess(fields) => {
        let base = &mut fields[0].clone();
        let sym_id = if let Some((_, symbol_id)) = self.symbol_table.symbol_from_expr(base) {
          FieldSymbol::Symbol(symbol_id)
        } else {
          FieldSymbol::Expr(Box::new(self.lower_expr(base)))
        };
        fields.remove(0);

        let mut indents = vec![];
        for field in fields {
          let (ident, _) = field.as_identifier().unwrap();
          indents.push((
            *ident,
            if field.ty.is_reference() { AccessKind::Pointer } else { AccessKind::Value },
          ));
        }
        let main_access =
          if base.ty.is_reference() { AccessKind::Pointer } else { AccessKind::Value };

        HirExprKind::FieldAccess { base_field: sym_id, main_access, fields: indents }
      }

      ExprKind::IndexAccess(object, index) => {
        let object_expr = Box::new(self.lower_expr(object));
        let index_expr = Box::new(self.lower_expr(index));
        HirExprKind::IndexAccess { object: object_expr, index: index_expr }
      }

      ExprKind::Block(stmts) => {
        self.push_scope(ScopeType::Block(ast_expr.id));
        let mut hir_stmts = vec![];

        for stmt in stmts.iter_mut() {
          match &mut stmt.0 {
            StmtKind::Expr(expr) => {
              let lowered_expr = self.lower_expr_stmt(expr);

              if self.is_expr_pointless(&lowered_expr.kind) {
                self.result_not_used_error(&lowered_expr);
                continue;
              }

              hir_stmts.push(HirStmt::Expr(lowered_expr));
            }
            StmtKind::Return(ret) => {
              hir_stmts
                .push(HirStmt::Return(ret.value.clone().map(|mut e| self.lower_expr(&mut e))));
              break;
            }
            StmtKind::Var(var_stmt) => {
              let symbol_id = var_stmt.symbol_id.unwrap();
              let expr = self.lower_expr(&mut var_stmt.value);
              if self.can_be_folded(&expr.kind) {
                let folded = self.try_to_constant_fold(expr.kind.clone());
                self.folded_vars.insert(symbol_id, folded);
              }

              hir_stmts.push(HirStmt::Var { symbol_id, init: expr });
            }
            StmtKind::If(if_stmt) => {
              let hir_if = self.lower_if_stmt(if_stmt);
              hir_stmts.push(HirStmt::Expr(hir_if));
            }
          }
        }

        self.pop_scope();
        HirExprKind::Block(hir_stmts)
      }
      ExprKind::Paren(inner) => {
        return self.lower_expr(inner);
      }

      ExprKind::Match { scrutinee, arms } => {
        let scrutinee_expr = Box::new(self.lower_expr(scrutinee));
        let hir_arms = arms.iter_mut().map(|arm| self.lower_match_arm(arm)).collect();
        HirExprKind::Match { scrutinee: scrutinee_expr, arms: hir_arms }
      }

      ExprKind::CouldntParse(_) => {
        self.error("Could not parse expression", vec![], vec![]);
        HirExprKind::Error
      }
    };

    HirExpr {
      kind: if context == ExprContext::Stmt && self.is_expr_pointless(&hir_kind) {
        hir_kind
      } else {
        self.try_to_constant_fold(hir_kind)
      },
      ty: ast_expr.ty.clone(),
      span: ast_expr.span.clone(),
      id: ast_expr.id,
    }
  }

  pub fn error(&mut self, message: &str, labels: Vec<(String, Range<usize>)>, notes: Vec<String>) {
    if let Some(file_id) = self.processed_file {
      let mut report = ReportBuilder::builder(message, ReportKind::Error);
      for note in notes {
        report = report.note(&note);
      }
      for (msg, span) in labels {
        report = report.label(&msg, span);
      }
      self.reports.add(file_id, report);
    } else {
      warn!("Report outside of a file: {message}");
    }
  }

  pub fn warn(&mut self, message: &str, labels: Vec<(String, Range<usize>)>, notes: Vec<String>) {
    if let Some(file_id) = self.processed_file {
      let mut report = ReportBuilder::builder(message, ReportKind::Warning);
      for note in notes {
        report = report.note(&note);
      }
      for (msg, span) in labels {
        report = report.label(&msg, span);
      }
      self.reports.add(file_id, report);
    }
  }

  fn lower_match_arm(&mut self, arm: &mut MatchArm) -> HirMatchArm {
    let pattern = self.lower_pattern(&mut arm.pattern);
    let body = self.lower_expr(&mut arm.body);
    HirMatchArm { pattern, body, span: arm.span.clone() }
  }

  fn lower_pattern(&mut self, pattern: &mut Pattern) -> HirPattern {
    match pattern {
      Pattern::Wildcard => HirPattern::Wildcard,
      Pattern::Identifier(name) => {
        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
          HirPattern::Identifier(symbol.id)
        } else {
          self.error("Unresolved pattern identifier", vec![], vec![]);
          HirPattern::Wildcard
        }
      }
      Pattern::Literal(lit) => HirPattern::Literal(lit.clone()),
      Pattern::EnumVariant { path, fields, resolved_variant } => {
        let hir_fields = if let Some(fields) = fields {
          Some(fields.iter_mut().map(|field| self.lower_pattern_field(field)).collect())
        } else {
          None
        };

        HirPattern::EnumVariant { symbol_id: resolved_variant.unwrap(), fields: hir_fields }
      }
      Pattern::Struct { name, fields } => {
        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
          let hir_fields = fields.iter_mut().map(|field| self.lower_pattern_field(field)).collect();
          HirPattern::Struct { symbol_id: symbol.id, fields: hir_fields }
        } else {
          self.error("Unresolved struct in pattern", vec![], vec![]);
          HirPattern::Wildcard
        }
      }
    }
  }

  fn lower_pattern_field(&mut self, field: &mut PatternField) -> HirPatternField {
    let symbol_id = self.symbol_table.lookup_symbol(&field.name).map(|s| s.id);
    let pattern = if let Some(ref mut pat) = field.pattern {
      Some(Box::new(self.lower_pattern(pat)))
    } else {
      None
    };

    HirPatternField {
      name: field.name,
      symbol_id,
      pattern,
      span: field.span.clone(),
      ty: field.ty.clone().unwrap(),
    }
  }

  fn lower_if_stmt(&mut self, if_stmt: &mut If) -> HirExpr {
    let condition = Box::new(self.lower_expr(&mut if_stmt.condition));

    self.push_scope(ScopeType::Block(if_stmt.then_branch_id));
    let then_branch_stmts = if_stmt
      .then_branch
      .iter_mut()
      .map(|stmt| match &mut stmt.0 {
        StmtKind::Expr(expr) => {
          let lowered_expr = self.lower_expr_stmt(expr);
          HirStmt::Expr(lowered_expr)
        }
        StmtKind::Return(ret) => {
          HirStmt::Return(ret.value.clone().map(|mut e| self.lower_expr(&mut e)))
        }
        StmtKind::Var(var_stmt) => {
          let symbol_id = var_stmt.symbol_id.unwrap();
          let expr = self.lower_expr(&mut var_stmt.value);
          if self.can_be_folded(&expr.kind) {
            let folded = self.try_to_constant_fold(expr.kind.clone());
            self.folded_vars.insert(symbol_id, folded);
          }
          HirStmt::Var { symbol_id, init: expr }
        }
        StmtKind::If(nested_if) => {
          let nested_hir_if = self.lower_if_stmt(nested_if);
          HirStmt::Expr(nested_hir_if)
        }
      })
      .collect::<Vec<_>>();
    self.pop_scope();

    let then_branch = Box::new(HirExpr {
      kind: HirExprKind::Block(then_branch_stmts),
      ty: Type::Void,
      span: if_stmt.span.clone(),
      id: if_stmt.then_branch_id,
    });

    let else_branch = if let Some(ref mut else_branch) = if_stmt.else_branch {
      match else_branch {
        ElseBranch::Block(stmts, block_id) => {
          self.push_scope(ScopeType::Block(*block_id));
          let else_stmts = stmts
            .iter_mut()
            .map(|stmt| match &mut stmt.0 {
              StmtKind::Expr(expr) => {
                let lowered_expr = self.lower_expr_stmt(expr);
                HirStmt::Expr(lowered_expr)
              }
              StmtKind::Return(ret) => {
                HirStmt::Return(ret.value.clone().map(|mut e| self.lower_expr(&mut e)))
              }
              StmtKind::Var(var_stmt) => {
                let symbol_id = var_stmt.symbol_id.unwrap();
                let expr = self.lower_expr(&mut var_stmt.value);
                if self.can_be_folded(&expr.kind) {
                  let folded = self.try_to_constant_fold(expr.kind.clone());
                  self.folded_vars.insert(symbol_id, folded);
                }
                HirStmt::Var { symbol_id, init: expr }
              }
              StmtKind::If(nested_if) => {
                let nested_hir_if = self.lower_if_stmt(nested_if);
                HirStmt::Expr(nested_hir_if)
              }
            })
            .collect::<Vec<_>>();
          self.pop_scope();

          Some(Box::new(HirExpr {
            kind: HirExprKind::Block(else_stmts),
            ty: Type::Void,
            span: if_stmt.span.clone(),
            id: *block_id,
          }))
        }
        ElseBranch::If(nested_if) => Some(Box::new(self.lower_if_stmt(nested_if))),
      }
    } else {
      None
    };

    HirExpr {
      kind: HirExprKind::If { condition, then_branch, else_branch },
      ty: Type::Void,
      span: if_stmt.span.clone(),
      id: if_stmt.then_branch_id,
    }
  }
}

pub fn lower_ast_to_hir<'reports>(
  lexed_modules: &mut Vec<LexedModule>,
  symbol_table: &SymbolTable,
  reports: &Reports<'reports>,
  mono: MonomorphizationTable,
) -> Vec<HirModule> {
  let mut lowering = AstLowering::new(symbol_table, reports, mono);
  lowering.lower_modules(lexed_modules)
}
