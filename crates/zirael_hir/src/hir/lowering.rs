use crate::hir::{
  ExprContext, HirBody, HirEnum, HirFunction, HirFunctionSignature, HirItem, HirItemKind,
  HirModule, HirParam, HirStruct, HirTypeExtension, HirVariant,
  expr::{
    AccessKind, FieldSymbol, HirExpr, HirExprKind, HirMatchArm, HirPattern, HirPatternField,
    HirStmt,
  },
};
use id_arena::Arena;
use std::collections::HashMap;
use zirael_parser::ty::{Ty, TyId};
use zirael_parser::{ast::item::Item, *};
use zirael_type_checker::{
  GenericEnumData, GenericSymbol, GenericSymbolKind, MonoSymbolTable, MonomorphizedSymbolKind,
};
use zirael_utils::prelude::*;

pub struct AstLowering<'reports, 'table> {
  pub symbol_table: &'table mut MonoSymbolTable,
  pub sym_table: &'table SymbolTable,
  reports: Reports<'reports>,
  processed_file: Option<SourceFileId>,
  pub folded_vars: HashMap<SymbolId, HirExprKind>,
  ast_id_arena: Arena<()>,
  pub is_library: bool,
  pub mode: Mode,
  pub current_items: Vec<HirItem>,
  pub processed_item: Option<SymbolId>,
}

impl<'reports, 'table> AstLowering<'reports, 'table> {
  pub fn new(
    symbol_table: &'table mut MonoSymbolTable,
    sym_table: &'table SymbolTable,
    reports: &Reports<'reports>,
    is_library: bool,
    mode: Mode,
  ) -> Self {
    Self {
      symbol_table,
      sym_table,
      reports: reports.clone(),
      processed_file: None,
      folded_vars: HashMap::new(),
      ast_id_arena: Arena::new(),
      is_library,
      mode,
      current_items: Vec::new(),
      processed_item: None,
    }
  }

  fn is_enum_variant_constructor(&self, name_expr: &Expr) -> bool {
    let symbol_id = match &name_expr.kind {
      ExprKind::Identifier(_, Some(sym_id)) => *sym_id,
      ExprKind::Path(path) => {
        if let Some(last_segment) = path.segments.last() {
          if let Some(sym_id) = last_segment.symbol_id {
            sym_id
          } else {
            return false;
          }
        } else {
          return false;
        }
      }
      _ => return false,
    };

    let symbol = self.symbol_table.get_generic_symbol(symbol_id);
    let Some(symbol) = symbol else {
      return false;
    };

    matches!(symbol.kind, GenericSymbolKind::EnumVariant { .. })
  }

  fn get_variant_field_order(&self, name_expr: &Expr) -> Option<Vec<Identifier>> {
    let symbol_id = match &name_expr.kind {
      ExprKind::Identifier(_, Some(sym_id)) => *sym_id,
      ExprKind::Path(path) => {
        if let Some(last_segment) = path.segments.last() {
          last_segment.symbol_id?
        } else {
          return None;
        }
      }
      _ => return None,
    };

    let symbol = self.symbol_table.get_generic_symbol(symbol_id);
    let Some(symbol) = symbol else {
      return None;
    };

    if let GenericSymbolKind::EnumVariant { data, .. } = &symbol.kind {
      if let GenericEnumData::Struct(fields) = data {
        return Some(fields.iter().map(|f| f.name).collect());
      }
    }
    None
  }

  fn lower_identifier(&mut self, symbol_id: SymbolId) -> HirExprKind {
    let symbol = self.symbol_table.get_generic_symbol(symbol_id);

    let Some(symbol) = symbol else {
      self.error(&format!("sym not found in generic symbol table {:?}", symbol_id), vec![], vec![]);
      return HirExprKind::Error;
    };
    // TODO: implement mode checking
    // self.symbol_for_mode_check(&symbol, *symbol.name.span());

    if let GenericSymbolKind::EnumVariant { data, .. } = &symbol.kind {
      match data {
        GenericEnumData::Unit => HirExprKind::Call {
          callee: Box::new(HirExpr {
            kind: HirExprKind::Symbol(symbol_id),
            ty: self.symbol_table.inferred(),
            span: Span { start: 0, end: 0 },
            id: self.ast_id_arena.alloc(()),
          }),
          args: vec![],
          call_info: None,
        },
        GenericEnumData::Struct(_) => HirExprKind::Symbol(symbol_id),
      }
    } else {
      HirExprKind::Symbol(symbol_id)
    }
  }

  pub fn symbol_for_mode_check(&mut self, symbol: &Symbol, span: Span) {
    if symbol.mode_specific.is_some_and(|mode| mode == self.mode) {
      self.error(
        &format!(
          "{} only works in {} mode, bur currently it's {} mode",
          symbol.name.to_string().dimmed().bold(),
          symbol.mode_specific.unwrap().to_string().dimmed().bold(),
          self.mode.to_string().dimmed().bold()
        ),
        vec![
          ("used here".to_string(), span),
          (
            format!("{} is defined here", symbol.name.to_string().dimmed().bold()),
            symbol.source_location.unwrap_or_default(),
          ),
        ],
        vec![],
      );
    }
  }

  fn lower_type(&mut self, ty: Type) -> TyId {
    fn normalize(sym_table: &SymbolTable, ty: Type) -> Type {
      match ty {
        Type::Pointer(inner) => Type::Pointer(Box::new(normalize(sym_table, *inner))),
        Type::Reference(inner) => Type::Reference(Box::new(normalize(sym_table, *inner))),
        Type::Array(inner, size) => Type::Array(Box::new(normalize(sym_table, *inner)), size),
        Type::Function { params, return_type } => {
          let params = params.into_iter().map(|p| normalize(sym_table, p)).collect();
          let return_type = Box::new(normalize(sym_table, *return_type));
          Type::Function { params, return_type }
        }
        Type::Named { name, generics } => {
          let generics = generics.into_iter().map(|g| normalize(sym_table, g)).collect::<Vec<_>>();
          if let Some(symbol) = sym_table.lookup_symbol(&name) {
            Type::Symbol(symbol.id)
          } else {
            Type::Named { name, generics }
          }
        }
        other => other,
      }
    }

    let ty = normalize(self.sym_table, ty);
    self.symbol_table.intern_type(ty)
  }

  pub fn lower_modules(&mut self, lexed_modules: &mut Vec<LexedModule>) -> Vec<HirModule> {
    lexed_modules.iter_mut().map(|module| self.lower_module(module)).collect()
  }

  fn lower_module(&mut self, lexed_module: &mut LexedModule) -> HirModule {
    let mut hir_module = HirModule { items: Vec::new(), id: lexed_module.file().unwrap() };
    let id = lexed_module.id.as_file().unwrap();
    self.processed_file = Some(id);

    self.current_items = vec![];
    for item in &mut lexed_module.ast.items {
      self.lower_item(item);
    }

    hir_module.items.extend(self.current_items.drain(..));

    hir_module
  }

  fn lower_item(&mut self, item: &mut Item) -> Option<HirItem> {
    if let ItemKind::Import(..) = &item.kind {
      return None;
    }

    let symbol_id = item.symbol_id.unwrap();
    let sym = &self.symbol_table.get_generic_symbol(symbol_id).cloned();
    let Some(sym) = sym else {
      self.error(
        &format!("Symbol not found in generic symbol table {:?}", symbol_id),
        vec![],
        vec![],
      );
      return None;
    };
    self.processed_item = Some(symbol_id);

    if self.try_unused_symbol(sym) {
      return None;
    }

    if let None = self.handle_monomorphized_variants(symbol_id, &sym, item) {
      return None;
    }

    // TODO: implement
    // if sym.mode_specific.is_some_and(|mode| mode == self.mode) {
    //   return None;
    // }

    let kind = match &mut item.kind {
      ItemKind::Function(func) => {
        let hir_function = self.lower_function(func, symbol_id);
        Some(HirItemKind::Function(hir_function))
      }
      ItemKind::Struct(struct_def) => {
        let hir_struct = self.lower_struct(struct_def, symbol_id);
        Some(HirItemKind::Struct(hir_struct))
      }
      ItemKind::TypeExtension(ty_ext) => {
        let ty_ext = self.lower_ty_ext(ty_ext, symbol_id);

        Some(HirItemKind::TypeExtension(ty_ext))
      }
      ItemKind::Enum(enum_def) => {
        let hir_enum = self.lower_enum(enum_def, symbol_id);

        Some(HirItemKind::Enum(hir_enum))
      }
      ItemKind::Import(..) => None,
    };

    if let Some(kind) = &kind {
      let item = HirItem {
        symbol_id,
        kind: kind.clone(),
        span: item.span,
        attrs: item.attributes.clone(),
        original_item_id: OriginalSymbolId::Symbol(symbol_id),
      };
      self.current_items.push(item.clone());
      Some(item)
    } else {
      None
    }
  }

  fn handle_monomorphized_variants(
    &mut self,
    symbol_id: SymbolId,
    sym: &GenericSymbol,
    item: &mut Item,
  ) -> Option<()> {
    if let Some(variants) = self.symbol_table.get_mono_variants(symbol_id) {
      debug!("Found {:?} mono variants for symbol {}", variants, sym.base.name);

      if let ItemKind::Enum(_enum) = &item.kind {
        return Some(());
      }

      for id in variants {
        let symbol = self.symbol_table.get_monomorphized_symbol(id).cloned();

        if let Some(symbol) = symbol {
          match (&mut item.kind.clone(), &symbol.kind) {
            (ItemKind::Function(func), MonomorphizedSymbolKind::Function { signature, .. }) => {
              func.signature = signature.clone();

              // TODO: might need to substitute types in function's body
              let hir_function = self.lower_function(func, symbol_id);
              let hir_item = HirItem {
                symbol_id,
                kind: HirItemKind::Function(hir_function),
                span: item.span,
                attrs: item.attributes.clone(),
                original_item_id: OriginalSymbolId::Monomorphization(symbol.mono_id()?),
              };

              self.current_items.push(hir_item);
            }
            _ => warn!("not implemented: \n{:?} \n->\n {:?}\n", item, symbol),
          }
        } else {
          self.error(
            &format!("Couldn't find a monomorphized version of a symbol {:?}", symbol_id),
            vec![],
            vec![],
          );
        }
      }

      return None;
    }

    Some(())
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

    let hir_signature = HirFunctionSignature {
      parameters,
      return_type: self.lower_type(func.signature.return_type.clone()),
    };

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
      ty: self.lower_type(param.ty.clone()),
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
        self.error(&format!("Unresolved identifier: {}", name), vec![], vec![]);
        HirExprKind::Error
      }
      ExprKind::FieldAccess(fields) => {
        let base = &mut fields[0].clone();
        let sym_id = FieldSymbol::Expr(Box::new(self.lower_expr_no_constant_fold(base)));
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
      ty: self.lower_type(ast_expr.ty.clone()),
      span: ast_expr.span,
      id: ast_expr.id,
    }
  }

  fn lower_expr_impl_no_fold(&mut self, ast_expr: &mut Expr, context: ExprContext) -> HirExpr {
    let hir_kind = match &mut ast_expr.kind {
      ExprKind::Identifier(_name, Some(symbol_id)) => HirExprKind::Symbol(*symbol_id),

      ExprKind::Identifier(name, None) => {
        self.error(&format!("Unresolved identifier: {}", name), vec![], vec![]);
        HirExprKind::Error
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
            ty: self.lower_type(ast_expr.ty.clone()),
            span: ast_expr.span,
            id: ast_expr.id,
          }),
        }
      }

      _ => {
        return HirExpr {
          kind: self.lower_expr_impl(ast_expr, context).kind,
          ty: self.lower_type(ast_expr.ty.clone()),
          span: ast_expr.span,
          id: ast_expr.id,
        };
      }
    };

    HirExpr {
      kind: hir_kind,
      ty: self.lower_type(ast_expr.ty.clone()),
      span: ast_expr.span,
      id: ast_expr.id,
    }
  }

  fn lower_call_args(&mut self, args: &mut Vec<Expr>) -> Vec<HirExpr> {
    args.iter_mut().map(|arg| self.lower_expr(arg)).collect()
  }

  fn handle_call_info(&mut self, call_info: &mut Option<CallInfo>) {
    if let Some(info) = call_info
      && let Some(mono_id) = info.monomorphized_id
      && let Some(sym_id) = self.processed_item
    {
      self
        .sym_table
        .new_relation(OriginalSymbolId::Symbol(sym_id), OriginalSymbolId::Monomorphization(mono_id))
    }
  }

  fn lower_expr_impl(&mut self, ast_expr: &mut Expr, context: ExprContext) -> HirExpr {
    let hir_kind =
      match &mut ast_expr.kind {
        ExprKind::Literal(lit) => HirExprKind::Literal(lit.clone()),

        ExprKind::Identifier(_name, Some(symbol_id)) => self.lower_identifier(*symbol_id),

        ExprKind::Identifier(name, None) => {
          self.error(&format!("Unresolved identifier in expression: {}", name), vec![], vec![]);
          HirExprKind::Error
        }

        ExprKind::Path(path) => {
          if let Some(last_segment) = path.segments.last() {
            if let Some(symbol_id) = last_segment.symbol_id {
              self.lower_identifier(symbol_id)
            } else {
              self.error("Unresolved path", vec![], vec![]);
              HirExprKind::Error
            }
          } else {
            self.error("Empty path", vec![], vec![]);
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
              ty: self.lower_type(ast_expr.ty.clone()),
              span: ast_expr.span,
              id: ast_expr.id,
            }),
          }
        }

        ExprKind::Call { callee, call, .. } => {
          let callee_expr = Box::new(self.lower_expr(callee));
          self.handle_call_info(&mut call.call_info);
          HirExprKind::Call {
            callee: callee_expr,
            args: self.lower_call_args(&mut call.args),
            call_info: call.call_info.clone(),
          }
        }

        ExprKind::StaticCall { callee, call } => {
          let ExprKind::FieldAccess(fields) = &mut callee.kind else { unreachable!() };
          self.handle_call_info(&mut call.call_info);

          let expr = self.lower_expr(&mut fields[1]);
          HirExprKind::Call {
            callee: Box::new(expr),
            args: self.lower_call_args(&mut call.args),
            call_info: call.call_info.clone(),
          }
        }

        ExprKind::MethodCall { chain, call } => {
          self.handle_call_info(&mut call.call_info);

          if chain.len() < 2 {
            self.error("Invalid method call chain", vec![], vec![]);
            return HirExpr {
              kind: HirExprKind::Error,
              ty: self.lower_type(ast_expr.ty.clone()),
              span: ast_expr.span,
              id: ast_expr.id,
            };
          }

          let receiver = self.lower_expr(&mut chain[0]);
          let method_expr = self.lower_expr(chain.last_mut().unwrap());
          let method = self.symbol_table.get_generic_symbol(
            if let HirExprKind::Symbol(id) = method_expr.kind { id } else { unreachable!() },
          );
          let GenericSymbolKind::Function { signature, .. } = &method.unwrap().kind else {
            unreachable!()
          };

          let mut method_args = vec![if signature.parameters.first().unwrap().ty.is_reference() {
            HirExpr {
              kind: HirExprKind::Unary { op: UnaryOp::Ref, operand: Box::new(receiver.clone()) },
              ty: self.lower_type(Type::Pointer(Box::new(Type::Id(receiver.ty)))),
              span: Default::default(),
              id: self.ast_id_arena.alloc(()),
            }
          } else {
            receiver
          }];
          method_args.extend(self.lower_call_args(&mut call.args));

          HirExprKind::Call {
            callee: Box::new(method_expr),
            args: method_args,
            call_info: call.call_info.clone(),
          }
        }

        ExprKind::StructInit { name, fields, call_info, .. } => {
          self.handle_call_info(call_info);

          let is_enum_variant = self.is_enum_variant_constructor(name);

          if is_enum_variant {
            let callee = Box::new(self.lower_expr(name));
            let mut field_values = Vec::new();

            if let Some(field_order) = self.get_variant_field_order(name) {
              for field_name in field_order {
                if let Some(field_expr) = fields.get_mut(&field_name) {
                  field_values.push(self.lower_expr(field_expr));
                }
              }
            }

            HirExprKind::Call { callee, args: field_values, call_info: call_info.clone() }
          } else {
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
        }

        ExprKind::FieldAccess(fields) => {
          let base = &mut fields[0].clone();
          let sym_id = FieldSymbol::Expr(Box::new(self.lower_expr(base)));
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
      ty: self.lower_type(ast_expr.ty.clone()),
      span: ast_expr.span,
      id: ast_expr.id,
    }
  }

  pub fn error(&mut self, message: &str, labels: Vec<(String, Span)>, notes: Vec<String>) {
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

  pub fn warn(&mut self, message: &str, labels: Vec<(String, Span)>, notes: Vec<String>) {
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
    HirMatchArm { pattern, body, span: arm.span }
  }

  fn lower_pattern(&mut self, pattern: &mut Pattern) -> HirPattern {
    match pattern {
      Pattern::Wildcard => HirPattern::Wildcard,
      Pattern::Identifier(name) => {
        self.error(&format!("Unresolved pattern identifier: {}", name), vec![], vec![]);
        HirPattern::Wildcard
      }
      Pattern::Literal(lit) => HirPattern::Literal(lit.clone()),
      Pattern::EnumVariant { path: _, fields, resolved_variant } => {
        let hir_fields = fields
          .as_mut()
          .map(|fields| fields.iter_mut().map(|field| self.lower_pattern_field(field)).collect());

        HirPattern::EnumVariant { symbol_id: resolved_variant.unwrap(), fields: hir_fields }
      }
      Pattern::Struct { name, fields: _ } => {
        self.error(&format!("Unresolved struct in pattern: {}", name), vec![], vec![]);
        HirPattern::Wildcard
      }
    }
  }

  fn lower_pattern_field(&mut self, field: &mut PatternField) -> HirPatternField {
    let pattern = field.pattern.as_mut().map(|pat| Box::new(self.lower_pattern(pat)));

    HirPatternField {
      name: field.name,
      symbol_id: field.sym_id.unwrap(),
      pattern,
      span: field.span,
      ty: self.lower_type(field.ty.clone().unwrap()),
    }
  }

  fn lower_if_stmt(&mut self, if_stmt: &mut If) -> HirExpr {
    let condition = Box::new(self.lower_expr(&mut if_stmt.condition));

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

    let then_branch = Box::new(HirExpr {
      kind: HirExprKind::Block(then_branch_stmts),
      ty: self.lower_type(Type::Void),
      span: if_stmt.span,
      id: if_stmt.then_branch_id,
    });

    let else_branch = if let Some(ref mut else_branch) = if_stmt.else_branch {
      match else_branch {
        ElseBranch::Block(stmts, block_id) => {
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

          Some(Box::new(HirExpr {
            kind: HirExprKind::Block(else_stmts),
            ty: self.lower_type(Type::Void),
            span: if_stmt.span,
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
      ty: self.lower_type(Type::Void),
      span: if_stmt.span,
      id: if_stmt.then_branch_id,
    }
  }
}

pub fn lower_ast_to_hir<'reports>(
  lexed_modules: &mut Vec<LexedModule>,
  symbol_table: &mut MonoSymbolTable,
  sym_table: &SymbolTable,
  reports: &Reports<'reports>,
  is_library: bool,
  mode: Mode,
) -> Vec<HirModule> {
  let mut lowering = AstLowering::new(symbol_table, sym_table, reports, is_library, mode);
  lowering.lower_modules(lexed_modules)
}
