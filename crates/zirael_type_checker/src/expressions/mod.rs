use std::collections::HashMap;
use zirael_parser::{
  AstId, AstWalker, BinaryOp, CallInfo, ElseBranch, EnumVariantData, Expr, ExprKind, If, Literal,
  MatchArm, Pattern, PatternField, ScopeType, Stmt, StmtKind, SymbolId, SymbolKind, Type, UnaryOp,
  VarDecl, WalkerContext,
};
use zirael_utils::prelude::{
  Color, Colorize, Identifier, ReportBuilder, ReportKind, Span, resolve, warn,
};

use crate::{
  TypeInference, monomorphization::MonomorphizationData, unification::UnificationResult,
};

mod binary;
mod fields;

impl<'reports> TypeInference<'reports> {
  pub(crate) fn expect_type(
    &mut self,
    expected: &Type,
    actual: &Type,
    span: &Span,
    context: &str,
  ) -> bool {
    if !self.eq(expected, actual) {
      self.error(
        &format!(
          "type mismatch in {}: expected {}, found {}",
          context,
          self.format_type(expected),
          self.format_type(actual)
        ),
        vec![(format!("in this {}", context), span.clone())],
        vec![],
      );
      false
    } else {
      true
    }
  }

  fn check_call_args(&mut self, params: &[Type], args: &mut [Expr], span: &Span) -> bool {
    if args.len() != params.len() {
      self.error(
        &format!("wrong number of arguments: expected {}, found {}", params.len(), args.len()),
        vec![("in this call".to_string(), span.clone())],
        vec![],
      );
      return false;
    }
    let mut valid = true;
    for (i, (arg, param_type)) in args.iter_mut().zip(params.iter()).enumerate() {
      let _arg_type = self.infer_expr(arg);
      self.try_monomorphize_named_type(&mut arg.ty);
      if !self.expect_type(param_type, &mut arg.ty, &arg.span, &format!("argument {}", i + 1)) {
        valid = false;
      }
    }
    valid
  }

  pub fn infer_expr(&mut self, expr: &mut Expr) -> Type {
    let ty = match &mut expr.kind {
      ExprKind::Literal(lit) => self.infer_literal(lit),
      ExprKind::Identifier(_, symbol_id) => {
        if let Some(sym_id) = symbol_id {
          self.infer_identifier(*sym_id, &expr.span)
        } else {
          self.error(
            "unresolved identifier",
            vec![("here".to_string(), expr.span.clone())],
            vec![],
          );
          Type::Error
        }
      }
      ExprKind::Assign(lhs, rhs) => self.infer_assignment(lhs, rhs),
      ExprKind::AssignOp(lhs, op, rhs) => self.infer_assign_op(lhs, op, rhs),
      ExprKind::Block(stmts) => self.infer_block(stmts, expr.id),
      ExprKind::Unary(op, expr) => self.infer_unary(op, expr),
      ExprKind::Binary { left, op, right } => self.infer_binary(left, op, right),
      ExprKind::Call { callee, call } => {
        self.infer_call(callee, &mut call.args, &mut call.call_info, &mut call.type_annotations)
      }
      ExprKind::Paren(expr) => self.infer_expr(expr),
      ExprKind::StructInit { name, fields, call_info } => {
        self.infer_struct_init(name, fields, call_info)
      }
      ExprKind::FieldAccess(fields) => self.infer_field_access(fields),
      ExprKind::IndexAccess(expr, index) => self.infer_index_access(expr, index),
      ExprKind::MethodCall { chain, call } => self.infer_method_call(
        chain,
        &mut call.args,
        &mut call.call_info,
        &mut call.type_annotations,
      ),
      ExprKind::StaticCall { callee, call } => self.infer_static_call(
        callee,
        &mut call.args,
        &mut call.call_info,
        &mut call.type_annotations,
      ),
      ExprKind::Ternary { true_expr, false_expr, condition } => {
        self.infer_ternary(condition, true_expr, false_expr)
      }
      ExprKind::Match { scrutinee, arms } => self.infer_match(scrutinee, arms),

      _ => {
        warn!("unimplemented expr: {:#?}", expr);
        Type::Error
      }
    };
    expr.ty = ty.clone();
    ty
  }

  fn infer_ternary(
    &mut self,
    condition: &mut Expr,
    true_expr: &mut Expr,
    false_expr: &mut Expr,
  ) -> Type {
    let condition_ty = self.infer_expr(condition);
    let true_ty = self.infer_expr(true_expr);
    let false_ty = self.infer_expr(false_expr);

    self.expect_type(&Type::Bool, &condition_ty, &condition.span, "ternary condition");

    match self.unify_types(&true_ty, &false_ty) {
      UnificationResult::Identical(ty) => ty,
      UnificationResult::Unified(ty) => {
        true_expr.ty = ty.clone();
        false_expr.ty = ty.clone();
        self.update_expr_recursively(true_expr, &ty);
        self.update_expr_recursively(false_expr, &ty);
        ty
      }
      UnificationResult::Incompatible => {
        self.ternary_error(&true_ty, &false_ty, true_expr, false_expr)
      }
    }
  }

  fn ternary_error(
    &mut self,
    true_ty: &Type,
    false_ty: &Type,
    true_expr: &mut Expr,
    false_expr: &mut Expr,
  ) -> Type {
    let true_expr_ty = self.format_type(true_ty).dimmed().bold();
    let false_expr_ty = self.format_type(false_ty).dimmed().bold();
    self.error(
      &format!(
        "ternary operator branches have incompatible types: {true_expr_ty} and {false_expr_ty}",
      ),
      vec![
        (format!("true branch has type {true_expr_ty}"), true_expr.span.clone()),
        (format!("false branch has type {false_expr_ty}"), false_expr.span.clone()),
      ],
      vec![],
    );
    Type::Error
  }

  fn infer_unary(&mut self, op: &UnaryOp, expr: &mut Expr) -> Type {
    let operand_ty = self.infer_expr(expr);
    match op {
      UnaryOp::Box => operand_ty,
      UnaryOp::Deref => {
        if operand_ty == Type::Error {
          Type::Error
        } else if let Type::Reference(ty) | Type::Pointer(ty) = operand_ty {
          *ty
        } else {
          self.error(
            "deref can be only applied to reference types",
            vec![("attempted to dereference here".to_string(), expr.span.clone())],
            vec![],
          );
          Type::Error
        }
      }
      UnaryOp::BitwiseNot | UnaryOp::Minus => {
        if let Type::Int = operand_ty {
          operand_ty
        } else {
          self.error(
            &format!(
              "unary operator can only be applied to integer types, found {}",
              self.format_type(&operand_ty).dimmed().bold()
            ),
            vec![("attempted to apply unary operator here".to_string(), expr.span.clone())],
            vec![],
          );
          Type::Error
        }
      }
      UnaryOp::Not => Type::Bool,
      // correctness of operand is check in memory pas
      UnaryOp::Ref => Type::Reference(Box::new(operand_ty)),
    }
  }

  fn infer_assignment(&mut self, lhs: &mut Expr, rhs: &mut Expr) -> Type {
    let lhs_ty = self.infer_expr(lhs);
    let rhs_ty = self.infer_expr(rhs);
    if !self.expect_type(&lhs_ty, &rhs_ty, &lhs.span, "assignment") {
      return Type::Error;
    }
    Type::Void
  }

  fn infer_block(&mut self, stmts: &mut [Stmt], id: AstId) -> Type {
    self.push_scope(ScopeType::Block(id));
    if stmts.is_empty() {
      self.pop_scope();
      return Type::Void;
    }

    let mut block_type = Type::Void;

    for stmt in stmts.iter_mut() {
      match &mut stmt.0 {
        StmtKind::Expr(expr) => {
          self.infer_expr(expr);
        }
        StmtKind::Return(ret) => {
          if let Some(expr) = ret.value.as_mut() {
            block_type = self.infer_expr(expr);
          } else {
            block_type = Type::Void;
          }
          break;
        }
        StmtKind::Var(var) => {
          self.infer_variable(var);
        }
        StmtKind::If(if_stmt) => {
          self.infer_if_stmt(if_stmt);
        }
      }
    }
    self.pop_scope();

    block_type
  }

  pub fn infer_call(
    &mut self,
    callee: &mut Expr,
    args: &mut Vec<Expr>,
    call_info: &mut Option<CallInfo>,
    type_annotations: &mut Vec<Type>,
  ) -> Type {
    if let Some((_, Some(sym_id))) = callee.as_identifier_mut() {
      let sym = self.symbol_table().get_symbol_unchecked(sym_id);
      if let SymbolKind::Function { signature, .. } = &sym.kind {
        let signature = &mut signature.clone();

        let expected_arg_count = if signature.is_static() {
          signature.parameters.len()
        } else {
          signature.parameters.len() - 1
        };

        if args.len() != expected_arg_count {
          self.error(
            &format!(
              "wrong number of arguments: expected {}, found {}",
              expected_arg_count,
              args.len()
            ),
            vec![("in this call".to_string(), callee.span.clone())],
            vec![],
          );
          return Type::Error;
        }

        if !type_annotations.is_empty() && type_annotations.len() != signature.generics.len() {
          self.error(
            &format!(
              "wrong number of type annotations: expected {}, found {}",
              signature.generics.len(),
              type_annotations.len()
            ),
            vec![("in this call".to_string(), callee.span.clone())],
            vec![],
          );
          return Type::Error;
        }

        for arg in args.iter_mut() {
          self.infer_expr(arg);
          self.try_monomorphize_named_type(&mut arg.ty);
        }

        let mut generic_mapping = HashMap::new();

        if !type_annotations.is_empty() {
          for (generic, annotation) in signature.generics.iter().zip(type_annotations.iter()) {
            let mut resolved_annotation = annotation.clone();
            self.try_monomorphize_named_type(&mut resolved_annotation);
            generic_mapping.insert(generic.name, resolved_annotation);
          }
        } else if !signature.generics.is_empty() {
          for generic in signature.generics.iter() {
            if !self.ctx.is_generic_parameter(generic.name) {
              let _type_var = self.ctx.fresh_type_var(Some(generic.name));
            }
          }
        }

        let params_to_check = if signature.is_static() {
          &mut signature.parameters[..]
        } else {
          &mut signature.parameters[1..]
        };

        let mut valid = true;

        if type_annotations.is_empty() {
          for (arg, param) in args.iter().zip(params_to_check.iter()) {
            self.infer_generic_types(&param.ty, &arg.ty, &mut generic_mapping);
          }
        }

        let mut concrete_params = params_to_check.to_vec();
        for param in concrete_params.iter_mut() {
          self.substitute_type_with_map(&mut param.ty, &generic_mapping);
        }

        for (i, (arg, param)) in args.iter().zip(concrete_params.iter()).enumerate() {
          if !self.expect_type(&param.ty, &arg.ty, &arg.span, &format!("argument {}", i + 1)) {
            valid = false;
          }
        }
        signature.parameters = concrete_params.clone();

        if !valid {
          return Type::Error;
        }

        let mut return_type = signature.return_type.clone();
        self.substitute_type_with_map(&mut return_type, &generic_mapping);
        signature.return_type = return_type.clone();

        let monomorphized_id = if !signature.generics.is_empty() && valid {
          let unresolved_generics: Vec<_> =
            signature.generics.iter().filter(|g| !generic_mapping.contains_key(&g.name)).collect();

          if unresolved_generics.is_empty() {
            Some(self.record_monomorphization_with_id(
              *sym_id,
              &generic_mapping,
              Some(MonomorphizationData::Signature(signature.clone())),
            ))
          } else {
            let function_name = resolve(&sym.name);
            let generics_list =
              unresolved_generics.iter().map(|g| resolve(&g.name)).collect::<Vec<_>>().join(", ");

            let suggestion = if type_annotations.is_empty() {
              format!(
                "consider providing explicit type annotations, e.g., '{}<{}>({})'",
                function_name,
                signature.generics.iter().map(|g| resolve(&g.name)).collect::<Vec<_>>().join(", "),
                args.iter().map(|_| "_").collect::<Vec<_>>().join(", ")
              )
            } else {
              "the provided type annotations are insufficient to resolve all generic parameters"
                .to_string()
            };

            self.error(
              &format!(
                "cannot infer type parameter{} {} for function {}",
                if unresolved_generics.len() > 1 { "s" } else { "" },
                generics_list.dimmed().bold(),
                function_name.dimmed().bold()
              ),
              vec![("in this call".to_string(), callee.span.clone())],
              vec![suggestion],
            );
            return Type::Error;
          }
        } else {
          None
        };

        *call_info = Some(CallInfo {
          original_symbol: *sym_id,
          monomorphized_id,
          concrete_types: generic_mapping.clone(),
        });

        return return_type;
      } else {
        self.error(
          &format!("cannot call non-function type: {}", sym.kind.name()),
          vec![("in this call".to_string(), callee.span.clone())],
          vec![],
        );
        return Type::Error;
      }
    }

    let callee_type = self.infer_expr(callee);
    match callee_type {
      Type::Function { params, return_type } => {
        if !self.check_call_args(&params, args, &callee.span) {
          return Type::Error;
        }
        *return_type
      }
      _ => {
        self.error(
          &format!("cannot call non-function type: {}", self.format_type(&callee_type)),
          vec![("in this call".to_string(), callee.span.clone())],
          vec![],
        );
        Type::Error
      }
    }
  }

  fn infer_if_stmt(&mut self, if_stmt: &mut If) {
    let condition_type = self.infer_expr(&mut if_stmt.condition);
    if !self.eq(&condition_type, &Type::Bool) {
      self.error(
        &format!("if condition must be boolean, found {}", self.format_type(&condition_type)),
        vec![("condition here".to_string(), if_stmt.condition.span.clone())],
        vec![],
      );
    }

    for stmt in &mut if_stmt.then_branch {
      match &mut stmt.0 {
        StmtKind::Expr(expr) => {
          self.infer_expr(expr);
        }
        StmtKind::Return(ret) => {
          if let Some(expr) = ret.value.as_mut() {
            self.infer_expr(expr);
          }
        }
        StmtKind::Var(var) => {
          self.infer_variable(var);
        }
        StmtKind::If(nested_if) => {
          self.infer_if_stmt(nested_if);
        }
      }
    }

    if let Some(else_branch) = &mut if_stmt.else_branch {
      self.infer_else_branch(else_branch);
    }
  }

  fn infer_else_branch(&mut self, else_branch: &mut ElseBranch) {
    match else_branch {
      ElseBranch::Block(statements, _else_branch_id) => {
        for stmt in statements {
          match &mut stmt.0 {
            StmtKind::Expr(expr) => {
              self.infer_expr(expr);
            }
            StmtKind::Return(ret) => {
              if let Some(expr) = ret.value.as_mut() {
                self.infer_expr(expr);
              }
            }
            StmtKind::Var(var) => {
              self.infer_variable(var);
            }
            StmtKind::If(nested_if) => {
              self.infer_if_stmt(nested_if);
            }
          }
        }
      }
      ElseBranch::If(nested_if) => {
        self.infer_if_stmt(nested_if);
      }
    }
  }

  pub fn infer_variable(&mut self, decl: &mut VarDecl) -> Type {
    let value_ty = &self.infer_expr(&mut decl.value);
    let variable_ty = if let Type::Inferred = decl.ty { value_ty.clone() } else { decl.ty.clone() };

    if let Some(symbol_id) = decl.symbol_id {
      let symbol = self.symbol_table.get_symbol_unchecked(&symbol_id);
      self.ctx.add_variable(symbol.id, variable_ty.clone());
      if !self.expect_type(&variable_ty, value_ty, &decl.span, "variable declaration") {
        return Type::Error;
      }

      if self.eq(&variable_ty, &Type::Void) && variable_ty != Type::Error {
        self.error(
          "cannot initialize variable with void type",
          vec![("here".to_string(), decl.span.clone())],
          vec![],
        );
      }

      decl.ty = value_ty.clone();
      variable_ty.clone()
    } else {
      self.error(
        "unresolved variable declaration",
        vec![("here".to_string(), decl.span.clone())],
        vec![],
      );
      Type::Error
    }
  }

  fn infer_identifier(&mut self, sym_id: SymbolId, span: &Span) -> Type {
    let sym = self.symbol_table().get_symbol_unchecked(&sym_id);

    match &sym.kind {
      SymbolKind::Parameter { .. }
      | SymbolKind::Variable { .. }
      | SymbolKind::MatchBinding { .. } => {
        if let Some(var_type) = self.ctx.get_variable(sym_id) {
          var_type.clone()
        } else {
          self.error(
            &format!("variable '{}' used before initialization", resolve(&sym.name)),
            vec![
              ("here".to_string(), span.clone()),
              ("declared here".to_string(), sym.source_location.unwrap()),
            ],
            vec![],
          );
          Type::Error
        }
      }
      _ => {
        self.error(
          &format!("identifier '{}' cannot be used in this context", resolve(&sym.name)),
          vec![("here".to_string(), span.clone())],
          vec![],
        );
        Type::Error
      }
    }
  }

  fn infer_literal(&mut self, lit: &Literal) -> Type {
    let ty = match lit {
      Literal::Bool(_) => Type::Bool,
      Literal::Char(_) => Type::Char,
      Literal::Float(_) => Type::Float,
      Literal::Integer(_) => Type::Int,
      Literal::String(_) => Type::String,
    };
    ty
  }

  pub fn infer_generic_types(
    &mut self,
    expected: &Type,
    actual: &Type,
    mapping: &mut HashMap<Identifier, Type>,
  ) {
    match (expected, actual) {
      (Type::Named { name, generics }, concrete_type) => {
        if generics.is_empty() && self.ctx.is_generic_parameter(*name) {
          mapping.insert(*name, concrete_type.clone());
        } else if !generics.is_empty() {
          if let Type::Named { name: a_name, generics: a_generics } = concrete_type {
            if name == a_name && generics.len() == a_generics.len() {
              for (e_gen, a_gen) in generics.iter().zip(a_generics.iter()) {
                self.infer_generic_types(e_gen, a_gen, mapping);
              }
            }
          }
        }
      }

      (Type::Variable { name, .. }, concrete_type) => {
        mapping.insert(*name, concrete_type.clone());
      }

      (Type::Pointer(e_inner), Type::Pointer(a_inner)) => {
        self.infer_generic_types(e_inner, a_inner, mapping);
      }

      (Type::Reference(e_inner), Type::Reference(a_inner)) => {
        self.infer_generic_types(e_inner, a_inner, mapping);
      }

      (Type::Array(e_inner, _), Type::Array(a_inner, _)) => {
        self.infer_generic_types(e_inner, a_inner, mapping);
      }

      _ => {}
    }
  }

  fn infer_match(&mut self, scrutinee: &mut Expr, arms: &mut Vec<MatchArm>) -> Type {
    let scrutinee_ty = self.infer_expr(scrutinee);

    if arms.is_empty() {
      self.error("match expression must have at least one arm", vec![], vec![]);
      return Type::Error;
    }

    for (i, arm) in arms.iter_mut().enumerate() {
      if !self.check_pattern_compatibility(arm, &scrutinee_ty) {
        self.error(
          &format!(
            "pattern in match arm {} is incompatible with scrutinee type {}",
            i + 1,
            self.format_type(&scrutinee_ty)
          ),
          vec![
            ("scrutinee has this type".to_string(), scrutinee.span.clone()),
            ("pattern here".to_string(), arm.span.clone()),
          ],
          vec![],
        );
      }
    }

    let mut arm_types = Vec::new();
    for arm in arms.iter_mut() {
      let body_ty = self.infer_expr(&mut arm.body);
      arm_types.push(body_ty);
    }

    let mut unified_type = arm_types[0].clone();
    for (i, arm_ty) in arm_types.iter().enumerate().skip(1) {
      match self.unify_types(&unified_type, arm_ty) {
        UnificationResult::Identical(_) => {}
        UnificationResult::Unified(new_unified) => {
          unified_type = new_unified;
        }
        UnificationResult::Incompatible => {
          let report = ReportBuilder::builder(
            format!(
              "mismatched types in match arms: expected {}, found {}",
              self.format_type(&unified_type),
              self.format_type(arm_ty)
            ),
            ReportKind::Error,
          )
          .label_color_custom(
            &format!("this arm returns {}", self.format_type(&unified_type)),
            arms[0].span.start..arms[0].span.end,
            Color::BrightGreen,
          )
          .label_color_custom(
            &format!("this arm returns {}", self.format_type(arm_ty)),
            arms[i].span.start..arms[i].span.end,
            Color::BrightRed,
          )
          .note("all match arms must return the same type");

          self.reports.add(self.processed_file.unwrap(), report);
        }
      }
    }

    unified_type
  }

  fn check_pattern_compatibility(&mut self, arm: &mut MatchArm, scrutinee_ty: &Type) -> bool {
    match &mut arm.pattern {
      Pattern::Wildcard => true,
      Pattern::Identifier(_) => true,
      Pattern::Literal(lit) => {
        let lit_ty = match lit {
          Literal::Bool(_) => Type::Bool,
          Literal::Char(_) => Type::Char,
          Literal::Float(_) => Type::Float,
          Literal::Integer(_) => Type::Int,
          Literal::String(_) => Type::String,
        };
        self.eq(&lit_ty, scrutinee_ty)
      }
      Pattern::EnumVariant { path, fields: pattern_fields, resolved_variant } => {
        if let Some(variant) = self.check_enum_variant_pattern_compatibility(
          path,
          arm.span.clone(),
          pattern_fields,
          scrutinee_ty,
        ) {
          *resolved_variant = Some(variant);
          true
        } else {
          false
        }
      }
      Pattern::Struct { name, .. } => {
        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
          if let Type::Named { name: scrutinee_name, .. } = scrutinee_ty {
            *scrutinee_name == symbol.name
          } else {
            false
          }
        } else {
          false
        }
      }
    }
  }

  fn check_enum_variant_pattern_compatibility(
    &mut self,
    path: &mut Box<Expr>,
    span: Span,
    pattern_fields: &mut Option<Vec<PatternField>>,
    scrutinee_ty: &Type,
  ) -> Option<SymbolId> {
    let ExprKind::FieldAccess(fields) = &mut path.kind else {
      return None;
    };

    let Some((_, sym_id)) = fields[0].as_identifier_unchecked() else {
      self.non_struct_type(fields[0].span.clone(), file!(), line!());
      return None;
    };

    let Some((ident, call_id)) = fields[1].as_identifier_mut() else {
      self.error(
        "right now, only one level of static calls are supported",
        vec![("in this field access".to_string(), fields[1].span.clone())],
        vec![],
      );
      return None;
    };

    let sym = self.symbol_table.get_symbol_unchecked(&sym_id);
    match &sym.kind {
      SymbolKind::Enum { methods: _, variants, generics: enum_generics, .. } => {
        let mut variant_sym_id = None;
        for variant in variants {
          let variant_symbol = self.symbol_table.get_symbol_unchecked(&variant);
          if variant_symbol.name == *ident {
            variant_sym_id = Some(variant);
            break;
          }
        }

        if let Some(variant_id) = variant_sym_id {
          let variant_sym = self.symbol_table.get_symbol_unchecked(&variant_id);

          if let SymbolKind::EnumVariant { data, .. } = &variant_sym.kind {
            match data {
              EnumVariantData::Unit => Some(*variant_id),
              EnumVariantData::Struct(variant_fields) => {
                if let Some(pf) = pattern_fields {
                  for field in pf {
                    if let Some(str_field) = variant_fields.iter().find(|f| f.name == field.name) {
                      let mut field_ty = str_field.ty.clone();

                      if let Type::Named { generics: concrete_generics, .. } = scrutinee_ty {
                        if !enum_generics.is_empty() && !concrete_generics.is_empty() {
                          self.substitute_generic_params(
                            &mut field_ty,
                            enum_generics,
                            concrete_generics,
                          );
                        }
                      }

                      field.ty = Some(field_ty.clone());
                      self.ctx.add_variable(field.sym_id.unwrap(), field_ty);
                    } else {
                      self.error(
                        &format!(
                          "no field named {} found for this pattern",
                          field.name.to_string().dimmed().bold()
                        ),
                        vec![("here".to_string(), span)],
                        vec![],
                      );
                      return None;
                    }
                  }
                }

                Some(*variant_id)
              }
            }
          } else {
            self.error(
              "can only match enum variants with struct data",
              vec![("here".to_string(), span)],
              vec![],
            );
            None
          }
        } else {
          self.error(
            // TODO: probably would want to add a name of this pattern
            &format!("no variant found for this pattern",),
            vec![("here".to_string(), span)],
            vec![],
          );
          None
        }
      }
      _ => {
        todo!()
      }
    }
  }
}
