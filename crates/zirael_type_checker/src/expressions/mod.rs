use crate::TypeInference;
use crate::symbol_table::TyId;
use crate::unification::UnificationResult;
use std::collections::HashMap;
use zirael_parser::{
  AstWalker, EnumVariantData, Expr, ExprKind, Literal, MatchArm, Path, Pattern, PatternField,
  SymbolId, SymbolKind, Type, UnaryOp, VarDecl, WalkerContext,
};
use zirael_utils::prelude::{
  Color, Colorize, Identifier, ReportBuilder, ReportKind, Span, debug, resolve, warn,
};

mod binary;
mod block_inference;
mod call;
// mod fields;

impl<'reports> TypeInference<'reports> {
  pub(crate) fn expect_type(
    &mut self,
    expected: &Type,
    actual: &Type,
    span: &Span,
    context: &str,
  ) -> bool {
    let mut expected_norm = expected.clone();
    let mut actual_norm = actual.clone();
    self.visit_type(&mut expected_norm);
    self.visit_type(&mut actual_norm);

    if !self.eq(&expected_norm, &actual_norm) {
      self.type_mismatch_with_context(expected, actual, span.clone(), context);
      false
    } else {
      true
    }
  }

  pub fn infer_expr(&mut self, expr: &mut Expr) -> Type {
    self.infer_expr_with_expected(expr, None)
  }

  pub fn infer_expr_with_expected(
    &mut self,
    expr: &mut Expr,
    expected_type: Option<&Type>,
  ) -> Type {
    let ty = match &mut expr.kind {
      ExprKind::Literal(lit) => self.infer_literal(lit),
      ExprKind::Identifier(_, symbol_id) => {
        if let Some(sym_id) = symbol_id {
          self.infer_identifier(*sym_id, &expr.span)
        } else {
          self.simple_error("unresolved identifier", "here", expr.span.clone());
          Type::Error
        }
      }
      ExprKind::Assign(lhs, rhs) => self.infer_assignment(lhs, rhs),
      ExprKind::AssignOp(lhs, op, rhs) => self.infer_assign_op(lhs, op, rhs),
      ExprKind::Block(stmts) => self.infer_block_with_expected(stmts, expr.id, expected_type),
      ExprKind::Unary(op, expr) => self.infer_unary(op, expr),
      ExprKind::Binary { left, op, right } => self.infer_binary(left, op, right),
      ExprKind::Call { callee, call } => {
        self.infer_call(callee, &mut call.args, &mut call.call_info, &mut call.type_annotations)
      }
      ExprKind::Paren(expr) => self.infer_expr_with_expected(expr, expected_type),
      ExprKind::StructInit { name, fields, call_info, type_annotations } => {
        self.infer_struct_init(name, fields, call_info, type_annotations, expected_type)
      }
      // ExprKind::FieldAccess(fields) => self.infer_field_access(fields),
      // ExprKind::IndexAccess(expr, index) => self.infer_index_access(expr, index),
      // ExprKind::MethodCall { chain, call } => self.infer_method_call(
      //   chain,
      //   &mut call.args,
      //   &mut call.call_info,
      //   &mut call.type_annotations,
      // ),
      // ExprKind::StaticCall { callee, call } => self.infer_static_call_with_expected(
      //   callee,
      //   &mut call.args,
      //   &mut call.call_info,
      //   &mut call.type_annotations,
      //   expected_type,
      // ),
      ExprKind::Ternary { true_expr, false_expr, condition } => {
        self.infer_ternary_with_expected(condition, true_expr, false_expr, expected_type)
      }
      ExprKind::Match { scrutinee, arms } => self.infer_match(scrutinee, arms),
      ExprKind::Path(path) => self.infer_path_with_expected(path, expected_type),

      _ => {
        warn!("unimplemented expr: {:#?}", expr);
        Type::Error
      }
    };
    // Normalize the resulting type so no Named variants remain post-inference.
    let mut normalized = ty.clone();
    self.visit_type(&mut normalized);
    expr.ty = normalized.clone();
    normalized
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

  fn infer_ternary_with_expected(
    &mut self,
    condition: &mut Expr,
    true_expr: &mut Expr,
    false_expr: &mut Expr,
    expected_type: Option<&Type>,
  ) -> Type {
    let condition_ty = self.infer_expr(condition);
    let true_ty = self.infer_expr_with_expected(true_expr, expected_type);
    let false_ty = self.infer_expr_with_expected(false_expr, expected_type);

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
        self.ternary_error(&true_ty, &false_ty, true_expr, false_expr);
        Type::Error
      }
    }
  }

  pub fn infer_variable(&mut self, decl: &mut VarDecl) -> Type {
    let expected_type = if let Type::Inferred = decl.ty { None } else { Some(&decl.ty) };
    let value_ty = &self.infer_expr_with_expected(&mut decl.value, expected_type);
    let mut variable_ty =
      if let Type::Inferred = decl.ty { value_ty.clone() } else { decl.ty.clone() };

    self.visit_type(&mut variable_ty);

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
        debug!("identifier '{}' resolved to symbol: {:#?}", resolve(&sym.name), sym);
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
    match lit {
      Literal::Bool(_) => Type::Bool,
      Literal::Char(_) => Type::Char,
      Literal::Float(_) => Type::Float,
      Literal::Integer(_) => Type::Int,
      Literal::String(_) => Type::String,
    }
  }

  pub fn infer_generic_types(
    &mut self,
    expected: &Type,
    actual: &Type,
    mapping: &mut HashMap<Identifier, TyId>,
  ) {
    match (expected, actual) {
      (Type::Named { name, generics }, concrete_type) => {
        if generics.is_empty() && self.ctx.is_generic_parameter(*name) {
          mapping.insert(*name, self.sym_table.intern_type(concrete_type.clone()));
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
        mapping.insert(*name, self.sym_table.intern_type(concrete_type.clone()));
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

      (Type::Id(id), _) | (_, Type::Id(id)) => {}

      _ => {
        println!(
          "Mismatched types in generic inference: expected {}, found {}",
          self.format_type(expected),
          self.format_type(actual)
        );
      }
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
            Span::new(arms[0].span.start, arms[0].span.end),
            Color::BrightGreen,
          )
          .label_color_custom(
            &format!("this arm returns {}", self.format_type(arm_ty)),
            Span::new(arms[i].span.start, arms[i].span.end),
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
    path: &mut Path,
    span: Span,
    pattern_fields: &mut Option<Vec<PatternField>>,
    scrutinee_ty: &Type,
  ) -> Option<SymbolId> {
    if path.segments.len() < 2 {
      return None;
    }

    let enum_segment = &path.segments[0];
    let variant_segment = &path.segments[1];

    let Some(enum_sym_id) = enum_segment.symbol_id else {
      return None;
    };

    let enum_sym = self.symbol_table.get_symbol_unchecked(&enum_sym_id);
    match &enum_sym.kind {
      SymbolKind::Enum { methods: _, variants, generics: enum_generics, .. } => {
        let mut variant_sym_id = None;
        for variant in variants {
          let variant_symbol = self.symbol_table.get_symbol_unchecked(&variant);
          if variant_symbol.name == variant_segment.identifier {
            variant_sym_id = Some(*variant);
            break;
          }
        }

        if let Some(variant_id) = variant_sym_id {
          let variant_sym = self.symbol_table.get_symbol_unchecked(&variant_id);

          if let SymbolKind::EnumVariant { data, .. } = &variant_sym.kind {
            match data {
              EnumVariantData::Unit => Some(variant_id),
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

                Some(variant_id)
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

  fn infer_path_with_expected(&mut self, path: &mut Path, expected_type: Option<&Type>) -> Type {
    if let Some(last_segment) = path.segments.last() {
      if let Some(symbol_id) = last_segment.symbol_id {
        let symbol = self.symbol_table.get_symbol_unchecked(&symbol_id);
        match &symbol.kind {
          SymbolKind::EnumVariant { parent_enum, .. } => {
            let parent_symbol = self.symbol_table.get_symbol_unchecked(parent_enum);
            if let SymbolKind::Enum { generics, .. } = &parent_symbol.kind {
              if generics.is_empty() {
                Type::Named { name: parent_symbol.name, generics: vec![] }
              } else {
                if let Some(expected) = expected_type {
                  match expected {
                    Type::Named { name, .. } => {
                      if *name == parent_symbol.name {
                        return expected.clone();
                      }
                    }
                    Type::Symbol(symbol_id) => {
                      if let Ok(expected_symbol) = self.symbol_table.get_symbol(*symbol_id) {
                        if expected_symbol.name == parent_symbol.name {
                          return expected.clone();
                        }
                      }
                    }
                    _ => {}
                  }
                }

                self.error(
                  "cannot infer generic enum variant type without context - provide explicit type annotation",
                  vec![("here".to_string(), path.span.clone())],
                  vec![],
                );
                Type::Error
              }
            } else {
              self.error(
                "invalid parent enum for variant",
                vec![("here".to_string(), path.span.clone())],
                vec![],
              );
              Type::Error
            }
          }
          _ => self.infer_identifier(symbol_id, &path.span),
        }
      } else {
        self.error("unresolved path", vec![("here".to_string(), path.span.clone())], vec![]);
        Type::Error
      }
    } else {
      self.error("empty path", vec![("here".to_string(), path.span.clone())], vec![]);
      Type::Error
    }
  }
}
