use crate::TypeInference;
use zirael_parser::{AstWalker, BinaryOp, Expr, Type};
use zirael_utils::prelude::{Colorize, Span};

impl<'reports> TypeInference<'reports> {
  pub fn infer_assign_op(&mut self, lhs: &mut Expr, op: &BinaryOp, rhs: &mut Expr) -> Type {
    let lhs_ty = self.infer_expr(lhs);
    let rhs_ty = self.infer_expr(rhs);

    let result_ty = self.validate_assignment_operation(op, &lhs_ty, &rhs_ty, &lhs.span);

    if result_ty != Type::Error
      && !self.expect_type(&lhs_ty, &result_ty, &lhs.span, "assignment operation")
    {
      return Type::Error;
    }

    Type::Void
  }

  fn validate_assignment_operation(
    &mut self,
    op: &BinaryOp,
    lhs_ty: &Type,
    rhs_ty: &Type,
    span: &Span,
  ) -> Type {
    match op {
      BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
        self.validate_arithmetic_types(lhs_ty, rhs_ty, span, "arithmetic assignment")
      }
      BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor | BinaryOp::Shl | BinaryOp::Shr => {
        self.validate_bitwise_assignment(lhs_ty, rhs_ty, span)
      }
      BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
        self.error(
          "comparison operators cannot be used in assignment operations",
          vec![("here".to_string(), span.clone())],
          vec![],
        );
        Type::Error
      }
      BinaryOp::And | BinaryOp::Or => {
        self.error(
          "logical operators cannot be used in assignment operations",
          vec![("here".to_string(), span.clone())],
          vec![],
        );
        Type::Error
      }
    }
  }

  fn validate_bitwise_assignment(&mut self, lhs_ty: &Type, rhs_ty: &Type, span: &Span) -> Type {
    if lhs_ty.is_int() && rhs_ty.is_int() {
      lhs_ty.clone()
    } else {
      self.error(
        &format!(
          "bitwise assignment operations can only be applied to integer types, found {} and {}",
          self.format_type(lhs_ty).dimmed().bold(),
          self.format_type(rhs_ty).dimmed().bold()
        ),
        vec![("here".to_string(), span.clone())],
        vec![],
      );
      Type::Error
    }
  }

  pub fn infer_binary(&mut self, left: &mut Expr, op: &BinaryOp, right: &mut Expr) -> Type {
    let left_type = self.infer_expr(left);
    let right_type = self.infer_expr(right);

    match op {
      BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
        self.validate_arithmetic_types(&left_type, &right_type, &left.span, "arithmetic")
      }
      BinaryOp::Eq | BinaryOp::Ne => {
        self.validate_equality_types(&left_type, &right_type, &left.span)
      }
      BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
        self.validate_comparison_types(&left_type, &right_type, &left.span)
      }
      BinaryOp::And | BinaryOp::Or => {
        self.validate_logical_types(&left_type, &right_type, &left.span)
      }
      BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
        self.validate_bitwise_types(&left_type, &right_type, &left.span)
      }
      BinaryOp::Shl | BinaryOp::Shr => {
        self.validate_shift_types(&left_type, &right_type, &left.span)
      }
    }
  }

  fn validate_arithmetic_types(
    &mut self,
    left_ty: &Type,
    right_ty: &Type,
    span: &Span,
    context: &str,
  ) -> Type {
    if left_ty.is_int() && right_ty.is_int() {
      left_ty.clone()
    } else if self.is_mixed_numeric(left_ty, right_ty)
      || (left_ty.is_float() && right_ty.is_float())
    {
      Type::Float
    } else {
      self.error(
        &format!(
          "cannot perform {} operation on {} and {}",
          context,
          self.format_type(left_ty).dimmed().bold(),
          self.format_type(right_ty).dimmed().bold()
        ),
        vec![("here".to_string(), span.clone())],
        vec![],
      );
      Type::Error
    }
  }

  fn validate_equality_types(&mut self, left_ty: &Type, right_ty: &Type, span: &Span) -> Type {
    if self.types_are_comparable_for_equality(left_ty, right_ty) {
      Type::Bool
    } else {
      self.error(
        &format!(
          "cannot compare {} and {} for equality",
          self.format_type(left_ty).dimmed().bold(),
          self.format_type(right_ty).dimmed().bold()
        ),
        vec![("here".to_string(), span.clone())],
        vec![],
      );
      Type::Bool
    }
  }

  fn validate_comparison_types(&mut self, left_ty: &Type, right_ty: &Type, span: &Span) -> Type {
    if left_ty.is_numeric() && right_ty.is_numeric() {
      Type::Bool
    } else {
      self.error(
        &format!(
          "cannot compare {} and {} with ordering operators",
          self.format_type(left_ty).dimmed().bold(),
          self.format_type(right_ty).dimmed().bold()
        ),
        vec![("here".to_string(), span.clone())],
        vec![],
      );
      Type::Bool
    }
  }

  fn validate_logical_types(&mut self, left_ty: &Type, right_ty: &Type, span: &Span) -> Type {
    if left_ty.is_bool() && right_ty.is_bool() {
      Type::Bool
    } else {
      self.error(
        &format!(
          "logical operators require boolean operands, found {} and {}",
          self.format_type(left_ty).dimmed().bold(),
          self.format_type(right_ty).dimmed().bold()
        ),
        vec![("here".to_string(), span.clone())],
        vec![],
      );
      Type::Bool
    }
  }

  fn validate_bitwise_types(&mut self, left_ty: &Type, right_ty: &Type, span: &Span) -> Type {
    if left_ty.is_int() && right_ty.is_int() {
      left_ty.clone()
    } else {
      self.error(
        &format!(
          "bitwise operations can only be applied to integer types, found {} and {}",
          self.format_type(left_ty).dimmed().bold(),
          self.format_type(right_ty).dimmed().bold()
        ),
        vec![("here".to_string(), span.clone())],
        vec![],
      );
      Type::Error
    }
  }

  fn validate_shift_types(&mut self, left_ty: &Type, right_ty: &Type, span: &Span) -> Type {
    if left_ty.is_int() && right_ty.is_int() {
      left_ty.clone()
    } else {
      self.error(
        &format!(
          "shift operations require integer operands, found {} and {}",
          self.format_type(left_ty).dimmed().bold(),
          self.format_type(right_ty).dimmed().bold()
        ),
        vec![("here".to_string(), span.clone())],
        vec![],
      );
      Type::Error
    }
  }

  fn is_mixed_numeric(&self, left_ty: &Type, right_ty: &Type) -> bool {
    (left_ty.is_float() && right_ty.is_int()) || (left_ty.is_int() && right_ty.is_float())
  }

  fn types_are_comparable_for_equality(&self, left_ty: &Type, right_ty: &Type) -> bool {
    left_ty == right_ty
      || (left_ty.is_numeric() && right_ty.is_numeric())
      || self.can_be_coerced(left_ty, right_ty)
  }

  fn can_be_coerced(&self, _from: &Type, _to: &Type) -> bool {
    false
  }
}
