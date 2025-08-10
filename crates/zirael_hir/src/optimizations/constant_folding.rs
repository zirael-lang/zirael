use crate::hir::{
    expr::{HirExpr, HirExprKind},
    lowering::AstLowering,
};
use zirael_parser::{BinaryOp, Literal, UnaryOp};

impl<'reports> AstLowering<'reports> {
    pub fn try_to_constant_fold(&mut self, expr: HirExprKind) -> HirExprKind {
        if !self.can_be_folded(&expr) {
            return expr;
        }

        match expr {
            HirExprKind::Literal(_) => expr,

            HirExprKind::Binary { left, op, right } => {
                let left_folded = self.try_to_constant_fold(left.kind);
                let right_folded = self.try_to_constant_fold(right.kind);

                if let (HirExprKind::Literal(left_lit), HirExprKind::Literal(right_lit)) =
                    (&left_folded, &right_folded)
                {
                    if let Some(result) = self.compute_binary_op(left_lit, &op, right_lit) {
                        return result;
                    }
                }

                HirExprKind::Binary {
                    left: Box::new(HirExpr {
                        kind: left_folded,
                        span: left.span,
                        ty: left.ty,
                        id: left.id,
                    }),
                    op,
                    right: Box::new(HirExpr {
                        kind: right_folded,
                        span: right.span,
                        ty: right.ty,
                        id: right.id,
                    }),
                }
            }

            HirExprKind::Unary { op, operand } => {
                let operand_folded = self.try_to_constant_fold(operand.kind);

                if let HirExprKind::Literal(lit) = &operand_folded {
                    if let Some(result) = self.compute_unary_op(&op, lit) {
                        return result;
                    }
                }

                HirExprKind::Unary {
                    op,
                    operand: Box::new(HirExpr {
                        kind: operand_folded,
                        span: operand.span,
                        ty: operand.ty,
                        id: operand.id,
                    }),
                }
            }

            HirExprKind::Symbol(sym) => self.folded_vars.get(&sym).cloned().unwrap_or(expr),

            _ => expr,
        }
    }

    pub fn can_be_folded(&mut self, expr: &HirExprKind) -> bool {
        match expr {
            HirExprKind::Literal(lit) => {
                matches!(lit, Literal::Integer(_) | Literal::Float(_))
            }

            HirExprKind::Binary { left, op, right } => {
                self.can_fold_binary_op(op)
                    && self.can_be_folded(&left.kind)
                    && self.can_be_folded(&right.kind)
            }

            HirExprKind::Unary { op, operand } => {
                matches!(op, UnaryOp::BitwiseNot | UnaryOp::Minus)
                    && self.can_be_folded(&operand.kind)
            }

            HirExprKind::Symbol(sym) => self.folded_vars.contains_key(sym),

            _ => false,
        }
    }

    fn can_fold_binary_op(&self, op: &BinaryOp) -> bool {
        matches!(
            op,
            BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Rem
                | BinaryOp::BitAnd
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::Shl
                | BinaryOp::Shr
        )
    }

    fn compute_binary_op(
        &self,
        left: &Literal,
        op: &BinaryOp,
        right: &Literal,
    ) -> Option<HirExprKind> {
        match (left, right) {
            (Literal::Integer(a), Literal::Integer(b)) => self
                .compute_int_binary_op(*a, op, *b)
                .map(|result| HirExprKind::Literal(Literal::Integer(result))),

            (Literal::Float(a), Literal::Float(b)) => self
                .compute_float_binary_op(*a, op, *b)
                .map(|result| HirExprKind::Literal(Literal::Float(result))),

            (Literal::Integer(a), Literal::Float(b)) => self
                .compute_float_binary_op(*a as f64, op, *b)
                .map(|result| HirExprKind::Literal(Literal::Float(result))),

            (Literal::Float(a), Literal::Integer(b)) => self
                .compute_float_binary_op(*a, op, *b as f64)
                .map(|result| HirExprKind::Literal(Literal::Float(result))),

            _ => None,
        }
    }

    fn compute_int_binary_op(&self, left: i64, op: &BinaryOp, right: i64) -> Option<i64> {
        match op {
            BinaryOp::Add => left.checked_add(right),
            BinaryOp::Sub => left.checked_sub(right),
            BinaryOp::Mul => left.checked_mul(right),
            BinaryOp::Div => {
                if right == 0 {
                    None
                } else {
                    left.checked_div(right)
                }
            }
            BinaryOp::Rem => {
                if right == 0 {
                    None
                } else {
                    left.checked_rem(right)
                }
            }
            BinaryOp::BitAnd => Some(left & right),
            BinaryOp::BitOr => Some(left | right),
            BinaryOp::BitXor => Some(left ^ right),
            BinaryOp::Shl => {
                if right < 0 || right >= 64 {
                    None
                } else {
                    left.checked_shl(right as u32)
                }
            }
            BinaryOp::Shr => {
                if right < 0 || right >= 64 {
                    None
                } else {
                    Some(left >> right)
                }
            }
            _ => None,
        }
    }

    fn compute_float_binary_op(&self, left: f64, op: &BinaryOp, right: f64) -> Option<f64> {
        let result = match op {
            BinaryOp::Add => left + right,
            BinaryOp::Sub => left - right,
            BinaryOp::Mul => left * right,
            BinaryOp::Div => left / right,
            BinaryOp::Rem => left % right,

            BinaryOp::BitAnd => {
                if left.fract() != 0.0 || right.fract() != 0.0 {
                    return None;
                }
                ((left as i64) & (right as i64)) as f64
            }
            BinaryOp::BitOr => {
                if left.fract() != 0.0 || right.fract() != 0.0 {
                    return None;
                }
                ((left as i64) | (right as i64)) as f64
            }
            BinaryOp::BitXor => {
                if left.fract() != 0.0 || right.fract() != 0.0 {
                    return None;
                }
                ((left as i64) ^ (right as i64)) as f64
            }
            BinaryOp::Shl => {
                if left.fract() != 0.0 || right.fract() != 0.0 || right < 0.0 || right >= 64.0 {
                    return None;
                }
                ((left as i64).checked_shl(right as u32)?) as f64
            }
            BinaryOp::Shr => {
                if left.fract() != 0.0 || right.fract() != 0.0 || right < 0.0 || right >= 64.0 {
                    return None;
                }
                ((left as i64) >> (right as i64)) as f64
            }

            _ => return None,
        };

        if result.is_nan() || result.is_infinite() { Some(result) } else { Some(result) }
    }

    fn compute_unary_op(&self, op: &UnaryOp, operand: &Literal) -> Option<HirExprKind> {
        match operand {
            Literal::Integer(val) => {
                let result = match op {
                    UnaryOp::Minus => val.checked_neg()?,
                    UnaryOp::BitwiseNot => !val,
                    _ => return None,
                };
                Some(HirExprKind::Literal(Literal::Integer(result)))
            }

            Literal::Float(val) => {
                let result = match op {
                    UnaryOp::Minus => -val,
                    UnaryOp::BitwiseNot => {
                        if val.fract() != 0.0 {
                            return None;
                        }
                        !(*val as i64) as f64
                    }
                    _ => return None,
                };

                if result.is_nan() || result.is_infinite() {
                    Some(HirExprKind::Literal(Literal::Float(result)))
                } else {
                    Some(HirExprKind::Literal(Literal::Float(result)))
                }
            }

            _ => None,
        }
    }
}
