use crate::hir::{
    expr::{HirExpr, HirExprKind},
    lowering::AstLowering,
};

impl<'reports> AstLowering<'reports> {
    /// Checks if an expression statement has no side effects and its result is unused.
    /// Such expressions are typically pointless as statements.
    ///
    /// ```zr
    /// 5 + 3; <- pointless expression
    /// print(5); <- actually does something
    /// ```
    ///
    /// This should only be run on expressions directly from statements.
    pub fn is_expr_pointless(&self, expr: &HirExprKind) -> bool {
        match &expr {
            HirExprKind::Binary { .. }
            | HirExprKind::Unary { .. }
            | HirExprKind::Literal(_)
            | HirExprKind::Symbol(_)
            | HirExprKind::FieldAccess { .. }
            | HirExprKind::IndexAccess { .. }
            | HirExprKind::StructInit { .. } => true,

            HirExprKind::Assign { .. }
            | HirExprKind::Call { .. }
            | HirExprKind::Block(_)
            | HirExprKind::Error => false,
        }
    }

    pub fn result_not_used_error(&mut self, expr: &HirExpr) {
        self.error(
            "expression result is not used",
            vec![("here".to_owned(), expr.span.clone())],
            vec![],
        );
    }
}
