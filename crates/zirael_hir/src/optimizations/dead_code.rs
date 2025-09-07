use crate::hir::{
  expr::{HirExpr, HirExprKind},
  lowering::AstLowering,
};
use zirael_parser::Symbol;
use zirael_utils::prelude::{Colorize, resolve};

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
      | HirExprKind::Ternary { .. }
      | HirExprKind::Unary { .. }
      | HirExprKind::Literal(_)
      | HirExprKind::Symbol(_)
      | HirExprKind::FieldAccess { .. }
      | HirExprKind::IndexAccess { .. }
      | HirExprKind::StructInit { .. } => true,

      HirExprKind::Assign { .. }
      | HirExprKind::Call { .. }
      | HirExprKind::Block(_)
      | HirExprKind::Match { .. }
      | HirExprKind::If { .. }
      | HirExprKind::Error => false,
    }
  }

  pub fn result_not_used_error(&mut self, expr: &HirExpr) {
    self.warn(
      "expression result is not used",
      vec![("here".to_owned(), expr.span.clone())],
      vec![],
    );
  }

  pub fn try_unused_symbol(&mut self, symbol: &Symbol) -> bool {
    let name = resolve(&symbol.name);

    if name == "main" || name.starts_with("_") {
      return false;
    }

    if self.is_library {
      return false;
    }

    let is_unused = if let Some(generics) = self.symbol_table.get_generics_for_symbol(symbol) {
      if !generics.is_empty() { !self.mono_table.has_entries(symbol.id) } else { !symbol.is_used }
    } else {
      !symbol.is_used
    };

    if is_unused {
      self.warn(
        &format!("symbol {} is never used", name.dimmed().bold()),
        vec![("declared here".to_string(), symbol.source_location.clone().unwrap())],
        vec!["prefix it with _ to remove this warning".to_string()],
      );
      return true;
    }

    false
  }
}
