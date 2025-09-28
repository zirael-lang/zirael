use crate::hir::{
  HirItem,
  expr::{HirExpr, HirExprKind},
  lowering::AstLowering,
};
use zirael_parser::Symbol;
use zirael_type_checker::{GenericEnumData, GenericSymbol, GenericSymbolKind};
use zirael_utils::prelude::{Colorize as _, resolve, warn};

impl<'reports, 'mono> AstLowering<'reports, 'mono> {
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
    self.warn("expression result is not used", vec![("here".to_owned(), expr.span)], vec![]);
  }

  pub fn try_unused_symbol(&mut self, symbol: &GenericSymbol) -> bool {
    let name = resolve(&symbol.name());

    if name == "main" || name.starts_with('_') {
      return false;
    }

    if self.is_library {
      return false;
    }

    let is_unused = match &symbol.kind {
      GenericSymbolKind::Enum { generics, variants } if !generics.is_empty() => {
        variants.iter().all(|v| !self.symbol_table.has_mono_variant(v.symbol_id))
      }
      _ => {
        if !symbol.generics().is_empty() {
          !self.symbol_table.has_mono_variant(symbol.base.symbol_id)
        } else {
          !symbol.base.is_used
        }
      }
    };

    if is_unused {
      self.warn(
        &format!("symbol {} is never used", name.dimmed().bold()),
        vec![("declared here".to_owned(), symbol.base.span)],
        vec!["prefix it with _ to remove this warning".to_owned()],
      );
      return true;
    }

    false
  }
}
