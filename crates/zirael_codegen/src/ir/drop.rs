use crate::ir::{HirLowering, IrExpr, IrExprKind, IrStmt};
use itertools::Itertools as _;
use zirael_parser::{DropStackEntry, Type};
use zirael_utils::prelude::debug;

impl<'reports> HirLowering<'reports> {
  pub fn add_drop(&mut self, entries: Vec<DropStackEntry>, stmts: &mut Vec<IrStmt>) {
    let used_names = self.get_used_symbol_names(&entries);
    debug!("dropping {} values", used_names.len());
    
    if used_names.is_empty() {
      return;
    }
    
    let insert_position = self.find_drop_insertion_position(stmts);
    
    for name in used_names.into_iter().rev() {
      let drop_stmt = self.create_free_statement(name);
      stmts.insert(insert_position, drop_stmt);
    }
  }

  fn get_used_symbol_names(&mut self, entries: &[DropStackEntry]) -> Vec<String> {
    entries
      .iter()
      .map(|entry| entry.symbol_id)
      .dedup()
      .filter_map(|id| {
        let sym = self.symbol_table.get_symbol_unchecked(&id);
        if sym.is_used { 
          Some(self.mangle_symbol(id)) 
        } else { 
          None 
        }
      })
      .collect_vec()
  }

  fn find_drop_insertion_position(&self, stmts: &[IrStmt]) -> usize {
    if let Some(last) = stmts.last() {
      if matches!(last, IrStmt::Return(_)) {
        return stmts.len().saturating_sub(1);
      }
    }
    stmts.len()
  }

  fn create_free_statement(&mut self, name: String) -> IrStmt {
    IrStmt::Expr(IrExpr::new(
      Type::Void,
      IrExprKind::CCall(
        "free".to_owned(),
        vec![IrExpr::new(Type::Inferred, IrExprKind::Symbol(name))],
      ),
    ))
  }
}
