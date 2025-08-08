use crate::ir::{HirLowering, IrExpr, IrExprKind, IrStmt};
use itertools::Itertools;
use std::ops::Index;
use zirael_parser::{DropStackEntry, Type, Type::Inferred};
use zirael_utils::prelude::debug;

impl HirLowering {
    pub fn add_drop(&mut self, entries: Vec<DropStackEntry>, stmts: &mut Vec<IrStmt>) {
        let ids = entries.iter().map(|entry| entry.symbol_id).dedup().collect_vec();
        debug!("dropping {} values", ids.len());

        let names = ids.iter().map(|id| self.mangle_symbol(*id)).collect_vec();
        let insert_pos = if let Some(last) = stmts.last()
            && let IrStmt::Return(_) = last
        {
            stmts.len().saturating_sub(1)
        } else {
            stmts.len()
        };

        for name in names {
            let stmt = IrStmt::Expr(IrExpr::new(
                Type::Void,
                IrExprKind::CCall(
                    "free".to_string(),
                    vec![IrExpr::new(Inferred, IrExprKind::Symbol(name.clone()))],
                ),
            ));

            stmts.insert(insert_pos, stmt);
        }
    }
}
