mod inference;

use crate::inference::TypeInference;
use zirael_parser::{AstWalker, LexedModule, SymbolTable, Type};
use zirael_utils::prelude::*;

pub fn run_type_checker<'reports>(
    table: &SymbolTable,
    reports: &Reports<'reports>,
    sources: &Sources,
    modules: &mut Vec<LexedModule>,
) {
    let mut ty = TypeInference::new(table, reports, sources);

    ty.walk_modules(modules);
}
