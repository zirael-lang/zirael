use crate::mangling_table::ManglingTable;
use std::collections::HashSet;
use std::path::PathBuf;
use zirael_hir::hir::HirModule;
use zirael_parser::{OriginalSymbolId, SymbolTable};
use zirael_type_checker::MonoSymbolTable;
use zirael_utils::sources::Sources;

mod mangling_table;

pub fn run_codegen(
  hir: Vec<HirModule>,
  used_externals: HashSet<String>,
  sym_table: &mut MonoSymbolTable,
  symbol_table: &SymbolTable,
  sources: &Sources,
  order: Vec<OriginalSymbolId>,
  name: String,
  root: PathBuf,
) {
  let mut mangling_table = ManglingTable::new(sym_table, symbol_table, sources, name, root);
  mangling_table.mangle_all();

  for item in order {
    println!("{:?}", sym_table.mangled_names.get(&item));
  }
}
