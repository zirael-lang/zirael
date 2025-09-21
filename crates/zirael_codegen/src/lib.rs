use crate::generator::CodeGenerator;
use crate::mangling_table::ManglingTable;
use anyhow::Result;
use std::collections::HashSet;
use std::path::PathBuf;
use zirael_hir::hir::HirModule;
use zirael_parser::{OriginalSymbolId, SymbolId, SymbolTable};
use zirael_type_checker::MonoSymbolTable;
use zirael_utils::prelude::CompilationInfo;
use zirael_utils::sources::Sources;

mod codegen;
mod expressions;
mod generator;
mod mangling_table;

pub fn run_codegen(
  hir: Vec<HirModule>,
  used_externals: HashSet<String>,
  sym_table: &mut MonoSymbolTable,
  symbol_table: &SymbolTable,
  sources: &Sources,
  order: Vec<OriginalSymbolId>,
  compilation_info: &CompilationInfo,
  main_function: Option<SymbolId>,
) -> Result<PathBuf> {
  let mut mangling_table = ManglingTable::new(
    sym_table,
    symbol_table,
    sources,
    compilation_info.name.clone(),
    compilation_info.root.clone(),
  );
  mangling_table.mangle_all();

  let mut code_generator = CodeGenerator::new(
    hir,
    used_externals,
    sym_table,
    symbol_table,
    sources,
    order,
    compilation_info,
    main_function
  );

  code_generator.generate_c_code()
}
