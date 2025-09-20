use std::path::PathBuf;
use zirael_parser::{MonomorphizationId, OriginalSymbolId, Path, Symbol, SymbolId, SymbolTable};
use zirael_type_checker::{GenericSymbol, MonoSymbolTable};
use zirael_utils::prelude::{Sources, get_or_intern, resolve, strip_same_root};

#[derive(Debug)]
pub struct ManglingTable<'table, 'sources> {
  pub sym_table: &'table mut MonoSymbolTable,
  /// this holds important info like parent symbols, declaration module, etc.
  pub symbol_table: &'table SymbolTable,
  pub sources: &'sources Sources,
  pub package_name: String,
  pub root: PathBuf,
}

impl<'table, 'sources> ManglingTable<'table, 'sources> {
  pub fn new(
    sym_table: &'table mut MonoSymbolTable,
    symbol_table: &'table SymbolTable,
    sources: &'sources Sources,
    package_name: String,
    root: PathBuf,
  ) -> Self {
    Self { sym_table, package_name, symbol_table, sources, root }
  }

  pub fn mangle_all(&mut self) {
    let mut remaining_symbols: Vec<_> =
      self.sym_table.generic_symbols.clone().into_iter().collect();
    let mut progress_made = true;

    while !remaining_symbols.is_empty() && progress_made {
      progress_made = false;
      let mut i = 0;

      while i < remaining_symbols.len() {
        let (_, sym) = &remaining_symbols[i];

        let can_process =
          if let Some(parent) = self.symbol_table.is_a_child_of_symbol(sym.symbol_id()) {
            self.sym_table.mangled_names.contains_key(&OriginalSymbolId::Symbol(parent))
          } else {
            true
          };

        if can_process {
          if let Some(mono) = self.sym_table.get_mono_variants(sym.symbol_id()) {
            self.mangle_mono(mono);
          } else {
            self.mangle_generic(sym.symbol_id());
          }
          remaining_symbols.remove(i);
          progress_made = true;
        } else {
          i += 1;
        }
      }
    }

    if !remaining_symbols.is_empty() {
      panic!("Could not resolve all symbol dependencies. Possible circular reference.");
    }
  }

  pub fn get_module_path(&self, sym: &GenericSymbol) -> PathBuf {
    let canonical_id = self.symbol_table.get_symbol_unchecked(&sym.symbol_id()).canonical_symbol;
    let canonical_symbol = self.symbol_table.get_symbol_unchecked(&canonical_id);

    let symbol_file = self.symbol_table.get_symbol_module(canonical_symbol.scope).unwrap();
    let file_path = self.sources.get_unchecked(symbol_file).path();
    strip_same_root(&file_path, self.root.as_path()).with_extension("")
  }

  pub fn get_module_segment(&self, sym: &GenericSymbol) -> String {
    let mod_path = self.get_module_path(sym);
    let segments: Vec<String> =
      mod_path.iter().map(|os_str| os_str.to_string_lossy().to_string()).collect();

    if segments.is_empty() {
      get_or_intern(&self.package_name, None).to_string()
    } else {
      segments.join("_")
    }
  }

  pub fn get_base_name(&self, sym: &GenericSymbol) -> String {
    let mut buff = String::new();

    if let Some(parent) = self.symbol_table.is_a_child_of_symbol(sym.symbol_id()) {
      if let Some(parent_name) = self.sym_table.mangled_names.get(&OriginalSymbolId::Symbol(parent))
      {
        buff.push_str(&parent_name.base);
        buff.push('_');
      } else {
        panic!("parent symbol not mangled yet. probably a ordering issue");
      }
    }
    buff.push_str(&resolve(sym.name()));

    buff
  }

  pub fn mangle_mono(&mut self, variants: Vec<MonomorphizationId>) {
    for variant in variants {
      let mono = self.sym_table.get_monomorphized_symbol(variant).unwrap();
      let generic = self.sym_table.get_generic_symbol(mono.original_symbol_id()).unwrap();
      let mod_path = self.get_module_segment(generic);

      let base = self.get_base_name(&generic);

      self.sym_table.add_mangled_name(OriginalSymbolId::Monomorphization(variant), mod_path, base);
    }
  }

  pub fn mangle_generic(&mut self, symbol: SymbolId) {
    let sym = self.sym_table.get_generic_symbol(symbol).unwrap();
    let base = self.get_base_name(&sym);
    let mod_path = self.get_module_segment(sym);

    self.sym_table.add_mangled_name(OriginalSymbolId::Symbol(symbol), mod_path, base);
  }
}
