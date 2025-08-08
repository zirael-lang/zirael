use crate::ir::HirLowering;
use std::hash::{DefaultHasher, Hash, Hasher};
use zirael_parser::SymbolId;
use zirael_utils::{
    ident_table::resolve,
    prelude::{Mode, strip_same_root},
};

impl HirLowering {
    pub fn mangle_symbol(&self, sym_id: SymbolId) -> String {
        if self.mode == Mode::Debug {
            self.symbol_table.get_c_identifier(sym_id).unwrap()
        } else {
            let mut result = String::from("_ZN");
            let symbol = self.symbol_table.get_symbol_unchecked(&sym_id);
            let symbol_name = resolve(&symbol.name);
            let symbol_file = self.symbol_table.get_symbol_module(symbol.scope).unwrap();
            let file_path = self.sources.get_unchecked(symbol_file).path();
            let base_path = strip_same_root(file_path, self.root.clone()).with_extension("");
            result.push_str(&format!("{}{}", symbol_name.len(), symbol_name));

            for component in base_path.components() {
                if let Some(component_str) = component.as_os_str().to_str() {
                    result.push_str(&format!("{}{}", component_str.len(), component_str));
                }
            }

            let mut hasher = DefaultHasher::new();
            symbol_name.hash(&mut hasher);
            let hash = format!("{:x}", hasher.finish());

            result.push('h');
            result.push_str(&hash);
            result.push('E');

            result
        }
    }
}
