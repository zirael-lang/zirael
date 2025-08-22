use std::collections::HashMap;
use zirael_parser::{SymbolId, Type};
use zirael_utils::{ident_table::Identifier, prelude::get_or_intern};

#[derive(Debug, Clone, Default)]
pub struct TypeInferenceContext {
    variables: HashMap<SymbolId, Type>,
    next_type_var_id: usize,
    pub generic_params: HashMap<Identifier, usize>,
}

impl TypeInferenceContext {
    pub fn new() -> Self {
        Self { variables: HashMap::new(), next_type_var_id: 0, generic_params: HashMap::new() }
    }

    pub fn add_variable(&mut self, symbol_id: SymbolId, type_: Type) {
        self.variables.insert(symbol_id, type_);
    }

    pub fn get_variable(&self, symbol_id: SymbolId) -> Option<&Type> {
        self.variables.get(&symbol_id)
    }

    pub fn fresh_type_var(&mut self, name: Option<Identifier>) -> Type {
        let id = self.next_type_var_id;
        self.next_type_var_id += 1;

        let name = name.unwrap_or_else(|| get_or_intern(&format!("T{}", id)));

        self.generic_params.insert(name, id);

        Type::TypeVariable { id, name }
    }

    pub fn is_generic_parameter(&self, name: Identifier) -> bool {
        self.generic_params.contains_key(&name)
    }

    pub fn next_type_var_id(&mut self) -> usize {
        let id = self.next_type_var_id;
        self.next_type_var_id += 1;
        id
    }
}
