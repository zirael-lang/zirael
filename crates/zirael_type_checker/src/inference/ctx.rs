use std::collections::HashMap;
use zirael_parser::{SymbolId, Type};

#[derive(Debug, Clone, Default)]
pub struct TypeInferenceContext {
    variables: HashMap<SymbolId, Type>,
}

impl TypeInferenceContext {
    pub fn new() -> Self {
        Self { variables: HashMap::new() }
    }

    pub fn add_variable(&mut self, symbol_id: SymbolId, type_: Type) {
        self.variables.insert(symbol_id, type_);
    }

    pub fn get_variable(&self, symbol_id: SymbolId) -> Option<&Type> {
        self.variables.get(&symbol_id)
    }
}
