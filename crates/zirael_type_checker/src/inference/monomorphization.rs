use crate::{TypeInference, inference::ctx::TypeInferenceContext};
use std::collections::HashMap;
use zirael_parser::{FunctionSignature, MonomorphizationId, Parameter, SymbolId, Type};
use zirael_utils::prelude::{Identifier, resolve, warn};

#[derive(Debug, Clone)]
pub struct MonomorphizationEntry {
    pub original_id: SymbolId,
    pub concrete_types: HashMap<Identifier, Type>,
}

#[derive(Debug, Clone, Default)]
pub struct MonomorphizationTable {
    pub entries: HashMap<MonomorphizationId, MonomorphizationEntry>,
}

impl<'reports> TypeInference<'reports> {
    pub fn record_monomorphization(
        &mut self,
        original_id: SymbolId,
        concrete_types: &HashMap<Identifier, Type>,
        id: MonomorphizationId,
    ) {
        if concrete_types.is_empty() {
            return;
        }

        let has_only_concrete_types = concrete_types
            .iter()
            .all(|(_, ty)| !matches!(ty, Type::TypeVariable { .. } | Type::Inferred));

        if !has_only_concrete_types {
            return;
        }

        self.mono_table.entries.insert(
            id,
            MonomorphizationEntry { concrete_types: concrete_types.clone(), original_id },
        );
    }

    pub(crate) fn record_monomorphization_with_id(
        &mut self,
        symbol_id: SymbolId,
        concrete_types: &HashMap<Identifier, Type>,
    ) -> MonomorphizationId {
        let mono_id = self.next_monomorphization_id();

        self.record_monomorphization(symbol_id, concrete_types, mono_id);
        mono_id
    }

    fn next_monomorphization_id(&mut self) -> MonomorphizationId {
        self.mono_arena.alloc(())
    }
}
