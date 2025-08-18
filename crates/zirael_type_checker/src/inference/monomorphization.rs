use crate::TypeInference;
use std::collections::HashMap;
use zirael_parser::{
    MonomorphizationId, StructField, SymbolId, Type,
};
use zirael_utils::prelude::Identifier;

#[derive(Debug, Clone)]
pub struct MonomorphizationEntry {
    pub original_id: SymbolId,
    pub concrete_types: HashMap<Identifier, Type>,
    pub monomorphized_fields: Option<Vec<StructField>>,
}

#[derive(Debug, Clone, Default)]
pub struct MonomorphizationTable {
    pub entries: HashMap<MonomorphizationId, MonomorphizationEntry>,
}

impl MonomorphizationTable {
    pub fn get_entry(&self, id: MonomorphizationId) -> Option<&MonomorphizationEntry> {
        self.entries.get(&id)
    }
}

impl<'reports> TypeInference<'reports> {
    pub fn record_monomorphization(
        &mut self,
        original_id: SymbolId,
        concrete_types: &HashMap<Identifier, Type>,
        id: MonomorphizationId,
        monomorphized_fields: Option<Vec<StructField>>,
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
            MonomorphizationEntry {
                concrete_types: concrete_types.clone(),
                original_id,
                monomorphized_fields,
            },
        );
    }

    pub fn record_monomorphization_with_id(
        &mut self,
        symbol_id: SymbolId,
        concrete_types: &HashMap<Identifier, Type>,
        monomorphized_fields: Option<Vec<StructField>>,
    ) -> MonomorphizationId {
        let candidate_ids: Vec<_> = self
            .mono_table
            .entries
            .iter()
            .filter(|(_, entry)| entry.original_id == symbol_id)
            .map(|(id, _)| *id)
            .collect();
        for id in candidate_ids {
            let entry = self.mono_table.entries[&id].clone();
            let mut fields_eq = true;
            for (name, value) in &entry.concrete_types {
                let Some(other) = concrete_types.get(name) else {
                    fields_eq = false;
                    break;
                };
                if !self.eq(value, other) {
                    fields_eq = false;
                    break;
                }
            }
            for (name, value) in concrete_types {
                let Some(other) = entry.concrete_types.get(name) else {
                    fields_eq = false;
                    break;
                };
                if !self.eq(value, other) {
                    fields_eq = false;
                    break;
                }
            }
            if fields_eq {
                return id;
            }
        }
        let mono_id = self.next_monomorphization_id();
        self.record_monomorphization(symbol_id, concrete_types, mono_id, monomorphized_fields);
        mono_id
    }

    fn next_monomorphization_id(&mut self) -> MonomorphizationId {
        self.mono_arena.alloc(())
    }
}
