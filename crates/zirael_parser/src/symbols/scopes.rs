use crate::{
    SymbolId,
    symbols::table::{SymbolTable, SymbolTableError},
};
use id_arena::Id;
use std::collections::HashMap;
use zirael_utils::prelude::*;

pub type ScopeId = Id<Scope>;

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub children: Vec<ScopeId>,
    pub symbols: HashMap<Identifier, SymbolId>,
    pub scope_type: ScopeType,
    pub depth: usize,
    pub imported_modules: Vec<ScopeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeType {
    Global,
    Module(SourceFileId),
    Function(Identifier),
    Block,
    Class(Identifier),
    Enum(Identifier),
    Loop,
    Conditional,
}

impl SymbolTable {
    pub fn push_scope(&self, scope_type: ScopeType) -> ScopeId {
        self.write(|table| {
            let current_scope = table.current_scope;
            if let Some(existing_child) = table.scopes.get(current_scope).and_then(|scope| {
                scope.children.iter().find(|&&child_id| {
                    table
                        .scopes
                        .get(child_id)
                        .map(|child| child.scope_type == scope_type)
                        .unwrap_or(false)
                })
            }) {
                table.current_scope = *existing_child;
                return *existing_child;
            }

            let parent_id = table.current_scope;
            let parent_depth = table.scopes.get(parent_id).map(|s| s.depth).unwrap_or(0);

            let new_scope = Scope {
                parent: Some(parent_id),
                children: Vec::new(),
                symbols: HashMap::new(),
                scope_type,
                depth: parent_depth + 1,
                imported_modules: Vec::new(),
            };

            let scope_id = table.scopes.alloc(new_scope);

            if let Some(parent) = table.scopes.get_mut(parent_id) {
                parent.children.push(scope_id);
            }

            table.current_scope = scope_id;
            scope_id
        })
    }

    pub fn lookup_in_scope(&self, name: Identifier, scope: ScopeId) -> Option<SymbolId> {
        self.read(|table| table.name_lookup.get(&(name, scope)).copied())
    }

    pub fn pop_scope(&self) -> Result<ScopeId, SymbolTableError> {
        self.write(|table| {
            let current_scope = table.current_scope;

            if let Some(scope) = table.scopes.get(current_scope) {
                if let Some(parent_id) = scope.parent {
                    table.current_scope = parent_id;
                    Ok(current_scope)
                } else {
                    Err(SymbolTableError::InvalidScope(current_scope))
                }
            } else {
                Err(SymbolTableError::ScopeNotFound(current_scope))
            }
        })
    }

    pub fn current_scope(&self) -> ScopeId {
        self.read(|table| table.current_scope)
    }

    pub fn global_scope(&self) -> ScopeId {
        self.read(|table| table.global_scope)
    }

    pub fn get_scope(&self, id: ScopeId) -> Option<Scope> {
        self.read(|table| table.scopes.get(id).cloned())
    }

    pub fn get_scope_unchecked(&self, id: ScopeId) -> Scope {
        self.read(|table| table.scopes.get(id).cloned().unwrap())
    }

    pub fn get_symbols_in_scope(&self, scope_id: ScopeId) -> Vec<(Identifier, SymbolId)> {
        self.read(|table| {
            table
                .scopes
                .get(scope_id)
                .map(|scope| scope.symbols.iter().map(|(&name, &id)| (name, id)).collect())
                .unwrap_or_default()
        })
    }
}
