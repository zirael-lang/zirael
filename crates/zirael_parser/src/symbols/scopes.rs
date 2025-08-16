use crate::{
    AstId, Expr, ExprKind, Symbol,
    symbols::{SymbolId, SymbolTable, SymbolTableError},
};
use id_arena::Id;
use std::collections::HashMap;
use zirael_utils::prelude::*;

pub type ScopeId = Id<Scope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub children: Vec<ScopeId>,
    pub symbols: HashMap<Identifier, SymbolId>,
    pub scope_type: ScopeType,
    pub depth: usize,
    pub imported_modules: Vec<ScopeId>,
    pub drop_stack: Vec<DropStackEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DropStackEntry {
    pub symbol_id: SymbolId,
    pub drop_span: Span,
}

impl DropStackEntry {
    pub fn new(symbol_id: SymbolId, drop_span: Span) -> Self {
        Self { symbol_id, drop_span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ScopeType {
    Global,
    Module(SourceFileId),
    Function(AstId),
    Block(AstId),
    Struct(AstId),
    Enum(AstId),
    Loop(AstId),
    Conditional(AstId),
}

impl SymbolTable {
    pub fn create_scope(&self, scope_type: ScopeType) -> ScopeId {
        self.write(|table| {
            let current_scope = table.current_scope_creation_id;
            let parent_depth = table.scopes_arena.get(current_scope).map(|s| s.depth).unwrap_or(0);

            let new_scope = Scope {
                parent: Some(current_scope),
                children: Vec::new(),
                symbols: HashMap::new(),
                scope_type,
                depth: parent_depth + 1,
                imported_modules: Vec::new(),
                drop_stack: Vec::new(),
            };

            let scope_id = table.scopes_arena.alloc(new_scope);

            if let Some(parent_scope) = table.scopes_arena.get_mut(current_scope) {
                parent_scope.children.push(scope_id);
            }

            table.current_scope_creation_id = scope_id;

            scope_id
        })
    }

    pub fn exit_scope(&self) -> Result<ScopeId, SymbolTableError> {
        self.write(|table| {
            let current_scope = table.current_scope_creation_id;

            if let Some(scope) = table.scopes_arena.get(current_scope) {
                if let Some(parent_id) = scope.parent {
                    table.current_scope_creation_id = parent_id;
                    Ok(current_scope)
                } else {
                    Err(SymbolTableError::InvalidScope(current_scope))
                }
            } else {
                Err(SymbolTableError::ScopeNotFound(current_scope))
            }
        })
    }

    pub fn push_scope(&self, scope_type: ScopeType) -> Result<ScopeId, SymbolTableError> {
        self.write(|table| {
            let current_scope = table.current_traversal_scope;

            if let Some(scope) = table.scopes_arena.get(current_scope) {
                if let Some(&target_child) = scope.children.iter().find(|&&child_id| {
                    table
                        .scopes_arena
                        .get(child_id)
                        .map(|child| child.scope_type == scope_type)
                        .unwrap_or(false)
                }) {
                    table.current_traversal_scope = target_child;
                    Ok(target_child)
                } else {
                    debug!("No child scope of type {scope_type:?} found in {scope:?}");
                    Err(SymbolTableError::InvalidScope(current_scope))
                }
            } else {
                Err(SymbolTableError::ScopeNotFound(current_scope))
            }
        })
    }

    pub fn pop_scope(&self) -> Result<ScopeId, SymbolTableError> {
        self.write(|table| {
            let current_scope = table.current_traversal_scope;

            if let Some(scope) = table.scopes_arena.get(current_scope) {
                if let Some(parent_id) = scope.parent {
                    table.current_traversal_scope = parent_id;
                    Ok(current_scope)
                } else {
                    Err(SymbolTableError::InvalidScope(current_scope))
                }
            } else {
                Err(SymbolTableError::ScopeNotFound(current_scope))
            }
        })
    }

    pub fn navigate_to_scope(&self, scope_id: ScopeId) -> Result<(), SymbolTableError> {
        self.write(|table| {
            if table.scopes_arena.get(scope_id).is_some() {
                table.current_traversal_scope = scope_id;
                Ok(())
            } else {
                Err(SymbolTableError::ScopeNotFound(scope_id))
            }
        })
    }

    pub fn get_child_scopes(&self) -> Vec<ScopeId> {
        self.read(|table| {
            table
                .scopes_arena
                .get(table.current_traversal_scope)
                .map(|scope| scope.children.clone())
                .unwrap_or_default()
        })
    }

    pub fn find_child_scope_by_type(&self, scope_type: &ScopeType) -> Option<ScopeId> {
        self.read(|table| {
            let current_scope = table.current_traversal_scope;
            table
                .scopes_arena
                .get(current_scope)?
                .children
                .iter()
                .find(|&&child_id| {
                    table
                        .scopes_arena
                        .get(child_id)
                        .map(|child| &child.scope_type == scope_type)
                        .unwrap_or(false)
                })
                .copied()
        })
    }

    pub fn reset_to_global(&self) {
        self.write(|table| {
            table.current_traversal_scope = table.global_scope;
        });
    }

    pub fn reset_construction_to_global(&self) {
        self.write(|table| {
            table.current_scope_creation_id = table.global_scope;
        });
    }

    pub fn get_scope_path(&self) -> Vec<ScopeId> {
        self.read(|table| {
            let mut path = Vec::new();
            let mut current = table.current_traversal_scope;

            while let Some(scope) = table.scopes_arena.get(current) {
                path.push(current);
                if let Some(parent) = scope.parent {
                    current = parent;
                } else {
                    break;
                }
            }

            path.reverse();
            path
        })
    }

    pub fn current_scope(&self) -> ScopeId {
        self.read(|table| table.current_traversal_scope)
    }

    pub fn current_construction_scope(&self) -> ScopeId {
        self.read(|table| table.current_scope_creation_id)
    }

    pub fn global_scope(&self) -> ScopeId {
        self.read(|table| table.global_scope)
    }

    pub fn get_scope(&self, id: ScopeId) -> Option<Scope> {
        self.read(|table| table.scopes_arena.get(id).cloned())
    }

    pub fn get_scope_unchecked(&self, id: ScopeId) -> Scope {
        self.read(|table| table.scopes_arena.get(id).cloned().unwrap())
    }

    pub fn lookup_in_scope(&self, name: Identifier, scope: ScopeId) -> Option<SymbolId> {
        self.read(|table| table.name_lookup.get(&(name, scope)).copied())
    }

    pub fn get_symbols_in_scope(&self, scope_id: ScopeId) -> Vec<(Identifier, SymbolId)> {
        self.read(|table| {
            table
                .scopes_arena
                .get(scope_id)
                .map(|scope| scope.symbols.iter().map(|(&name, &id)| (name, id)).collect())
                .unwrap_or_default()
        })
    }

    pub fn mark_drop(&self, symbol_id: SymbolId, span: Span) {
        self.write(|table| {
            let current_scope = table.current_traversal_scope;
            table
                .scopes_arena
                .get_mut(current_scope)
                .unwrap()
                .drop_stack
                .push(DropStackEntry::new(symbol_id, span));
        });
    }

    pub fn is_borrowed(&self, symbol_id: SymbolId) -> Option<DropStackEntry> {
        self.read(|table| {
            let current_scope = table.current_traversal_scope;
            table
                .scopes_arena
                .get(current_scope)
                .map(|scope| {
                    scope.drop_stack.iter().find(|entry| entry.symbol_id == symbol_id).cloned()
                })
                .unwrap_or(None)
        })
    }

    pub fn symbol_from_expr(&self, expr: &Expr) -> Option<(Symbol, SymbolId)> {
        if let ExprKind::Identifier(_, sym_id) = expr.kind {
            self.get_symbol(sym_id?).map(|sym| (sym, sym_id.unwrap()))
        } else {
            None
        }
    }
}
