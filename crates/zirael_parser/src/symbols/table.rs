use crate::{
  Type,
  symbols::{
    Scope, ScopeId, ScopeType, Symbol, SymbolId, SymbolKind, TemporaryLifetime,
    relations::{SymbolRelationNode, SymbolRelations},
  },
};
use id_arena::Arena;
use std::{collections::HashMap, sync::Arc};
use strsim::levenshtein;
use zirael_utils::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolTableError {
  SymbolAlreadyExists { name: Identifier, existing_id: SymbolId, scope: ScopeId },
  SymbolNotFound { name: Identifier, scope: ScopeId },
  InvalidScope(ScopeId),
  ScopeNotFound(ScopeId),
  CircularScopeReference,
  ImportConflict(Vec<ImportConflict>),
  InvalidImportTarget(ScopeId),
  CannotImportFromSelf,
  ModuleNotFound(SourceFileId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportConflict {
  pub name: Identifier,
  pub existing_id: SymbolId,
  pub new_id: SymbolId,
}

#[derive(Debug)]
pub struct SymbolTableImpl {
  pub symbols: Arena<Symbol>,
  pub scopes_arena: Arena<Scope>,
  pub current_scope_creation_id: ScopeId,
  pub current_traversal_scope: ScopeId,
  pub global_scope: ScopeId,
  pub declaration_counter: usize,
  pub name_lookup: HashMap<(Identifier, ScopeId), SymbolId>,
  pub symbol_relations: SymbolRelations,
  pub mangled_names: HashMap<SymbolId, String>,
  pub parent_symbols_lookup: HashMap<SymbolId, Vec<SymbolId>>,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable(Arc<RwLock<SymbolTableImpl>>);

impl Default for SymbolTableImpl {
  fn default() -> Self {
    let symbols = Arena::new();
    let mut scopes = Arena::new();

    let global_scope = Scope {
      parent: None,
      children: Vec::new(),
      symbols: HashMap::new(),
      scope_type: ScopeType::Global,
      depth: 0,
      imported_modules: Vec::new(),
      drop_stack: Vec::new(),
    };

    let global_scope_id = scopes.alloc(global_scope);

    Self {
      symbols,
      scopes_arena: scopes,
      current_scope_creation_id: global_scope_id,
      global_scope: global_scope_id,
      declaration_counter: 0,
      name_lookup: HashMap::new(),
      symbol_relations: SymbolRelations::new(),
      current_traversal_scope: global_scope_id,
      mangled_names: HashMap::new(),
      parent_symbols_lookup: HashMap::new(),
    }
  }
}

impl SymbolTable {
  pub fn new() -> Self {
    Default::default()
  }

  pub fn read<R>(&self, reader: impl FnOnce(&SymbolTableImpl) -> R) -> R {
    reader(&self.0.read())
  }

  pub fn write<R>(&self, writer: impl FnOnce(&mut SymbolTableImpl) -> R) -> R {
    writer(&mut self.0.write())
  }

  pub fn insert(
    &self,
    name: Identifier,
    kind: SymbolKind,
    span: Option<Span>,
  ) -> Result<SymbolId, SymbolTableError> {
    self.write(|table| {
      let current_scope = table.current_scope_creation_id;

      if let Some(&existing_id) = table.name_lookup.get(&(name, current_scope)) {
        return Err(SymbolTableError::SymbolAlreadyExists {
          name,
          existing_id,
          scope: current_scope,
        });
      }

      table.declaration_counter += 1;
      let symbol_id = table.symbols.alloc_with_id(|id| Symbol {
        id,
        name,
        kind,
        scope: current_scope,
        source_location: span,
        is_used: false,
        declaration_order: table.declaration_counter,
        imported_from: None,
        canonical_symbol: id,
      });

      table.name_lookup.insert((name, current_scope), symbol_id);

      if let Some(scope) = table.scopes_arena.get_mut(current_scope) {
        scope.symbols.insert(name, symbol_id);
      }

      Ok(symbol_id)
    })
  }

  pub fn import_all_from_module(
    &self,
    source_file: SourceFileId,
    target_file: SourceFileId,
  ) -> Result<Vec<SymbolId>, SymbolTableError> {
    self.write(|table| {
      let source_module = table
        .find_module_by_source_id(source_file)
        .ok_or(SymbolTableError::ModuleNotFound(source_file))?;
      let target_module = table
        .find_module_by_source_id(target_file)
        .ok_or(SymbolTableError::ModuleNotFound(target_file))?;

      if source_file == target_file {
        return Err(SymbolTableError::CannotImportFromSelf);
      }

      let mut imported_symbols = Vec::new();
      let mut conflicts = Vec::new();

      let source_symbols = table.get_originally_declared_symbols(source_module);

      for (symbol_name, new_id) in source_symbols {
        if let Some(&existing_id) = table.name_lookup.get(&(symbol_name, target_module)) {
          conflicts.push(ImportConflict { name: symbol_name, existing_id, new_id });
          continue;
        }

        let imported_symbol_id =
          table.create_imported_symbol(new_id, symbol_name, target_module, source_module)?;

        imported_symbols.push(imported_symbol_id);
      }

      table.record_module_import(target_module, source_module);

      if !conflicts.is_empty() {
        return Err(SymbolTableError::ImportConflict(conflicts));
      }

      Ok(imported_symbols)
    })
  }

  pub fn get_imported_modules(&self, scope: ScopeId) -> Vec<ScopeId> {
    self.read(|table| {
      table.scopes_arena.get(scope).map(|s| s.imported_modules.clone()).unwrap_or_default()
    })
  }

  pub fn get_import_source(&self, symbol_id: SymbolId) -> Option<ScopeId> {
    self.read(|table| table.symbols.get(symbol_id).and_then(|symbol| symbol.imported_from))
  }

  pub fn get_imported_symbols(&self, scope: ScopeId) -> Vec<(SymbolId, ScopeId)> {
    self.read(|table| {
      table
        .scopes_arena
        .get(scope)
        .map(|scope| {
          scope
            .symbols
            .values()
            .filter_map(|&symbol_id| {
              table
                .symbols
                .get(symbol_id)
                .and_then(|symbol| symbol.imported_from.map(|source| (symbol_id, source)))
            })
            .collect()
        })
        .unwrap_or_default()
    })
  }

  pub fn find_module_by_source(&self, source_file: SourceFileId) -> Option<ScopeId> {
    self.read(|table| {
      table
        .scopes_arena
        .iter()
        .find(|(_, scope)| matches!(scope.scope_type, ScopeType::Module(id) if id == source_file))
        .map(|(id, _)| id)
    })
  }

  pub fn lookup(&self, name: &Identifier) -> Option<SymbolId> {
    self.read(|table| {
      let mut current_scope = Some(table.current_traversal_scope);

      while let Some(scope_id) = current_scope {
        if let Some(&symbol_id) = table.name_lookup.get(&(*name, scope_id)) {
          return Some(symbol_id);
        }
        current_scope = table.scopes_arena.get(scope_id)?.parent;
      }

      None
    })
  }

  pub fn lookup_symbol(&self, name: &Identifier) -> Option<Symbol> {
    let symbol_id = self.lookup(name)?;
    self.get_symbol(symbol_id)
  }

  pub fn get_symbol(&self, id: SymbolId) -> Option<Symbol> {
    self.read(|table| table.symbols.get(id).cloned())
  }

  pub fn get_symbol_unchecked(&self, id: &SymbolId) -> Symbol {
    self.read(|table| table.symbols.get(*id).cloned().unwrap())
  }

  pub fn get_symbol_unchecked_mut<R>(&self, id: &SymbolId, f: impl FnOnce(&mut Symbol) -> R) -> R {
    self.write(|table| {
      let symbol = table.symbols.get_mut(*id).unwrap();
      f(symbol)
    })
  }

  pub fn mark_used(&self, id: SymbolId) -> Result<(), SymbolTableError> {
    self.write(|table| {
      if let Some(symbol) = table.symbols.get_mut(id) {
        symbol.is_used = true;
        Ok(())
      } else {
        Err(SymbolTableError::SymbolNotFound {
          name: default_ident(),
          scope: table.current_traversal_scope,
        })
      }
    })
  }

  pub fn mark_heap_variable(&self, id: SymbolId) -> Result<(), SymbolTableError> {
    self.write(|table| {
      if let Some(symbol) = table.symbols.get_mut(id) {
        if let SymbolKind::Variable { is_heap, .. } = &mut symbol.kind {
          *is_heap = true;
        }
        Ok(())
      } else {
        Err(SymbolTableError::SymbolNotFound {
          name: default_ident(),
          scope: table.current_traversal_scope,
        })
      }
    })
  }

  pub fn get_unused_symbols(&self) -> Vec<(SymbolId, Symbol)> {
    self.read(|table| {
      table
        .symbols
        .iter()
        .filter(|(_, symbol)| !symbol.is_used)
        .map(|(id, symbol)| (id, symbol.clone()))
        .collect()
    })
  }

  pub fn update_symbol_kind(
    &self,
    id: SymbolId,
    new_kind: impl FnOnce(&mut SymbolKind) -> SymbolKind,
  ) -> Result<(), SymbolTableError> {
    self.write(|table| {
      if let Some(symbol) = table.symbols.get_mut(id) {
        symbol.kind = new_kind(&mut symbol.kind);
        Ok(())
      } else {
        Err(SymbolTableError::SymbolNotFound {
          name: default_ident(),
          scope: table.current_traversal_scope,
        })
      }
    })
  }

  pub fn is_ancestor_scope(&self, ancestor: ScopeId, descendant: ScopeId) -> bool {
    self.read(|table| {
      let mut current = Some(descendant);

      while let Some(scope_id) = current {
        if scope_id == ancestor {
          return true;
        }
        current = table.scopes_arena.get(scope_id).expect("missing").parent;
      }

      false
    })
  }

  pub fn get_accessible_symbols(&self) -> Vec<(Identifier, SymbolId, ScopeId)> {
    self.read(|table| {
      let mut symbols = Vec::new();
      let mut current_scope = Some(table.current_traversal_scope);

      while let Some(scope_id) = current_scope {
        if let Some(scope) = table.scopes_arena.get(scope_id) {
          for (&name, &symbol_id) in &scope.symbols {
            symbols.push((name, symbol_id, scope_id));
          }
          current_scope = scope.parent;
        } else {
          break;
        }
      }

      symbols
    })
  }

  pub fn get_c_identifier(&self, id: SymbolId) -> Option<String> {
    self.read(|table| {
      table.symbols.get(id).map(|symbol| {
        if let SymbolKind::Temporary { .. } = &symbol.kind {
          format!("__zirael_temp_{}", id.index())
        } else {
          let base_name = resolve(&symbol.name);
          format!("__zirael_{base_name}")
        }
      })
    })
  }

  pub fn get_temporaries_by_lifetime(
    &self,
    lifetime: TemporaryLifetime,
  ) -> Vec<(SymbolId, Symbol)> {
    self.read(|table| {
      table
        .symbols
        .iter()
        .filter(|(_, symbol)| {
          matches!(&symbol.kind, SymbolKind::Temporary { lifetime: temp_lifetime, .. }
                            if temp_lifetime == &lifetime)
        })
        .map(|(id, symbol)| (id, symbol.clone()))
        .collect()
    })
  }

  pub fn clear(&self) {
    self.write(|table| {
      *table = SymbolTableImpl::default();
    });
  }

  pub fn insert_temporary(
    &self,
    ty: Type,
    lifetime: TemporaryLifetime,
    span: Option<Span>,
  ) -> Result<SymbolId, SymbolTableError> {
    let temp_name = self.write(|table| {
      let temp_count = table
        .symbols
        .iter()
        .filter(|(_, s)| matches!(s.kind, SymbolKind::Temporary { .. }))
        .count();
      get_or_intern(&format!("__temp_{temp_count}"))
    });

    let kind = SymbolKind::Temporary { ty, lifetime };
    self.insert(temp_name, kind, span)
  }

  pub fn find_similar_symbol(
    &self,
    name: &Identifier,
    predicate: impl Fn(&Symbol) -> bool,
  ) -> Option<SymbolId> {
    self.read(|table| {
      let mut current_scope = Some(table.current_traversal_scope);

      while let Some(scope_id) = current_scope {
        let symbols_in_scope = self.get_symbols_in_scope(scope_id);

        if let Some(symbol_id) = symbols_in_scope.iter().find_map(|(symbol_name, symbol_id)| {
          let sym = self.get_symbol_unchecked(symbol_id);

          let is_similar =
            levenshtein(&resolve(name), &resolve(symbol_name)) <= 2 && predicate(&sym);

          if is_similar { Some(*symbol_id) } else { None }
        }) {
          return Some(symbol_id);
        }

        current_scope = table.scopes_arena.get(scope_id)?.parent;
      }

      None
    })
  }

  pub fn get_symbol_module(&self, scope: ScopeId) -> Option<SourceFileId> {
    self.read(|table| {
      let mut current_scope = Some(scope);

      while let Some(scope_id) = current_scope {
        let scope = table.scopes_arena.get(scope_id)?;
        if let ScopeType::Module(id) = &scope.scope_type {
          return Some(*id);
        }
        current_scope = scope.parent;
      }

      None
    })
  }

  pub fn new_relation(&self, referrer: SymbolRelationNode, referred: SymbolRelationNode) {
    self.write(|table| {
      table.symbol_relations.entry(referrer, referred);
    });
  }

  pub fn build_symbol_relations(&self) -> Result<Vec<SymbolRelationNode>> {
    self.read(|table| table.symbol_relations.build_graph())
  }

  pub fn get_mangled_name(&self, id: SymbolId) -> Option<String> {
    self.read(|table| table.mangled_names.get(&id).cloned())
  }

  pub fn add_mangled_name(&self, id: SymbolId, name: String) {
    self.write(|table| {
      table.mangled_names.insert(id, name);
    });
  }

  pub fn get_struct_methods(&self, struct_id: SymbolId) -> Option<Vec<SymbolId>> {
    self.read(|table| table.parent_symbols_lookup.get(&struct_id).cloned())
  }

  pub fn is_a_child_of_symbol(&self, symbol_id: SymbolId) -> Option<SymbolId> {
    self.write(|table| {
      for (&struct_id, methods) in &table.parent_symbols_lookup {
        if methods.contains(&symbol_id) {
          table
            .symbol_relations
            .entry(SymbolRelationNode::Symbol(symbol_id), SymbolRelationNode::Symbol(struct_id));
          return Some(struct_id);
        }
      }
      None
    })
  }

  pub fn add_symbol_child(&self, parent_id: SymbolId, child_id: SymbolId) {
    self.write(|table| {
      table.parent_symbols_lookup.entry(parent_id).or_default().push(child_id);

      if let Some(symbol) = table.symbols.get_mut(parent_id) {
        if let SymbolKind::Struct { methods, .. } = &mut symbol.kind {
          if !methods.contains(&child_id) {
            methods.push(child_id);
          }
        }
      }
    });
  }
}

impl SymbolTableImpl {
  fn find_module_by_source_id(&self, source_file: SourceFileId) -> Option<ScopeId> {
    self
      .scopes_arena
      .iter()
      .find(|(_, scope)| matches!(scope.scope_type, ScopeType::Module(id) if id == source_file))
      .map(|(id, _)| id)
  }

  fn get_originally_declared_symbols(&self, module_id: ScopeId) -> Vec<(Identifier, SymbolId)> {
    let Some(module_scope) = self.scopes_arena.get(module_id) else {
      return Vec::new();
    };

    module_scope
      .symbols
      .iter()
      .filter_map(|(&name, &symbol_id)| {
        let symbol = self.symbols.get(symbol_id)?;
        if symbol.imported_from.is_none() { Some((name, symbol_id)) } else { None }
      })
      .collect()
  }

  fn create_imported_symbol(
    &mut self,
    source_symbol_id: SymbolId,
    symbol_name: Identifier,
    target_module: ScopeId,
    source_module: ScopeId,
  ) -> Result<SymbolId, SymbolTableError> {
    let source_symbol = self
      .symbols
      .get(source_symbol_id)
      .ok_or(SymbolTableError::SymbolNotFound { name: symbol_name, scope: source_module })?
      .clone();

    self.declaration_counter += 1;
    let imported_symbol_id = self.symbols.alloc_with_id(|id| Symbol {
      id,
      name: symbol_name,
      kind: source_symbol.kind,
      scope: target_module,
      source_location: source_symbol.source_location,
      is_used: false,
      declaration_order: self.declaration_counter,
      imported_from: Some(source_module),
      canonical_symbol: source_symbol_id,
    });

    self.name_lookup.insert((symbol_name, target_module), imported_symbol_id);

    if let Some(target_scope) = self.scopes_arena.get_mut(target_module) {
      target_scope.symbols.insert(symbol_name, imported_symbol_id);
    }

    Ok(imported_symbol_id)
  }

  fn record_module_import(&mut self, target_module: ScopeId, source_module: ScopeId) {
    if let Some(target_scope) = self.scopes_arena.get_mut(target_module) {
      if !target_scope.imported_modules.contains(&source_module) {
        target_scope.imported_modules.push(source_module);
      }
    }
  }
}
