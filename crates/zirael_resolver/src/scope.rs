use crate::def::DefId;
use dashmap::DashMap;
use zirael_source::new_id;
use zirael_source::prelude::SourceFileId;

new_id!(ScopeId);

/// A scope represents a region where names are visible.
#[derive(Debug, Clone)]
pub struct Scope {
  pub id: ScopeId,
  /// None for the root/global scope
  pub parent: Option<ScopeId>,
  pub kind: ScopeKind,
  pub source_file: SourceFileId,
  /// The definition that created this scope
  pub owner: Option<DefId>,
}

impl Scope {
  pub fn new(
    parent: Option<ScopeId>,
    kind: ScopeKind,
    source_file: SourceFileId,
    owner: Option<DefId>,
  ) -> Self {
    Self {
      id: ScopeId::new(),
      parent,
      kind,
      source_file,
      owner,
    }
  }

  pub fn root(source_file: SourceFileId) -> Self {
    Self::new(None, ScopeKind::Module, source_file, None)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
  /// The root scope of a module/file.
  Module,
  /// A function body scope.
  Function,
  /// A block scope (e.g., `{ ... }`).
  Block,
  /// A loop scope (for break/continue).
  Loop,
  /// A struct definition scope.
  Struct,
  /// An enum definition scope.
  Enum,
  /// A generic parameter scope.
  Generic,
}

#[derive(Debug, Default)]
pub struct Scopes {
  scopes: DashMap<ScopeId, Scope>,
  module_scopes: DashMap<SourceFileId, ScopeId>,
}

impl Scopes {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn create(
    &self,
    parent: Option<ScopeId>,
    kind: ScopeKind,
    source_file: SourceFileId,
    owner: Option<DefId>,
  ) -> ScopeId {
    let scope = Scope::new(parent, kind, source_file, owner);
    let id = scope.id;
    self.scopes.insert(id, scope);
    id
  }

  pub fn create_module_scope(&self, source_file: SourceFileId) -> ScopeId {
    let scope = Scope::root(source_file);
    let id = scope.id;
    self.scopes.insert(id, scope.clone());
    self.module_scopes.insert(source_file, id);
    id
  }

  pub fn get(&self, id: ScopeId) -> Option<Scope> {
    self.scopes.get(&id).map(|r| r.clone())
  }

  pub fn get_module_scope(&self, source_file: SourceFileId) -> Option<ScopeId> {
    self.module_scopes.get(&source_file).map(|r| *r)
  }

  pub fn parent(&self, id: ScopeId) -> Option<ScopeId> {
    let s = self.get(id)?;
    s.parent
  }

  pub fn ancestors(&self, id: ScopeId) -> ScopeAncestors<'_> {
    ScopeAncestors {
      scopes: self,
      current: Some(id),
    }
  }
}

pub struct ScopeAncestors<'a> {
  scopes: &'a Scopes,
  current: Option<ScopeId>,
}

impl Iterator for ScopeAncestors<'_> {
  type Item = ScopeId;

  fn next(&mut self) -> Option<Self::Item> {
    let current = self.current?;
    self.current = self.scopes.parent(current);
    Some(current)
  }
}
