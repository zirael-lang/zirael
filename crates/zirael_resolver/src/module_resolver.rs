use crate::def::DefId;
use crate::resolver::Resolver;
use crate::ribs::{Rib, RibKind};
use crate::scope::{ScopeId, ScopeKind};
use zirael_source::prelude::SourceFileId;

/// This holds the mutable state needed during resolution of a single module.
pub struct ModuleResolver<'a> {
  pub resolver: &'a Resolver,
  pub current_file: SourceFileId,
  rib_stack: Vec<Rib>,
  /// The current scope being resolved.
  current_scope: Option<ScopeId>,
}

impl<'a> ModuleResolver<'a> {
  pub fn new(resolver: &'a Resolver, source_file: SourceFileId) -> Self {
    Self {
      resolver,
      current_file: source_file,
      rib_stack: Vec::new(),
      current_scope: None,
    }
  }

  pub fn enter_scope(
    &mut self,
    kind: ScopeKind,
    owner: Option<DefId>,
  ) -> ScopeId {
    let parent = self.current_scope;

    // Use special module scope creation for Module kind
    let scope_id = if kind == ScopeKind::Module && parent.is_none() {
      self.resolver.create_module_scope(self.current_file)
    } else {
      self
        .resolver
        .create_scope(parent, kind, self.current_file, owner)
    };

    self.current_scope = Some(scope_id);

    let rib_kind = match kind {
      ScopeKind::Module => RibKind::Module,
      ScopeKind::Function => RibKind::Function,
      ScopeKind::Block | ScopeKind::Loop => RibKind::Normal,
      ScopeKind::Struct | ScopeKind::Enum => RibKind::Item,
      ScopeKind::Generic => RibKind::TypeParam,
    };
    self.rib_stack.push(Rib::new(scope_id, rib_kind));

    scope_id
  }

  pub fn leave_scope(&mut self) {
    if let Some(current) = self.current_scope {
      self.current_scope = self.resolver.parent_scope(current);
      self.rib_stack.pop();
    }
  }

  pub fn current_scope(&self) -> Option<ScopeId> {
    self.current_scope
  }

  pub fn define_value(&mut self, name: String, def_id: DefId) {
    if let Some(rib) = self.rib_stack.last_mut() {
      rib.define_value(name, def_id);
    }
  }

  pub fn define_type(&mut self, name: String, def_id: DefId) {
    if let Some(rib) = self.rib_stack.last_mut() {
      rib.define_type(name, def_id);
    }
  }

  pub fn lookup_value(&self, name: &str) -> Option<DefId> {
    for rib in self.rib_stack.iter().rev() {
      if !rib.kind.allows_value_lookup() {
        continue;
      }
      if let Some(def_id) = rib.lookup_value(name) {
        return Some(def_id);
      }
    }
    None
  }

  pub fn lookup_type(&self, name: &str) -> Option<DefId> {
    for rib in self.rib_stack.iter().rev() {
      if let Some(def_id) = rib.lookup_type(name) {
        return Some(def_id);
      }
    }
    None
  }

  pub fn is_at_module_scope(&self) -> bool {
    self.rib_stack.len() == 1
      && self
        .rib_stack
        .first()
        .is_some_and(|rib| rib.kind == RibKind::Module)
  }

  pub fn save_module_rib(&self) {
    if let Some(rib) = self.rib_stack.first() {
      if rib.kind == RibKind::Module {
        if let Some(scope_id) = self.current_scope {
          self.resolver.save_module_rib(
            self.current_file,
            scope_id,
            rib.clone(),
          );
        }
      }
    }
  }

  pub fn restore_module_rib(&mut self) -> bool {
    if let Some((scope_id, rib)) =
      self.resolver.get_module_rib(self.current_file)
    {
      self.current_scope = Some(scope_id);
      self.rib_stack.push(rib);
      true
    } else {
      false
    }
  }

  pub fn export_value(&self, name: String, def_id: DefId) {
    self.resolver.export_value(self.current_file, name, def_id);
  }

  pub fn export_type(&self, name: String, def_id: DefId) {
    self.resolver.export_type(self.current_file, name, def_id);
  }
}
