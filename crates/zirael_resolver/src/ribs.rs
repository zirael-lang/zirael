use crate::def::DefId;
use crate::scope::ScopeId;
use std::collections::HashMap;

/// A rib contains bindings introduced in a particular context.
#[derive(Debug, Clone)]
pub struct Rib {
  pub scope_id: ScopeId,
  pub kind: RibKind,
  pub values: HashMap<String, DefId>,
  pub types: HashMap<String, DefId>,
}

impl Rib {
  pub fn new(scope_id: ScopeId, kind: RibKind) -> Self {
    Self {
      scope_id,
      kind,
      values: HashMap::new(),
      types: HashMap::new(),
    }
  }

  pub fn define_value(&mut self, name: String, def_id: DefId) {
    self.values.insert(name, def_id);
  }
  pub fn define_type(&mut self, name: String, def_id: DefId) {
    self.types.insert(name, def_id);
  }

  pub fn lookup_value(&self, name: &str) -> Option<DefId> {
    self.values.get(name).copied()
  }
  pub fn lookup_type(&self, name: &str) -> Option<DefId> {
    self.types.get(name).copied()
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RibKind {
  Normal,
  Module,
  Function,
  Closure,
  Const,
  TypeParam,
  Item,
}

impl RibKind {
  pub fn allows_value_lookup(&self) -> bool {
    match self {
      Self::Const => false,
      _ => true,
    }
  }
}
