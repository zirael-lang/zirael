use crate::def::DefId;
use crate::scope::ScopeId;
use dashmap::DashMap;
use zirael_parser::ast::NodeId;

#[derive(Debug, Clone)]
pub struct Symbol {
  pub name: String,
  pub def_id: DefId,
  pub kind: SymbolKind,
  /// The scope where this symbol is visible.
  pub scope_id: ScopeId,
}

impl Symbol {
  pub fn new(
    name: String,
    def_id: DefId,
    kind: SymbolKind,
    scope_id: ScopeId,
  ) -> Self {
    Self {
      name,
      def_id,
      kind,
      scope_id,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
  Type,
  Value,
  Module,
}

#[derive(Debug, Default)]
pub struct SymbolTable {
  /// Maps (scope, name) -> Symbol for value namespace.
  values: DashMap<(ScopeId, String), Symbol>,
  /// Maps (scope, name) -> Symbol for type namespace.
  types: DashMap<(ScopeId, String), Symbol>,
  /// Maps (scope, name) -> Symbol for module namespace.
  modules: DashMap<(ScopeId, String), Symbol>,
  /// Maps NodeId -> DefId for quick lookup of what a node resolved to.
  resolutions: DashMap<NodeId, DefId>,
}

impl SymbolTable {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn insert(&self, symbol: Symbol) {
    let key = (symbol.scope_id, symbol.name.clone());
    match symbol.kind {
      SymbolKind::Value => {
        self.values.insert(key, symbol);
      }
      SymbolKind::Type => {
        self.types.insert(key, symbol);
      }
      SymbolKind::Module => {
        self.modules.insert(key, symbol);
      }
    }
  }

  pub fn lookup_value(&self, scope: ScopeId, name: &str) -> Option<Symbol> {
    self
      .values
      .get(&(scope, name.to_string()))
      .map(|r| r.clone())
  }

  pub fn lookup_type(&self, scope: ScopeId, name: &str) -> Option<Symbol> {
    self
      .types
      .get(&(scope, name.to_string()))
      .map(|r| r.clone())
  }

  pub fn lookup_module(&self, scope: ScopeId, name: &str) -> Option<Symbol> {
    self
      .modules
      .get(&(scope, name.to_string()))
      .map(|r| r.clone())
  }

  pub fn has_value(&self, scope: ScopeId, name: &str) -> bool {
    self.values.contains_key(&(scope, name.to_string()))
  }

  pub fn has_type(&self, scope: ScopeId, name: &str) -> bool {
    self.types.contains_key(&(scope, name.to_string()))
  }

  pub fn has_module(&self, scope: ScopeId, name: &str) -> bool {
    self.modules.contains_key(&(scope, name.to_string()))
  }

  pub fn record_resolution(&self, node_id: NodeId, def_id: DefId) {
    self.resolutions.insert(node_id, def_id);
  }

  pub fn get_resolution(&self, node_id: NodeId) -> Option<DefId> {
    self.resolutions.get(&node_id).map(|r| *r)
  }
}
