use crate::def::{DefId, Definition};
use crate::import_order::ImportGraph;
use crate::ribs::Rib;
use crate::scope::{ScopeId, ScopeKind, Scopes};
use crate::symbol::SymbolTable;
use dashmap::DashMap;
use parking_lot::RwLock;
use zirael_parser::module::Modules;
use zirael_parser::{NodeId, Path};
use zirael_source::prelude::SourceFileId;

/// The main resolver structure.
/// This struct contains all shared state for name resolution.
#[derive(Debug)]
pub struct Resolver {
  /// All scopes in the program
  pub scopes: Scopes,
  /// The symbol table mapping names to definitions
  pub symbols: SymbolTable,
  /// All definitions in the program
  pub definitions: DashMap<DefId, Definition>,
  /// The import dependency graph
  pub import_graph: ImportGraph,
  /// Path to SourceFileId mapping
  pub path_to_files: DashMap<NodeId, SourceFileId>,
  /// Per-file exported symbols for cross-module resolution
  pub module_exports_values: DashMap<SourceFileId, DashMap<String, DefId>>,
  /// Per-file exported symbols for cross-module resolution
  pub module_exports_types: DashMap<SourceFileId, DashMap<String, DefId>>,
  /// Per-DefId exported symbols for inline modules
  pub inline_module_exports_values: DashMap<DefId, DashMap<String, DefId>>,
  /// Per-DefId exported symbols for inline modules
  pub inline_module_exports_types: DashMap<DefId, DashMap<String, DefId>>,
  module_ribs: DashMap<SourceFileId, RwLock<(ScopeId, Rib)>>,
}

impl Resolver {
  pub fn new() -> Self {
    Self {
      scopes: Scopes::new(),
      symbols: SymbolTable::new(),
      definitions: DashMap::new(),
      import_graph: ImportGraph::new(),
      module_exports_values: DashMap::new(),
      module_exports_types: DashMap::new(),
      inline_module_exports_values: DashMap::new(),
      inline_module_exports_types: DashMap::new(),
      path_to_files: DashMap::new(),
      module_ribs: DashMap::new(),
    }
  }

  pub fn build_import_graph(&self, modules: &Modules) {
    for module_ref in modules.all() {
      let source_file = module_ref.source_file_id;
      self.import_graph.add_module(source_file);

      self
        .module_exports_values
        .entry(source_file)
        .or_insert_with(DashMap::new);
      self
        .module_exports_types
        .entry(source_file)
        .or_insert_with(DashMap::new);
    }
  }

  pub fn add_import_edge(&self, from: SourceFileId, to: SourceFileId) {
    self.import_graph.add_import(from, to);
  }

  pub fn add_definition(&self, def: Definition) -> DefId {
    let id = def.id;
    self.definitions.insert(id, def);
    id
  }

  pub fn get_definition(&self, id: DefId) -> Option<Definition> {
    self.definitions.get(&id).map(|r| r.clone())
  }

  pub fn create_scope(
    &self,
    parent: Option<ScopeId>,
    kind: ScopeKind,
    source_file: SourceFileId,
    owner: Option<DefId>,
  ) -> ScopeId {
    self.scopes.create(parent, kind, source_file, owner)
  }

  pub fn create_module_scope(&self, source_file: SourceFileId) -> ScopeId {
    self.scopes.create_module_scope(source_file)
  }

  pub fn parent_scope(&self, id: ScopeId) -> Option<ScopeId> {
    self.scopes.parent(id)
  }

  pub fn save_module_rib(
    &self,
    file: SourceFileId,
    scope_id: ScopeId,
    rib: Rib,
  ) {
    self.module_ribs.insert(file, RwLock::new((scope_id, rib)));
  }

  pub fn get_module_rib(&self, file: SourceFileId) -> Option<(ScopeId, Rib)> {
    self
      .module_ribs
      .get(&file)
      .map(|entry| entry.read().clone())
  }

  pub fn export_value(
    &self,
    module: SourceFileId,
    name: String,
    def_id: DefId,
  ) {
    self
      .module_exports_values
      .entry(module)
      .or_insert_with(DashMap::new)
      .insert(name, def_id);
  }

  pub fn export_type(&self, module: SourceFileId, name: String, def_id: DefId) {
    self
      .module_exports_types
      .entry(module)
      .or_insert_with(DashMap::new)
      .insert(name, def_id);
  }

  pub fn lookup_module_value(
    &self,
    module: SourceFileId,
    name: &str,
  ) -> Option<DefId> {
    let exports = self.module_exports_values.get(&module)?;
    exports.get(name).map(|r| *r)
  }

  pub fn lookup_module_type(
    &self,
    module: SourceFileId,
    name: &str,
  ) -> Option<DefId> {
    let exports = self.module_exports_types.get(&module)?;
    exports.get(name).map(|r| *r)
  }

  /// Export a value from an inline module (mod foo { ... })
  pub fn export_inline_value(
    &self,
    module_def: DefId,
    name: String,
    def_id: DefId,
  ) {
    self
      .inline_module_exports_values
      .entry(module_def)
      .or_insert_with(DashMap::new)
      .insert(name, def_id);
  }

  /// Export a type from an inline module (mod foo { ... })
  pub fn export_inline_type(
    &self,
    module_def: DefId,
    name: String,
    def_id: DefId,
  ) {
    self
      .inline_module_exports_types
      .entry(module_def)
      .or_insert_with(DashMap::new)
      .insert(name, def_id);
  }

  /// Look up a value in an inline module's exports
  pub fn lookup_inline_module_value(
    &self,
    module_def: DefId,
    name: &str,
  ) -> Option<DefId> {
    let exports = self.inline_module_exports_values.get(&module_def)?;
    exports.get(name).map(|r| *r)
  }

  /// Look up a type in an inline module's exports
  pub fn lookup_inline_module_type(
    &self,
    module_def: DefId,
    name: &str,
  ) -> Option<DefId> {
    let exports = self.inline_module_exports_types.get(&module_def)?;
    exports.get(name).map(|r| *r)
  }

  pub fn lookup_file_for_path(&self, path: &Path) -> SourceFileId {
    self
      .path_to_files
      .get(&path.id)
      .map(|f| *f)
      .expect("path should be already resolved")
  }

  pub fn add_path_mapping(&self, path: &Path, source_file_id: SourceFileId) {
    self.path_to_files.insert(path.id, source_file_id);
  }
}

impl Default for Resolver {
  fn default() -> Self {
    Self::new()
  }
}
