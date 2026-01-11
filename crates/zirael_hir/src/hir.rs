use crate::ids::HirId;
use crate::item::{Const, Enum, Function, Struct};

use dashmap::DashMap;
use dashmap::mapref::one::Ref;
use zirael_resolver::DefId;
use zirael_source::prelude::SourceFileId;

#[derive(Debug, Default)]
pub struct Hir {
  pub functions: DashMap<DefId, Function>,
  pub structs: DashMap<DefId, Struct>,
  pub enums: DashMap<DefId, Enum>,
  pub consts: DashMap<DefId, Const>,
  pub modules: DashMap<SourceFileId, Vec<DefId>>,
}

impl Hir {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn get_function(
    &self,
    def_id: DefId,
  ) -> Option<Ref<'_, DefId, Function>> {
    self.functions.get(&def_id)
  }

  pub fn get_struct(&self, def_id: DefId) -> Option<Ref<'_, DefId, Struct>> {
    self.structs.get(&def_id)
  }

  pub fn get_enum(&self, def_id: DefId) -> Option<Ref<'_, DefId, Enum>> {
    self.enums.get(&def_id)
  }

  pub fn get_const(&self, def_id: DefId) -> Option<Ref<'_, DefId, Const>> {
    self.consts.get(&def_id)
  }

  pub fn get_module_items(
    &self,
    file_id: SourceFileId,
  ) -> Option<Ref<'_, SourceFileId, Vec<DefId>>> {
    self.modules.get(&file_id)
  }
}

#[derive(Debug, Default)]
pub struct HirMap {
  nodes: DashMap<HirId, NodeData>,
  parents: DashMap<HirId, HirId>,
}

#[derive(Debug, Clone)]
pub enum NodeData {
  Function(DefId),
  Struct(DefId),
  Enum(DefId),
  Const(DefId),
  Nested { owner: DefId },
}

impl HirMap {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn insert(&self, hir_id: HirId, data: NodeData) {
    self.nodes.insert(hir_id, data);
  }

  pub fn set_parent(&self, child: HirId, parent: HirId) {
    self.parents.insert(child, parent);
  }

  pub fn parent(&self, id: HirId) -> Option<HirId> {
    self.parents.get(&id).map(|r| *r)
  }

  pub fn owner(&self, id: HirId) -> Option<DefId> {
    Some(id.owner)
  }

  pub fn get_node_data(&self, id: HirId) -> Option<NodeData> {
    self.nodes.get(&id).map(|r| r.clone())
  }
}
