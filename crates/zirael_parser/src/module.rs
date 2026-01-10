use crate::ProgramNode;
use dashmap::DashMap;
use dashmap::iter::Iter;
use dashmap::mapref::one::Ref;
use zirael_source::prelude::SourceFileId;

/// This is what we get after parsing.
#[derive(Debug)]
pub struct Module {
  pub node: ProgramNode,
  pub source_file_id: SourceFileId,
}

impl Module {
  pub fn new(source_file_id: SourceFileId, node: ProgramNode) -> Self {
    Self {
      node,
      source_file_id,
    }
  }
}

#[derive(Debug, Default)]
pub struct Modules {
  pub sources: DashMap<SourceFileId, Module>,
}

impl Modules {
  pub fn new() -> Self {
    Self {
      sources: DashMap::new(),
    }
  }

  pub fn add(&self, module: Module) -> SourceFileId {
    let id = module.source_file_id.clone();
    self.sources.insert(id, module);
    id
  }

  pub fn get(&self, id: SourceFileId) -> Option<Ref<'_, SourceFileId, Module>> {
    self.sources.get(&id)
  }

  pub fn get_unchecked(
    &self,
    id: SourceFileId,
  ) -> Ref<'_, SourceFileId, Module> {
    self.sources.get(&id).unwrap()
  }

  pub fn all(&self) -> Iter<'_, SourceFileId, Module> {
    self.sources.iter()
  }
}
