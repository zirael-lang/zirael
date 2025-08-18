use crate::ast::Ast;
use petgraph::{Directed, Graph, prelude::EdgeRef as _};
use zirael_utils::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ModuleId {
    File(SourceFileId),
    External(Vec<Identifier>),
}

impl ModuleId {
    pub fn as_file(&self) -> Option<SourceFileId> {
        match self {
            Self::File(id) => Some(*id),
            Self::External(_) => None,
        }
    }
}

impl From<SourceFileId> for ModuleId {
    fn from(val: SourceFileId) -> Self {
        Self::File(val)
    }
}

#[derive(Clone, Debug)]
pub struct LexedModule {
    pub id: ModuleId,
    pub ast: Ast,
}

impl LexedModule {
    pub fn new(id: ModuleId, ast: Ast) -> Self {
        Self { id, ast }
    }

    pub fn file(&self) -> Option<SourceFileId> {
        self.id.as_file()
    }
}

pub type DependencyGraph = Graph<ModuleId, (), Directed>;

#[derive(Debug, Default)]
pub struct ModuleDiscoveryResult {
    pub modules: Vec<LexedModule>,
    pub dependency_graph: DependencyGraph,
}

impl ModuleDiscoveryResult {
    pub fn dependencies(&self, module_id: &ModuleId) -> Vec<ModuleId> {
        if let Some(node_idx) = self
            .dependency_graph
            .node_indices()
            .find(|&idx| &self.dependency_graph[idx] == module_id)
        {
            self.dependency_graph
                .neighbors(node_idx)
                .map(|idx| self.dependency_graph[idx].clone())
                .collect()
        } else {
            Vec::new()
        }
    }

    pub fn dependents(&self, module_id: &ModuleId) -> Vec<ModuleId> {
        if let Some(node_idx) = self
            .dependency_graph
            .node_indices()
            .find(|&idx| &self.dependency_graph[idx] == module_id)
        {
            self.dependency_graph
                .neighbors_directed(node_idx, petgraph::Direction::Incoming)
                .map(|idx| self.dependency_graph[idx].clone())
                .collect()
        } else {
            Vec::new()
        }
    }
}
