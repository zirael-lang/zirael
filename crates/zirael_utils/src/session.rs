use crate::project_config::ProjectConfig;
use dashmap::DashMap;
use parking_lot::Mutex;
use petgraph::graph::{DiGraph, NodeIndex};
use std::path::PathBuf;
use std::sync::Arc;
use zirael_diagnostics::{DiagnosticCtx, DiagnosticWriter};
use zirael_source::prelude::{SourceFileId, Sources};

pub struct ModuleGraph {
  graph: Mutex<DiGraph<SourceFileId, ()>>,
  map: DashMap<SourceFileId, NodeIndex>,
}

impl ModuleGraph {
  pub fn new() -> Self {
    Self {
      graph: Mutex::new(DiGraph::new()),
      map: DashMap::new(),
    }
  }

  pub fn ensure_node(&self, id: SourceFileId) -> NodeIndex {
    *self.map.entry(id).or_insert_with(|| {
      let mut graph = self.graph.lock();
      graph.add_node(id)
    })
  }

  /// from is the module that declares the mod item and to is the module that got resolved from that item
  pub fn add_relation(&self, from: SourceFileId, to: SourceFileId) {
    let a = self.ensure_node(from);
    let b = self.ensure_node(to);
    self.graph.lock().add_edge(a, b, ());
  }
}

/// Struct that holds information about current package
pub struct Session {
  config: ProjectConfig,
  dcx: DiagnosticCtx,
  module_graph: ModuleGraph,

  is_test: bool,
}

impl Session {
  pub fn new(
    config: ProjectConfig,
    sources: Arc<Sources>,
    w: DiagnosticWriter,
    is_test: bool,
  ) -> Self {
    Self {
      dcx: DiagnosticCtx::new(
        sources.clone(),
        config.color,
        config.diagnostic_output_type.clone(),
        w,
      ),
      config,
      is_test,
      module_graph: ModuleGraph::new(),
    }
  }

  pub fn config(&self) -> &ProjectConfig {
    &self.config
  }

  pub fn dcx(&self) -> &DiagnosticCtx {
    &self.dcx
  }

  pub fn is_test(&self) -> bool {
    self.is_test
  }

  pub fn root(&self) -> PathBuf {
    self.config.root.clone()
  }

  pub fn graph(&self) -> &ModuleGraph {
    &self.module_graph
  }
}
