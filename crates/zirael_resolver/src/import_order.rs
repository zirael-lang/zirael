use parking_lot::RwLock;
use petgraph::algo::{has_path_connecting, toposort};
use petgraph::graph::{DiGraph, NodeIndex};
use std::collections::HashMap;
use zirael_diagnostic_macro::Diagnostic;
use zirael_source::prelude::{SourceFileId, Span};

/// Graph of import dependencies between modules.
#[derive(Debug)]
pub struct ImportGraph {
  inner: RwLock<ImportGraphInner>,
}

#[derive(Debug)]
struct ImportGraphInner {
  graph: DiGraph<SourceFileId, ()>,
  /// Maps source file IDs to their node indices in the graph.
  node_indices: HashMap<SourceFileId, NodeIndex>,
}

impl ImportGraph {
  pub fn new() -> Self {
    Self {
      inner: RwLock::new(ImportGraphInner {
        graph: DiGraph::new(),
        node_indices: HashMap::new(),
      }),
    }
  }

  pub fn add_module(&self, source_file: SourceFileId) -> NodeIndex {
    let mut inner = self.inner.write();

    if let Some(&idx) = inner.node_indices.get(&source_file) {
      return idx;
    }

    let idx = inner.graph.add_node(source_file);
    inner.node_indices.insert(source_file, idx);
    idx
  }

  /// `from` imports something from `to`.
  /// This means `to` must be resolved before `from`.
  pub fn add_import(&self, from: SourceFileId, to: SourceFileId) {
    let mut inner = self.inner.write();

    let from_idx = if let Some(&idx) = inner.node_indices.get(&from) {
      idx
    } else {
      let idx = inner.graph.add_node(from);
      inner.node_indices.insert(from, idx);
      idx
    };

    let to_idx = if let Some(&idx) = inner.node_indices.get(&to) {
      idx
    } else {
      let idx = inner.graph.add_node(to);
      inner.node_indices.insert(to, idx);
      idx
    };

    inner.graph.add_edge(to_idx, from_idx, ());
  }

  pub fn resolution_order(&self) -> Result<ResolutionOrder, ImportCycleError> {
    let inner = self.inner.read();

    match toposort(&inner.graph, None) {
      Ok(order) => {
        let files: Vec<SourceFileId> =
          order.into_iter().map(|idx| inner.graph[idx]).collect();
        Ok(ResolutionOrder { order: files })
      }
      Err(cycle) => {
        let cycle_node = inner.graph[cycle.node_id()];
        Err(ImportCycleError {
          cycle_member: cycle_node,
          span: Span::dummy(), // TODO: get real span
        })
      }
    }
  }

  pub fn would_create_cycle(
    &self,
    from: SourceFileId,
    to: SourceFileId,
  ) -> bool {
    let inner = self.inner.read();

    let Some(&from_idx) = inner.node_indices.get(&from) else {
      return false;
    };
    let Some(&to_idx) = inner.node_indices.get(&to) else {
      return false;
    };

    has_path_connecting(&inner.graph, from_idx, to_idx, None)
  }

  pub fn dependencies(&self, source_file: SourceFileId) -> Vec<SourceFileId> {
    let inner = self.inner.read();

    let Some(&idx) = inner.node_indices.get(&source_file) else {
      return Vec::new();
    };

    let mut deps = Vec::new();
    let mut visited = std::collections::HashSet::new();
    Self::collect_dependencies_inner(
      &inner.graph,
      idx,
      &mut deps,
      &mut visited,
    );
    deps
  }

  fn collect_dependencies_inner(
    graph: &DiGraph<SourceFileId, ()>,
    idx: NodeIndex,
    deps: &mut Vec<SourceFileId>,
    visited: &mut std::collections::HashSet<NodeIndex>,
  ) {
    for neighbor in graph.neighbors_directed(idx, petgraph::Direction::Incoming)
    {
      if visited.insert(neighbor) {
        deps.push(graph[neighbor]);
        Self::collect_dependencies_inner(graph, neighbor, deps, visited);
      }
    }
  }

  pub fn module_count(&self) -> usize {
    self.inner.read().graph.node_count()
  }

  pub fn is_empty(&self) -> bool {
    self.inner.read().graph.node_count() == 0
  }
}

impl Default for ImportGraph {
  fn default() -> Self {
    Self::new()
  }
}

#[derive(Debug, Clone)]
pub struct ResolutionOrder {
  order: Vec<SourceFileId>,
}

impl ResolutionOrder {
  pub fn empty() -> Self {
    Self { order: Vec::new() }
  }

  pub fn iter(&self) -> impl Iterator<Item = &SourceFileId> {
    self.order.iter()
  }

  pub fn len(&self) -> usize {
    self.order.len()
  }

  pub fn is_empty(&self) -> bool {
    self.order.is_empty()
  }
}

impl IntoIterator for ResolutionOrder {
  type Item = SourceFileId;
  type IntoIter = std::vec::IntoIter<SourceFileId>;

  fn into_iter(self) -> Self::IntoIter {
    self.order.into_iter()
  }
}

impl<'a> IntoIterator for &'a ResolutionOrder {
  type Item = &'a SourceFileId;
  type IntoIter = std::slice::Iter<'a, SourceFileId>;

  fn into_iter(self) -> Self::IntoIter {
    self.order.iter()
  }
}

// TODO: make this diagnostic more useful
#[derive(Diagnostic)]
#[error("cyclic import detected involving module {cycle_member:?}")]
pub struct ImportCycleError {
  pub cycle_member: SourceFileId,
  #[error("while resolving this import")]
  pub span: Span,
}
