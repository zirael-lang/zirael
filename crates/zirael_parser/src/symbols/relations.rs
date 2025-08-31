use crate::{MonomorphizationId, SymbolId};
use std::hash::RandomState;

use anyhow::{Result, anyhow};
use petgraph::{algo::toposort, graphmap::DiGraphMap};

#[derive(Debug, Clone)]
pub struct SymbolRelations {
  pub entries: Vec<SymbolRelationEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub enum SymbolRelationNode {
  Symbol(SymbolId),
  Monomorphization(MonomorphizationId),
}

#[derive(Debug, Clone)]
pub struct SymbolRelationEntry {
  /// symbol that uses the referred
  pub referer: SymbolRelationNode,
  /// symbol that is being referred
  pub referred: SymbolRelationNode,
}

impl SymbolRelationEntry {
  pub fn new(referer: SymbolRelationNode, referred: SymbolRelationNode) -> Self {
    Self { referer, referred }
  }
}

impl Default for SymbolRelations {
  fn default() -> Self {
    Self::new()
  }
}

impl SymbolRelations {
  pub fn new() -> Self {
    Self { entries: Vec::new() }
  }

  pub fn entry(&mut self, referer: SymbolRelationNode, referred: SymbolRelationNode) {
    self.entries.push(SymbolRelationEntry::new(referer, referred));
  }

  pub fn build_graph(&self) -> Result<Vec<SymbolRelationNode>> {
    let mut graph = DiGraphMap::<SymbolRelationNode, (), RandomState>::new();

    for entry in &self.entries {
      graph.add_node(entry.referer);
      graph.add_node(entry.referred);
      graph.add_edge(entry.referred, entry.referer, ());
    }

    match toposort(&graph, None) {
      Ok(order) => Ok(order),
      Err(cycle) => Err(anyhow!("Cyclic dependency detected involving {:?}", cycle.node_id())),
    }
  }
}
