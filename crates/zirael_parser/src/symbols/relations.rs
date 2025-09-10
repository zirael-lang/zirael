use crate::{MonomorphizationId, SymbolId};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::hash::RandomState;

use anyhow::{Result, anyhow};
use petgraph::{Direction, algo::toposort, graphmap::DiGraphMap};

#[derive(Debug, Clone)]
pub struct SymbolRelations {
  pub entries: Vec<SymbolRelationEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub enum SymbolRelationNode {
  Symbol(SymbolId),
  Monomorphization(MonomorphizationId),
}

impl fmt::Display for SymbolRelationNode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      SymbolRelationNode::Symbol(id) => write!(f, "Symbol({:?})", id),
      SymbolRelationNode::Monomorphization(id) => write!(f, "Mono({:?})", id),
    }
  }
}

#[derive(Debug, Clone)]
pub struct SymbolRelationEntry {
  /// symbol that uses the referred
  pub referer: SymbolRelationNode,
  /// symbol that is being referred
  pub referred: SymbolRelationNode,
}

#[derive(Debug, Clone)]
pub struct CycleInfo {
  pub cycle_nodes: Vec<SymbolRelationNode>,
  pub all_edges: Vec<SymbolRelationEntry>,
}

impl CycleInfo {
  pub fn print_cycle_visualization(&self) {
    println!("\nCYCLIC DEPENDENCY DETECTED");
    println!("=====================================");

    self.print_cycle_chain();
    self.print_detailed_graph();
    self.print_ascii_diagram();
  }

  fn print_cycle_chain(&self) {
    println!("\nCycle Chain:");
    println!("---------------");

    if self.cycle_nodes.is_empty() {
      println!("  (No cycle nodes found)");
      return;
    }

    for (i, node) in self.cycle_nodes.iter().enumerate() {
      let arrow = if i == self.cycle_nodes.len() - 1 { "╰──→" } else { "├──→" };
      let connector = if i == 0 { "" } else { "│   " };

      println!("  {}{}  {}", connector, arrow, node);
    }

    if !self.cycle_nodes.is_empty() {
      println!("  ╰──→  {} (completes cycle)", self.cycle_nodes[0]);
    }
  }

  fn print_detailed_graph(&self) {
    println!("\nAll Dependencies in Cycle:");
    println!("-----------------------------");

    let cycle_set: HashSet<_> = self.cycle_nodes.iter().collect();

    for entry in &self.all_edges {
      let is_cycle_edge = cycle_set.contains(&entry.referer) && cycle_set.contains(&entry.referred);
      let marker = if is_cycle_edge { "🔴" } else { "🔵" };

      println!("  {} {} ──→ {}", marker, entry.referred, entry.referer);
    }

    println!("\n  Legend: 🔴 = Part of cycle  🔵 = Other dependency");
  }

  fn print_ascii_diagram(&self) {
    println!("\nASCII Dependency Graph:");
    println!("---------------------------");

    if self.cycle_nodes.len() < 2 {
      println!("  (Cycle too small to diagram)");
      return;
    }

    let cycle_len = self.cycle_nodes.len();

    match cycle_len {
      2 => self.print_two_node_cycle(),
      3 => self.print_three_node_cycle(),
      _ => self.print_multi_node_cycle(),
    }
  }

  fn print_two_node_cycle(&self) {
    if self.cycle_nodes.len() != 2 {
      return;
    }

    let a = &self.cycle_nodes[0];
    let b = &self.cycle_nodes[1];

    println!("  ┌─────────────────┐");
    println!("  │  {}  │", self.get_name(a));
    println!("  │       ↕         │");
    println!("  │  {}  │", self.get_name(b));
    println!("  └─────────────────┘");
  }

  fn print_three_node_cycle(&self) {
    if self.cycle_nodes.len() != 3 {
      return;
    }

    let nodes: Vec<_> = self.cycle_nodes.iter().map(|n| self.get_name(n)).collect();

    println!("        {}      ", nodes[0]);
    println!("       ╱ ╲      ");
    println!("      ╱   ╲     ");
    println!("     ╱     ╲    ");
    println!("    ╱       ╲   ");
    println!("   ╱         ╲  ");
    println!("  {}  ←───→  {}", nodes[1], nodes[2]);
  }

  fn print_multi_node_cycle(&self) {
    println!("  Multi-node cycle detected:");
    for (i, node) in self.cycle_nodes.iter().enumerate() {
      let next_i = (i + 1) % self.cycle_nodes.len();
      println!("    {} ──→ {}", self.get_name(node), self.get_name(&self.cycle_nodes[next_i]));
    }
  }

  fn get_name(&self, node: &SymbolRelationNode) -> String {
    match node {
      SymbolRelationNode::Symbol(sym) => format!("S{:?}", sym.index()),
      SymbolRelationNode::Monomorphization(mono) => format!("M{:?}", mono.index()),
    }
  }
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
      Err(cycle) => {
        let cycle_info = self.analyze_cycle(&graph, cycle.node_id());
        cycle_info.print_cycle_visualization();

        Err(anyhow!("Cyclic dependency detected involving {:?}", cycle.node_id()))
      }
    }
  }

  fn analyze_cycle(
    &self,
    graph: &DiGraphMap<SymbolRelationNode, (), RandomState>,
    cycle_node: SymbolRelationNode,
  ) -> CycleInfo {
    let cycle_nodes = self.find_cycle_nodes(graph, cycle_node);

    CycleInfo { cycle_nodes, all_edges: self.entries.clone() }
  }

  fn find_cycle_nodes(
    &self,
    graph: &DiGraphMap<SymbolRelationNode, (), RandomState>,
    start_node: SymbolRelationNode,
  ) -> Vec<SymbolRelationNode> {
    let mut visited = HashSet::new();
    let mut path = Vec::new();
    let mut cycle = Vec::new();

    self.dfs_find_cycle(graph, start_node, &mut visited, &mut path, &mut cycle);
    cycle
  }

  fn dfs_find_cycle(
    &self,
    graph: &DiGraphMap<SymbolRelationNode, (), RandomState>,
    node: SymbolRelationNode,
    visited: &mut HashSet<SymbolRelationNode>,
    path: &mut Vec<SymbolRelationNode>,
    cycle: &mut Vec<SymbolRelationNode>,
  ) -> bool {
    if let Some(pos) = path.iter().position(|&n| n == node) {
      cycle.extend_from_slice(&path[pos..]);
      cycle.push(node);
      return true;
    }

    if visited.contains(&node) {
      return false;
    }

    visited.insert(node);
    path.push(node);

    for neighbor in graph.neighbors_directed(node, Direction::Outgoing) {
      if self.dfs_find_cycle(graph, neighbor, visited, path, cycle) {
        return true;
      }
    }

    path.pop();
    false
  }

  pub fn build_graph_with_detailed_errors(&self) -> Result<Vec<SymbolRelationNode>> {
    let mut graph = DiGraphMap::<SymbolRelationNode, (), RandomState>::new();

    for entry in &self.entries {
      graph.add_node(entry.referer);
      graph.add_node(entry.referred);
      graph.add_edge(entry.referred, entry.referer, ());
    }

    match toposort(&graph, None) {
      Ok(order) => Ok(order),
      Err(cycle) => {
        println!("\n⚠️  DEPENDENCY ANALYSIS FAILED ⚠️");
        println!("===================================");

        let cycle_info = self.analyze_cycle(&graph, cycle.node_id());
        cycle_info.print_cycle_visualization();

        Err(anyhow!(
          "Cyclic dependency detected. See visualization above for details. Cycle involves: {:?}",
          cycle.node_id()
        ))
      }
    }
  }
}
