use crate::{ast::Ast, parser::Parser};
use ariadne::ReportKind;
use petgraph::{Directed, Graph};
use std::collections::{HashMap, HashSet};
use zirael_utils::prelude::*;

#[derive(Clone, Debug)]
pub struct LexedModule {
    pub file: SourceFileId,
    pub ast: Ast,
}

impl LexedModule {
    pub fn new(file: SourceFileId, ast: Ast) -> Self {
        Self { file, ast }
    }
}

pub type DependencyGraph = Graph<SourceFileId, (), Directed>;

#[derive(Debug, Default)]
pub struct ModuleDiscoveryResult {
    pub modules: Vec<LexedModule>,
    pub dependency_graph: DependencyGraph,
}

pub fn determine_lexed_modules<'a>(
    entrypoint: SourceFileId,
    sources: &Sources,
    reports: &Reports<'a>,
) -> ModuleDiscoveryResult {
    debug!("starting module discovery from entrypoint: {:?}", entrypoint);

    let mut all_modules = Vec::new();
    let mut graph = Graph::new();
    let mut file_to_node = HashMap::new();
    let processed = Mutex::new(HashSet::new());
    let mut current_wave = vec![entrypoint];
    let mut wave_number = 1;

    // Add entrypoint to graph
    let entrypoint_node = graph.add_node(entrypoint);
    file_to_node.insert(entrypoint, entrypoint_node);

    while !current_wave.is_empty() {
        let wave_results: Vec<_> = current_wave
            .into_par_iter()
            .filter_map(|file| {
                {
                    let mut proc = processed.lock();
                    if proc.contains(&file) {
                        debug!("file {:?} already processed, skipping", file);
                        return None;
                    }
                    proc.insert(file);
                }
                let mut parser = Parser::new(sources.get_unchecked(file));
                let result = parser.parse(file);

                let mut discovered_files = Vec::new();
                for (path, span) in parser.discover_queue {
                    let contents = fs_err::read_to_string(path.clone());
                    let Ok(contents) = contents else {
                        let err = contents.unwrap_err();
                        debug!("failed to read dependency file: {}", path.display());
                        reports.add(
                            file,
                            ReportBuilder::builder(
                                format!(
                                    "Failed to read contents of {} while discovering dependencies: {}",
                                    path.display().to_string().dimmed(),
                                    err
                                ),
                                ReportKind::Error,
                            )
                                .label("failed while resolving this file", span),
                        );
                        continue;
                    };
                    let file_id = sources.add_owned(contents, Some(path.clone()));
                    debug!("discovered dependency: {} -> {:?}", path.display(), file_id);
                    discovered_files.push(file_id);
                }

                for report in parser.errors {
                    reports.add(file, report.clone());
                }

                Some((result, discovered_files))
            })
            .collect();

        let mut next_wave = Vec::new();
        for (module, discovered) in wave_results {
            let current_file = module.file;

            if !file_to_node.contains_key(&current_file) {
                let node = graph.add_node(current_file);
                file_to_node.insert(current_file, node);
            }

            let current_node = file_to_node[&current_file];

            for dep_file in &discovered {
                let dep_node =
                    *file_to_node.entry(*dep_file).or_insert_with(|| graph.add_node(*dep_file));

                graph.add_edge(current_node, dep_node, ());
            }

            all_modules.push(module);
            next_wave.extend(discovered);
        }

        next_wave.sort_unstable();
        next_wave.dedup();

        debug!(
            "Wave {} completed. Processed {} modules, discovered {} new files for next wave",
            wave_number,
            all_modules.len(),
            next_wave.len()
        );

        current_wave = next_wave;
        wave_number += 1;
    }

    debug!("Module discovery completed. Total modules processed: {}", all_modules.len());

    ModuleDiscoveryResult { modules: all_modules, dependency_graph: graph }
}

impl ModuleDiscoveryResult {
    pub fn dependencies(&self, file: SourceFileId) -> Vec<SourceFileId> {
        if let Some(node_idx) =
            self.dependency_graph.node_indices().find(|&idx| self.dependency_graph[idx] == file)
        {
            self.dependency_graph
                .neighbors(node_idx)
                .map(|idx| self.dependency_graph[idx])
                .collect()
        } else {
            Vec::new()
        }
    }

    pub fn dependents(&self, file: SourceFileId) -> Vec<SourceFileId> {
        if let Some(node_idx) =
            self.dependency_graph.node_indices().find(|&idx| self.dependency_graph[idx] == file)
        {
            self.dependency_graph
                .neighbors_directed(node_idx, petgraph::Direction::Incoming)
                .map(|idx| self.dependency_graph[idx])
                .collect()
        } else {
            Vec::new()
        }
    }
}
