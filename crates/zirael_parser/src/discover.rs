use crate::{
    ModuleDiscoveryResult, ModuleId,
    ast::{Ast, ImportKind, ItemKind},
    parser::Parser,
};
use ariadne::ReportKind;
use petgraph::Graph;
use zirael_utils::prelude::*;

pub fn determine_lexed_modules<'a>(
    entrypoint: SourceFileId,
    sources: &Sources,
    reports: &Reports<'a>,
) -> ModuleDiscoveryResult {
    debug!("starting module discovery from entrypoint: {:?}", entrypoint);

    let mut all_modules = Vec::new();
    let mut graph = Graph::new();
    let mut module_to_node = HashMap::new();
    let processed = Mutex::new(HashSet::new());
    let mut current_wave = vec![ModuleId::File(entrypoint)];
    let mut wave_number = 1;

    let entrypoint_id = ModuleId::File(entrypoint);
    let entrypoint_node = graph.add_node(entrypoint_id.clone());
    module_to_node.insert(entrypoint_id, entrypoint_node);

    while !current_wave.is_empty() {
        let wave_results: Vec<_> = current_wave
            .into_par_iter()
            .filter_map(|module_id| {
                let file_id = module_id.as_file()?;

                {
                    let mut proc = processed.lock();
                    if proc.contains(&module_id) {
                        debug!("module {:?} already processed, skipping", module_id);
                        return None;
                    }
                    proc.insert(module_id.clone());
                }

                let mut parser = Parser::new(sources.get_unchecked(file_id));
                let result = parser.parse(file_id);

                let mut discovered_modules = Vec::new();

                for (path, span) in parser.discover_queue {
                    let contents = fs_err::read_to_string(path.clone());
                    let Ok(contents) = contents else {
                        let err = contents.unwrap_err();
                        debug!("failed to read dependency file: {}", path.display());
                        reports.add(
                            file_id,
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
                    let new_file_id = sources.add_owned(contents, Some(path.clone()));
                    debug!("discovered file dependency: {} -> {:?}", path.display(), new_file_id);
                    discovered_modules.push(ModuleId::File(new_file_id));
                }

                let external_deps = extract_external_dependencies(&result.ast);
                for ext_module in external_deps {
                    debug!("discovered external dependency: {:?}", ext_module);
                    discovered_modules.push(ModuleId::External(ext_module));
                }

                for report in parser.reports {
                    reports.add(file_id, report.clone());
                }

                Some((result, discovered_modules))
            })
            .collect();

        let mut next_wave = Vec::new();
        for (module, discovered) in wave_results {
            let current_id = &module.id;

            if !module_to_node.contains_key(current_id) {
                let node = graph.add_node(current_id.clone());
                module_to_node.insert(current_id.clone(), node);
            }

            let current_node = module_to_node[current_id];

            for dep_id in &discovered {
                let dep_node = *module_to_node
                    .entry(dep_id.clone())
                    .or_insert_with(|| graph.add_node(dep_id.clone()));

                graph.add_edge(current_node, dep_node, ());

                if dep_id.as_file().is_some() {
                    next_wave.push(dep_id.clone());
                }
            }

            all_modules.push(module);
        }

        next_wave.sort_unstable_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
        next_wave.dedup();

        debug!(
            "Wave {} completed. Processed {} modules, discovered {} new modules for next wave",
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

fn extract_external_dependencies(ast: &Ast) -> Vec<Vec<Identifier>> {
    let mut external_deps = Vec::new();

    for item in &ast.items {
        if let ItemKind::Import(ImportKind::ExternalModule(parts)) = &item.kind {
            external_deps.push(parts.clone());
        }
    }

    external_deps
}
