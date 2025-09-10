use crate::{
  LexedModule, ModuleDiscoveryResult, ModuleId,
  ast::{Ast, ImportKind, ItemKind},
  parser::Parser,
};
use petgraph::Graph;
use std::path::Path;
use zirael_utils::prelude::*;

pub fn determine_lexed_modules<'a>(
  entrypoint: SourceFileId,
  sources: &Sources,
  reports: &Reports<'a>,
) -> ModuleDiscoveryResult {
  debug!("starting module discovery from entrypoint: {entrypoint:?}");

  let mut discovery = ModuleDiscovery::new(entrypoint);

  while discovery.has_pending_modules() {
    let wave_results = discovery.process_current_wave(sources, reports);
    discovery.process_wave_results(wave_results);
  }

  debug!("Module discovery completed. Total modules processed: {}", discovery.all_modules.len());

  discovery.into_result()
}

struct ModuleDiscovery {
  all_modules: Vec<LexedModule>,
  graph: Graph<ModuleId, ()>,
  module_to_node: HashMap<ModuleId, petgraph::graph::NodeIndex>,
  processed: Mutex<HashSet<ModuleId>>,
  current_wave: Vec<ModuleId>,
  wave_number: usize,
}

impl ModuleDiscovery {
  fn new(entrypoint: SourceFileId) -> Self {
    let mut graph = Graph::new();
    let mut module_to_node = HashMap::new();

    let entrypoint_id = ModuleId::File(entrypoint);
    let entrypoint_node = graph.add_node(entrypoint_id.clone());
    module_to_node.insert(entrypoint_id.clone(), entrypoint_node);

    Self {
      all_modules: Vec::new(),
      graph,
      module_to_node,
      processed: Mutex::new(HashSet::new()),
      current_wave: vec![entrypoint_id],
      wave_number: 1,
    }
  }

  fn has_pending_modules(&self) -> bool {
    !self.current_wave.is_empty()
  }

  fn process_current_wave<'a>(&self, sources: &Sources, reports: &Reports<'a>) -> Vec<WaveResult> {
    self
      .current_wave
      .clone()
      .into_par_iter()
      .filter_map(|module_id| self.process_single_module(module_id, sources, reports))
      .collect()
  }

  fn process_single_module<'a>(
    &self,
    module_id: ModuleId,
    sources: &Sources,
    reports: &Reports<'a>,
  ) -> Option<WaveResult> {
    let file_id = module_id.as_file()?;

    if self.is_already_processed(&module_id) {
      debug!("module {module_id:?} already processed, skipping");
      return None;
    }

    self.mark_as_processed(module_id.clone());

    let parse_result = self.parse_module(file_id, sources, reports)?;
    let discovered_modules = self.discover_dependencies(file_id, &parse_result, sources, reports);

    Some(WaveResult { module: parse_result.module, discovered_modules })
  }

  fn is_already_processed(&self, module_id: &ModuleId) -> bool {
    self.processed.lock().contains(module_id)
  }

  fn mark_as_processed(&self, module_id: ModuleId) {
    self.processed.lock().insert(module_id);
  }

  fn parse_module<'a>(
    &self,
    file_id: SourceFileId,
    sources: &Sources,
    reports: &Reports<'a>,
  ) -> Option<ParseResult> {
    let mut parser = Parser::new(sources.get_unchecked(file_id));
    let result = parser.parse(file_id);

    for report in parser.reports {
      reports.add(file_id, report.clone());
    }

    Some(ParseResult { module: result, discover_queue: parser.discover_queue })
  }

  fn discover_dependencies<'a>(
    &self,
    file_id: SourceFileId,
    parse_result: &ParseResult,
    sources: &Sources,
    reports: &Reports<'a>,
  ) -> Vec<ModuleId> {
    let mut discovered = Vec::new();

    for (path, span) in &parse_result.discover_queue {
      if let Some(module_id) = self.process_file_dependency(file_id, path, span, sources, reports) {
        discovered.push(module_id);
      }
    }

    let external_deps = extract_external_dependencies(&parse_result.module.ast);
    for ext_module in external_deps {
      debug!("discovered external dependency: {ext_module:?}");
      discovered.push(ModuleId::External(ext_module));
    }

    discovered
  }

  fn process_file_dependency<'a>(
    &self,
    file_id: SourceFileId,
    path: &Path,
    span: &Span,
    sources: &Sources,
    reports: &Reports<'a>,
  ) -> Option<ModuleId> {
    match fs_err::read_to_string(path) {
      Ok(contents) => {
        let new_file_id = sources.add_owned(contents, path.to_path_buf());
        debug!("discovered file dependency: {} -> {:?}", path.display(), new_file_id);
        Some(ModuleId::File(new_file_id))
      }
      Err(err) => {
        debug!("failed to read dependency file: {}", path.display());
        self.report_file_read_error(file_id, path, &err, span, reports);
        None
      }
    }
  }

  fn report_file_read_error<'a>(
    &self,
    file_id: SourceFileId,
    path: &Path,
    err: &std::io::Error,
    span: &Span,
    reports: &Reports<'a>,
  ) {
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
      .label("failed while resolving this file", *span),
    );
  }

  fn process_wave_results(&mut self, wave_results: Vec<WaveResult>) {
    let mut next_wave = Vec::new();

    for result in wave_results {
      self.add_module_to_graph(&result);
      self.all_modules.push(result.module);

      for dep_id in result.discovered_modules {
        if dep_id.as_file().is_some() {
          next_wave.push(dep_id);
        }
      }
    }

    self.prepare_next_wave(next_wave);
  }

  fn add_module_to_graph(&mut self, result: &WaveResult) {
    let current_id = &result.module.id;

    let current_node = self.get_or_create_node(current_id);

    for dep_id in &result.discovered_modules {
      let dep_node = self.get_or_create_node(dep_id);
      self.graph.add_edge(current_node, dep_node, ());
    }
  }

  fn get_or_create_node(&mut self, module_id: &ModuleId) -> petgraph::graph::NodeIndex {
    *self
      .module_to_node
      .entry(module_id.clone())
      .or_insert_with(|| self.graph.add_node(module_id.clone()))
  }

  fn prepare_next_wave(&mut self, mut next_wave: Vec<ModuleId>) {
    next_wave.sort_unstable_by(|a, b| format!("{a:?}").cmp(&format!("{b:?}")));
    next_wave.dedup();

    debug!(
      "Wave {} completed. Processed {} modules, discovered {} new modules for next wave",
      self.wave_number,
      self.all_modules.len(),
      next_wave.len()
    );

    self.current_wave = next_wave;
    self.wave_number += 1;
  }

  fn into_result(self) -> ModuleDiscoveryResult {
    ModuleDiscoveryResult { modules: self.all_modules, dependency_graph: self.graph }
  }
}

struct WaveResult {
  module: LexedModule,
  discovered_modules: Vec<ModuleId>,
}

struct ParseResult {
  module: LexedModule,
  discover_queue: Vec<(PathBuf, Span)>,
}

fn extract_external_dependencies(ast: &Ast) -> Vec<Vec<Identifier>> {
  ast
    .items
    .iter()
    .filter_map(|item| {
      if let ItemKind::Import(ImportKind::ExternalModule(parts), _) = &item.kind {
        Some(parts.clone())
      } else {
        None
      }
    })
    .collect()
}
