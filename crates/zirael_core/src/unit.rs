use crate::{
  passes::{DeclarationCollection, MemoryAnalysis, NameResolution},
  prelude::*,
};
use zirael_codegen::run_codegen;
use zirael_hir::hir::HirModule;
use zirael_hir::hir::lowering::lower_ast_to_hir;
use zirael_type_checker::{MonoSymbolTable, TypeInference};

#[derive(Debug)]
pub struct CompilationUnit<'ctx> {
  pub entry_point: SourceFileId,
  pub context: Context<'ctx>,
  pub module_graph: DependencyGraph,
  pub info: CompilationInfo,
  pub main_function_id: Option<MainFunction>,
}

#[derive(Debug)]
pub struct CheckResult {
  pub hir: Vec<HirModule>,
  pub used_externals: HashSet<String>,
  pub symbol_table: MonoSymbolTable,
}

impl<'ctx> CompilationUnit<'ctx> {
  pub fn new(entry_point: SourceFileId, context: Context<'ctx>, info: CompilationInfo) -> Self {
    Self { entry_point, context, module_graph: Default::default(), info, main_function_id: None }
  }

  fn find_main_function(&self, symbols: &SymbolTable, reports: &Reports<'_>) -> Option<SymbolId> {
    let main_ident = get_or_intern("main", None);

    symbols.read(|table| {
      let entry_scope = table
        .scopes_arena
        .iter()
        .find(
          |(_, scope)| matches!(scope.scope_type, ScopeType::Module(id) if id == self.entry_point),
        )
        .map(|(id, _)| id);

      if let Some(scope_id) = entry_scope {
        if let Some(&symbol_id) = table.name_lookup.get(&(main_ident, scope_id)) {
          let symbol = &table.symbols[symbol_id];

          if let SymbolKind::Function { signature, .. } = &symbol.kind {
            if signature.parameters.is_empty() {
              return Some(symbol_id);
            } else {
              if let Some(span) = symbol.source_location.as_ref() {
                reports.add(
                  self.entry_point,
                  ReportBuilder::builder(
                    "main function must not take any parameters",
                    ReportKind::Error,
                  )
                  .label("invalid main function signature", *span),
                );
              }
              return None;
            }
          }
        }
      }

      reports.add(
        self.entry_point,
        ReportBuilder::builder("binary package must have a main function", ReportKind::Error)
          .label("add a main function to this module", Span::default()),
      );
      None
    })
  }

  pub fn check(&mut self) -> CheckResult {
    let reports = self.context.reports();
    let sources = self.context.sources();
    let symbols = self.context.symbols();

    let mut result = determine_lexed_modules(self.entry_point, sources, reports);
    self.module_graph = result.dependency_graph;

    let packages_ref = self.context.packages();
    let mut decl = DeclarationCollection::new_no_defaults(
      symbols,
      reports,
      sources,
      packages_ref.clone(),
      HashSet::new(),
      self.info.mode,
    );

    decl.collect(&mut result.modules);
    if self.info.ty == PackageType::Binary
      && let Some(main_id) = self.find_main_function(symbols, reports)
    {
      self.main_function_id = Some(MainFunction::Symbol(main_id));
    }

    NameResolution::new(symbols, reports, sources).walk_modules(&mut result.modules);
    reports.print(sources);

    MemoryAnalysis::new(symbols, reports, sources).walk_modules(&mut result.modules);
    let mut inference = TypeInference::new(symbols, reports, sources);
    inference.walk_modules(&mut result.modules);

    reports.print(sources);

    let hir = lower_ast_to_hir(
      &mut result.modules,
      &mut inference.sym_table,
      symbols,
      reports,
      self.info.ty == PackageType::Library,
      self.info.mode,
    );
    reports.print(sources);

    CheckResult { hir, used_externals: decl.used_externals, symbol_table: inference.sym_table }
  }

  pub fn compile(&mut self) -> Result<PathBuf> {
    let mut res = self.check();

    let order = self.context.symbols().build_symbol_relations()?;
    let output_path = run_codegen(
      res.hir,
      res.used_externals,
      &mut res.symbol_table,
      self.context.symbols(),
      self.context.sources(),
      order,
      &self.info,
    )?;

    Ok(output_path)
  }
}
