use crate::{
    passes::{DeclarationCollection, MemoryAnalysis, NameResolution},
    prelude::*,
};
use zirael_codegen::{codegen::run_codegen, ir::lower_hir_to_ir};
use zirael_hir::hir::lowering::lower_ast_to_hir;
use zirael_type_checker::TypeInference;

#[derive(Debug, Clone)]
pub struct CompilationInfo {
    pub mode: Mode,
    pub root: PathBuf,
    pub name: String,
}

#[derive(Debug)]
pub struct CompilationUnit<'ctx> {
    pub entry_point: SourceFileId,
    pub context: Context<'ctx>,
    pub module_graph: DependencyGraph,
    pub info: CompilationInfo,
}

impl<'ctx> CompilationUnit<'ctx> {
    pub fn new(entry_point: SourceFileId, context: Context<'ctx>, info: CompilationInfo) -> Self {
        Self { entry_point, context, module_graph: Default::default(), info }
    }

    pub fn compile(&mut self) -> Result<()> {
        let reports = self.context.reports();
        let sources = self.context.sources();
        let symbols = self.context.symbols();

        let mut result = determine_lexed_modules(self.entry_point, sources, reports);
        self.module_graph = result.dependency_graph;

        DeclarationCollection::new(symbols, reports, sources).collect(&mut result.modules);
        NameResolution::new(symbols, reports, sources).walk_modules(&mut result.modules);
        reports.print(sources);

        MemoryAnalysis::new(symbols, reports, sources).walk_modules(&mut result.modules);
        TypeInference::new(symbols, reports, sources).walk_modules(&mut result.modules);
        reports.print(sources);

        let mut hir = lower_ast_to_hir(&mut result.modules, symbols, reports, sources);
        // TODO: optimizations on HIR

        let ir = lower_hir_to_ir(
            &mut hir,
            symbols,
            reports,
            sources,
            self.info.mode,
            self.info.root.clone(),
        );
        reports.print(sources);

        let order = symbols.build_symbol_relations()?;
        run_codegen(ir, self.info.name.clone(), order)
    }
}
