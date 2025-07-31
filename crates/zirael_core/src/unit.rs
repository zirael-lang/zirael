use crate::{
    passes::{DeclarationCollection, NameResolution},
    prelude::*,
};

#[derive(Debug)]
pub struct CompilationUnit<'ctx> {
    pub entry_point: SourceFileId,
    pub context: Context<'ctx>,
    pub module_graph: DependencyGraph,
}

impl<'ctx> CompilationUnit<'ctx> {
    pub fn new(entry_point: SourceFileId, context: Context<'ctx>) -> Self {
        Self { entry_point, context, module_graph: Default::default() }
    }

    pub fn compile(&mut self) {
        let reports = self.context.reports();
        let sources = self.context.sources();
        let symbols = self.context.symbols();

        let mut result = determine_lexed_modules(self.entry_point, sources, reports);
        self.module_graph = result.dependency_graph;

        DeclarationCollection::new(symbols, reports, sources).collect(&mut result.modules);
        NameResolution::new(symbols, reports, sources).walk(&mut result.modules);

        reports.print(sources);
    }
}
