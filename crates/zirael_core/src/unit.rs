use crate::prelude::*;

#[derive(Debug)]
pub struct CompilationUnit<'ctx> {
    pub entry_point: SourceFileId,
    pub context: Context<'ctx>,
    pub modules_structure: ModuleDiscoveryResult,
}

impl<'ctx> CompilationUnit<'ctx> {
    pub fn new(entry_point: SourceFileId, context: Context<'ctx>) -> Self {
        Self { entry_point, context, modules_structure: Default::default() }
    }

    pub fn compile(&self) {
        let reports = self.context.reports();
        let sources = self.context.sources();
        let structure = determine_lexed_modules(self.entry_point, sources, reports);
        println!("{:#?}", structure.dependencies(self.entry_point));

        if reports.has_errors() {
            reports.print(sources);
        }
    }
}
