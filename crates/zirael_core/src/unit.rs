use crate::prelude::*;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct CompilationUnit<'ctx> {
    pub entry_point: SourceFileId,
    pub context: Context<'ctx>,
    pub modules_structure: Vec<LexedModule>,
}

impl<'ctx> CompilationUnit<'ctx> {
    pub fn new(entry_point: SourceFileId, context: Context<'ctx>) -> Self {
        Self { entry_point, context, modules_structure: Vec::new() }
    }

    pub fn compile(&self) {
        let reports = self.context.reports();
        let structure =
            determine_lexed_modules(self.entry_point, self.context.sources(), reports.clone());

        println!("{:#?}", structure);
        if reports.has_errors() {
            reports.print(&self.context.sources())
        }
    }
}
