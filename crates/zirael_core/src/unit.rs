use crate::prelude::*;

#[derive(Debug)]
pub struct CompilationUnit<'ctx> {
  pub entry_point: SourceFileId,
  pub context: &'ctx Context<'ctx>,
}

impl<'ctx> CompilationUnit<'ctx> {
  pub fn new(entry_point: SourceFileId, context: &'ctx Context<'ctx>) -> Self {
    Self { entry_point, context }
  }

  pub fn check(&mut self) {}
}
