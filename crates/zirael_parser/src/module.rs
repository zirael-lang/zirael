use crate::ProgramNode;
use zirael_source::prelude::SourceFileId;

/// This is what we get after parsing.
#[derive(Debug)]
pub struct Module {
  pub node: ProgramNode,
  pub source_file_id: SourceFileId,
}

impl Module {
  pub fn new(source_file_id: SourceFileId, node: ProgramNode) -> Self {
    Self { source_file_id, node }
  }
}
