use crate::prelude::*;
use zirael_parser::parser::parse;
use zirael_source::source_file::SourceFileId;

pub struct CompilationUnit<'ctx> {
  pub entry_point: SourceFileId,
  pub ctx: &'ctx Context<'ctx>,
}

impl<'ctx> CompilationUnit<'ctx> {
  pub fn new(entry_point: SourceFileId, context: &'ctx Context<'ctx>) -> Self {
    Self {
      entry_point,
      ctx: context,
    }
  }

  pub fn check(&mut self) {
    let _ = self.file_to_module(self.entry_point);
  }

  fn file_to_module(&self, id: SourceFileId) -> Module {
    let dcx = self.ctx.dcx();

    let source_file = self.ctx.sources.get(id).unwrap_or_else(|| {
      dcx.bug(format!("Source file {id:?} not found"));
      unreachable!();
    });

    let mut lexer = Lexer::new(source_file.value(), dcx);
    let tokens = lexer.tokenize().unwrap_or_else(|| {
      dcx.bug(format!("lexer made no progress for source file {id:?}"));
      unreachable!();
    });
    dcx.emit_all();

    let node = parse(tokens, dcx);
    dcx.emit_all();

    Module::new(id, node.unwrap())
  }
}
