use crate::prelude::*;
use std::process::exit;
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
    let Some(entrypoint) = self.file_to_module(self.entry_point) else {
      return;
    };
  }

  fn file_to_module(&self, id: SourceFileId) -> Option<Module> {
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
    if self.emit_errors() {
      return None;
    };

    let node = parse(tokens, dcx);
    if self.emit_errors() {
      return None;
    };

    Some(Module::new(id, node.unwrap()))
  }

  fn emit_errors(&self) -> bool {
    let emitted = self.ctx.dcx().emit_all();
    if emitted > 0 {
      error!(
        "stopping compilation due to {emitted} {}",
        if emitted == 1 { "error" } else { "errors" }
      );
    }

    if self.ctx.session.is_test() && emitted > 0 {
      true
    } else if emitted > 0 {
      self.ctx.dcx().flush_to_stderr();
      exit(1)
    } else {
      false
    }
  }
}
