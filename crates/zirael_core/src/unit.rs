use crate::prelude::*;
use std::process::exit;
use zirael_parser::module::{Module, Modules};
use zirael_parser::parser::errors::ModuleNotFound;
use zirael_parser::parser::parse;
use zirael_source::source_file::SourceFileId;

pub struct CompilationUnit<'ctx> {
  pub entry_point: SourceFileId,
  pub ctx: &'ctx Context<'ctx>,
  pub modules: Modules,
}

impl<'ctx> CompilationUnit<'ctx> {
  pub fn new(entry_point: SourceFileId, context: &'ctx Context<'ctx>) -> Self {
    Self {
      entry_point,
      ctx: context,
      modules: Modules::new(),
    }
  }

  pub fn check(&mut self) {
    let Some(entrypoint) = self.file_to_module(self.entry_point) else {
      return;
    };
  }

  pub fn sess(&self) -> &Session {
    &self.ctx.session
  }

  fn file_to_module(&self, id: SourceFileId) -> Option<SourceFileId> {
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
    }

    let node = parse(tokens, dcx);
    if self.emit_errors() {
      return None;
    }
    let node = node.expect("handle correctly");
    let module = Module::new(id, node);
    self.modules.add(module);

    for module in &self.modules.get_unchecked(id).node.discover_modules {
      let full_path =
        module.construct_file(self.ctx.session.root(), source_file.path());

      let Some(path) = full_path else {
        dcx.emit(ModuleNotFound {
          module: module.clone(),
          span: module.span,
        });

        continue;
      };

      if !path.exists() {
        dcx.emit(ModuleNotFound {
          module: module.clone(),
          span: module.span,
        });
        continue;
      }

      if self.ctx.sources.get_by_path(&path).is_some() {
        continue;
      }

      let contents = fs_err::read_to_string(path.clone());
      let Ok(contents) = contents else {
        dcx.bug(format!("Failed to read contents of {}", path.display()));
        continue;
      };

      let file_id = self.ctx.sources.add(contents, path);

      self.sess().graph().add_relation(id, file_id);
      let _ = self.file_to_module(file_id);
    }
    self.emit_errors();

    Some(id)
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
