use crate::prelude::*;
use std::process::exit;
use zirael_analysis::{TypeTable, typeck_hir};
use zirael_hir::hir::Hir;
use zirael_hir::lower::lower_to_hir;
use zirael_parser::module::{Module, Modules};
use zirael_parser::parser::errors::ModuleNotFound;
use zirael_parser::parser::parse;
use zirael_resolver::{ResolveVisitor, Resolver};
use zirael_source::source_file::SourceFileId;

pub struct CompilationUnit<'ctx> {
  pub entry_point: SourceFileId,
  pub ctx: &'ctx Context<'ctx>,
  pub modules: Modules,
  pub resolver: Resolver,
  pub hir: Option<Hir>,
  pub typeck: Option<TypeTable>,
}

impl<'ctx> CompilationUnit<'ctx> {
  pub fn new(entry_point: SourceFileId, context: &'ctx Context<'ctx>) -> Self {
    Self {
      entry_point,
      ctx: context,
      modules: Modules::new(),
      resolver: Resolver::new(),
      hir: None,
      typeck: None,
    }
  }

  pub fn check(&mut self) {
    let Some(_entrypoint) = self.file_to_module(self.entry_point) else {
      return;
    };

    self.resolver.build_import_graph(&self.modules);
    self.resolve_names();

    self.lower_to_hir();
    self.typeck();
  }

  fn typeck(&mut self) {
    let Some(hir) = &self.hir else { return };
    let dcx = self.ctx.dcx();

    let table = typeck_hir(hir, dcx, &self.resolver);

    self.emit_errors();
    self.typeck = Some(table);
  }

  fn resolve_names(&mut self) {
    ResolveVisitor::resolve_modules(&self.resolver, &self.modules, self.ctx);
    self.emit_errors();
  }

  fn lower_to_hir(&mut self) {
    let dcx = self.ctx.dcx();
    let hir = lower_to_hir(&self.resolver, &self.modules, dcx);
    self.hir = Some(hir);
  }

  pub fn sess(&self) -> &Session {
    self.ctx.session
  }

  fn file_to_module(&mut self, id: SourceFileId) -> Option<SourceFileId> {
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

    let files_to_process: Vec<_> = {
      let this = self.modules.get_unchecked(id);
      let dcx = self.ctx.dcx();
      let mut files = Vec::new();

      for module in &this.node.discover_modules {
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
        self.sess().graph().add_discovered_relation(id, file_id);
        files.push(file_id);
      }

      files
    };

    for file_id in files_to_process {
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
