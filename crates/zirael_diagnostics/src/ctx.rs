use crate::emitter::{Emitter, HumanReadableEmitter};
use crate::output_type::DiagnosticOutputType;
use crate::{Diag, Diagnostic, DiagnosticId, DiagnosticLevel};
use dashmap::DashSet;
use generational_arena::Arena;
use parking_lot::RwLock;
use std::fmt::{Debug, Display};
use std::io::stderr;
use std::sync::Arc;
use zirael_source::arena::{ArenaExt, ArenaId, GenArena};
use zirael_source::prelude::{Sources, Span};

#[derive(Debug)]
pub struct DiagnosticCtx {
  arena: Arc<RwLock<Arena<Diagnostic>>>,
  emitted_diagnostics: Arc<DashSet<DiagnosticId>>,
  emitter: Box<dyn Emitter>,
}

impl GenArena<Diagnostic> for Arena<Diagnostic> {
  fn arena(&self) -> &Arena<Diagnostic> {
    &self
  }
}

impl ArenaExt<Arena<Diagnostic>, Diagnostic, DiagnosticId> for DiagnosticCtx {
  fn lock(&self) -> &Arc<RwLock<Arena<Diagnostic>>> {
    &self.arena
  }
}

pub trait ToDiagnostic {
  fn to_diagnostic(&self) -> Diag;
}

impl DiagnosticCtx {
  pub fn new(
    sources: Arc<Sources>,
    color: bool,
    diagnostic_output_type: DiagnosticOutputType,
  ) -> Self {
    Self {
      arena: Default::default(),
      emitted_diagnostics: Arc::new(DashSet::new()),
      emitter: match diagnostic_output_type {
        DiagnosticOutputType::HumanReadable => Box::new(HumanReadableEmitter::new(sources, color)),
        _ => panic!("Other diagnostic emitter not supported yet"),
      },
    }
  }

  pub fn add(&self, diag: Diag) -> DiagnosticId {
    let id = {
      let mut arena = self.arena.write();
      arena.insert_with(|id| Diagnostic { id: DiagnosticId(id), diag: Some(Box::new(diag)) })
    };
    DiagnosticId::new(id)
  }

  pub fn emit(&self, diag: impl ToDiagnostic) {
    let diagnostic = diag.to_diagnostic();
    let id = self.add(diagnostic.clone());

    self.emit_diag(id);
    if let DiagnosticLevel::Bug = diagnostic.level {
      panic!("look at the emitted diagnostic")
    }
  }

  // actually emits the diagnostic to stderr
  fn emit_diag(&self, id: DiagnosticId) {
    let Some(diagnostic) = self.get(id) else {
      panic!("No diagnostic found for {:?}", id);
    };
    let Some(diagnostic) = &diagnostic.diag else { todo!() };

    self.emitter.emit_diagnostic(diagnostic).expect("TODO: panic message");
    self.write(|arena| {
      arena.get_mut(id.index()).take();
    });
  }
}
