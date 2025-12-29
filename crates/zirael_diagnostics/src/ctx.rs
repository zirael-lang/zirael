use crate::emitter::{Emitter, HumanReadableEmitter};
use crate::output_type::DiagnosticOutputType;
use crate::{Diag, Diagnostic, DiagnosticId, DiagnosticLevel};
use dashmap::mapref::one::{Ref, RefMut};
use dashmap::{DashMap, DashSet};
use generational_arena::Arena;
use log::{debug, error};
use parking_lot::RwLock;
use std::fmt::{Debug, Display};
use std::io::stderr;
use std::process::exit;
use std::sync::Arc;
use zirael_source::arena::{ArenaExt, ArenaId, GenArena};
use zirael_source::prelude::{Sources, Span};

#[derive(Debug)]
pub struct DiagnosticCtx {
  diagnostics: Arc<DashMap<DiagnosticId, Diagnostic>>,
  emitted_diagnostics: Arc<DashSet<DiagnosticId>>,
  emitter: Box<dyn Emitter>,
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
      diagnostics: Default::default(),
      emitted_diagnostics: Arc::new(DashSet::new()),
      emitter: match diagnostic_output_type {
        DiagnosticOutputType::HumanReadable => Box::new(HumanReadableEmitter::new(sources, color)),
        _ => panic!("Other diagnostic emitter not supported yet"),
      },
    }
  }

  pub fn add(&self, diag: Diag) -> DiagnosticId {
    let id = DiagnosticId::new();
    self.diagnostics.insert(id, Diagnostic { id, diag: Some(Box::new(diag)) });

    id
  }

  pub fn get(&self, id: DiagnosticId) -> Option<Ref<'_, DiagnosticId, Diagnostic>> {
    self.diagnostics.get(&id)
  }

  pub fn get_mut(&self, id: DiagnosticId) -> Option<RefMut<'_, DiagnosticId, Diagnostic>> {
    self.diagnostics.get_mut(&id)
  }

  pub fn cancel(&self, id: DiagnosticId) {
    self.take(id);
    debug!("cancelled diagnostic {:?}", id)
  }

  /// Takes the value of .diag.
  fn take(&self, id: DiagnosticId) {
    if let Some(mut diag) = self.get_mut(id) {
      diag.diag.take();
    }

    debug_assert!(self.get(id).unwrap().diag.is_none())
  }

  pub fn emit(&self, diag: impl ToDiagnostic) {
    let diagnostic = diag.to_diagnostic();
    let id = self.add(diagnostic.clone());

    if let DiagnosticLevel::Bug = diagnostic.level {
      self.emit_diag(id, &mut 0);
      panic!("look at the emitted diagnostic")
    }
  }

  // actually emits the diagnostic to stderr
  fn emit_diag(&self, id: DiagnosticId, counter: &mut usize) {
    if self.emitted_diagnostics.contains(&id) {
      return;
    }

    let Some(diagnostic) = self.get(id) else {
      panic!("No diagnostic found for {:?}", id);
    };
    // won't emit cancelled diagnostic
    let Some(diagnostic) = &diagnostic.diag else { return };

    self.emitter.emit_diagnostic(diagnostic).expect("TODO: panic message");

    if diagnostic.level == DiagnosticLevel::Error {
      *counter += 1;
    }

    self.take(id);
  }

  pub fn emit_all(&self) {
    let counter = &mut 0;

    for diag in self.diagnostics.iter() {
      self.emit_diag(diag.id, counter);
    }
    debug!("emitted all diagnostics");

    if *counter > 0 {
      error!(
        "stoping due to {} {} emitted",
        counter,
        if *counter > 1 { "errors" } else { "error" }
      );
      exit(1);
    }
  }
}
