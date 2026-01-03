use crate::emitter::{Emitter, HumanReadableEmitter};
use crate::output_type::DiagnosticOutputType;
use crate::writer::Writer;
use crate::{Diag, Diagnostic, DiagnosticId, DiagnosticLevel};
use dashmap::mapref::one::{Ref, RefMut};
use dashmap::{DashMap, DashSet};
use derivative::Derivative;
use generational_arena::Arena;
use log::{debug, error};
use parking_lot::{Mutex, RwLock};
use std::fmt::{Debug, Display};
use std::io::{Cursor, Write, stderr};
use std::process::exit;
use std::sync::Arc;
use zirael_source::prelude::{Sources, Span};

#[derive(Derivative)]
#[derivative(Debug)]
pub struct DiagnosticCtx {
  pub diagnostics: Arc<DashMap<DiagnosticId, Diagnostic>>,
  emitted_diagnostics: Arc<DashSet<DiagnosticId>>,
  emitter: Box<dyn Emitter>,
  #[derivative(Debug = "ignore")]
  pub writer: DiagnosticWriter,
  stop_on_error: bool,
}
pub type DiagnosticWriter = Arc<Mutex<Cursor<Vec<u8>>>>;

pub trait ToDiagnostic {
  fn to_diagnostic(&self) -> Diag;
}

impl DiagnosticCtx {
  pub fn new(
    sources: Arc<Sources>,
    color: bool,
    diagnostic_output_type: DiagnosticOutputType,
    writer: DiagnosticWriter,
  ) -> Self {
    let emitter: Box<dyn Emitter> = match diagnostic_output_type {
      DiagnosticOutputType::HumanReadable => {
        Box::new(HumanReadableEmitter::new(sources.clone(), color))
      }
      DiagnosticOutputType::JSON => unimplemented!(),
    };

    Self {
      diagnostics: Default::default(),
      emitted_diagnostics: Arc::new(DashSet::new()),
      emitter,
      stop_on_error: true,
      writer,
    }
  }

  pub fn sources(&self) -> &Arc<Sources> {
    &self.emitter.sources()
  }

  pub fn add(&self, diag: Diag) -> DiagnosticId {
    let id = DiagnosticId::new();
    self.diagnostics.insert(
      id,
      Diagnostic {
        id,
        diag: Box::new(diag),
        cancelled: false,
        emitted: false,
      },
    );

    id
  }

  pub fn get(
    &self,
    id: DiagnosticId,
  ) -> Option<Ref<'_, DiagnosticId, Diagnostic>> {
    self.diagnostics.get(&id)
  }

  pub fn get_mut(
    &self,
    id: DiagnosticId,
  ) -> Option<RefMut<'_, DiagnosticId, Diagnostic>> {
    self.diagnostics.get_mut(&id)
  }

  pub fn cancel(&self, id: DiagnosticId) {
    if let Some(mut diag) = self.get_mut(id) {
      diag.cancelled = true;
    }
    debug!("cancelled diagnostic {:?}", id)
  }

  pub fn emit(&self, diag: impl ToDiagnostic) {
    let diagnostic = diag.to_diagnostic();
    let id = self.add(diagnostic.clone());

    if let DiagnosticLevel::Bug = diagnostic.level {
      self.emit_diag(id);
      panic!("look at the emitted diagnostic")
    }
  }

  pub fn bug(&self, msg: impl Into<String>) {
    self.emit(Diagnostic {
      id: DiagnosticId::new(),
      diag: Box::new(Diag::new(msg.into(), DiagnosticLevel::Bug)),
      cancelled: false,
      emitted: false,
    })
  }

  // actually emits the diagnostic to stderr
  fn emit_diag(&self, id: DiagnosticId) {
    if self.emitted_diagnostics.contains(&id) {
      return;
    }

    let diagnostic = {
      let Some(diagnostic) = self.get(id) else {
        panic!("No diagnostic found for {:?}", id);
      };

      // won't emit cancelled diagnostic
      if diagnostic.emitted || diagnostic.cancelled {
        return;
      }

      diagnostic.diag.clone()
    };

    let mut writer = self.writer.lock();
    self
      .emitter
      .emit_diagnostic(&*diagnostic, &mut *writer)
      .expect("TODO: panic message");

    if let Some(mut diag) = self.get_mut(id) {
      diag.emitted = true;
    }
  }

  pub fn emit_all(&self) {
    let ids: Vec<_> = self.diagnostics.iter().map(|d| d.id).collect();
    for id in ids {
      self.emit_diag(id);
    }
  }

  pub fn has_errors(&self) -> bool {
    self
      .diagnostics
      .iter()
      .any(|diag| diag.diag.level == DiagnosticLevel::Error)
  }
}
