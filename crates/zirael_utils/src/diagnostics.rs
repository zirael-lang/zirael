use crate::arena::{ArenaExt, ArenaId, GenArena};
use crate::prelude::{SourceFileId, Sources};
use crate::span::Span;
use anyhow::Result;
use ariadne::{Cache, Color, Report, ReportKind, Source};
use dashmap::DashSet;
use generational_arena::{Arena, Index};
use parking_lot::RwLock;
use std::fmt::{Debug, Display};
use std::io::stderr;
use std::sync::Arc;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct DiagnosticId(pub Index);

impl ArenaId for DiagnosticId {
  fn new(index: Index) -> Self {
    DiagnosticId(index)
  }

  fn index(&self) -> Index {
    self.0
  }
}

/// Diagnostic when dropped and not consumed by either cancel or emit panics.
#[derive(Clone, Debug)]
pub struct Diagnostic {
  id: DiagnosticId,
  diag: Option<Box<Diag>>,
}

#[derive(Clone, Debug)]
pub struct Diag {
  pub message: String,
  pub level: DiagnosticLevel,
  pub labels: Vec<Label>,
  pub notes: Vec<String>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Label {
  pub span: Span,
  pub message: String,
  pub level: DiagnosticLevel,

  // Optional because span already contains the file id, but this is useful for overwriting the file.
  pub file: Option<SourceFileId>,
}

impl Label {
  pub fn new(message: impl Into<String>, span: Span, level: DiagnosticLevel) -> Self {
    Label { message: message.into(), span, file: None, level }
  }

  pub fn new_with_file(
    message: impl Into<String>,
    span: Span,
    level: DiagnosticLevel,
    file_id: SourceFileId,
  ) -> Self {
    Label { message: message.into(), span, file: Some(file_id), level }
  }

  pub fn file(&self) -> SourceFileId {
    self.file.unwrap_or(self.span.file_id)
  }

  pub fn ariadne_label(&self) -> ariadne::Label<Span> {
    ariadne::Label::new(self.span).with_message(self.message.clone()).with_color(self.level.color())
  }
}

#[derive(Debug, Copy, PartialEq, Eq, Clone, Hash)]
pub enum DiagnosticLevel {
  /// Error that prevents the compilation from continuing
  Error,

  /// Warning that doesn't affect the compilation.
  Warning,

  /// This is a bug in the compiler
  Bug,
}

impl DiagnosticLevel {
  pub fn severity<'a>(&self) -> ReportKind<'a> {
    ReportKind::Custom(
      match self {
        DiagnosticLevel::Error => "error",
        DiagnosticLevel::Warning => "warn",
        DiagnosticLevel::Bug => "bug",
      },
      self.color(),
    )
  }

  pub fn color(&self) -> Color {
    match self {
      DiagnosticLevel::Error => Color::BrightRed,
      DiagnosticLevel::Warning => Color::BrightYellow,
      DiagnosticLevel::Bug => Color::Red,
    }
  }
}

impl Drop for Diagnostic {
  fn drop(&mut self) {
    if self.diag.is_some() {
      panic!("Diagnostic {:?} dropped but it wasn't emitted or cancelled", self.id)
    }
  }
}

#[derive(Debug, Default)]
pub struct DiagnosticCtx {
  arena: Arc<RwLock<Arena<Diagnostic>>>,
  emitted_diagnostics: Arc<DashSet<DiagnosticId>>,
  sources: Sources,
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

    let mut writer = stderr();
    let mut report =
      Report::build(diagnostic.level.severity(), diagnostic.span).with_message(&diagnostic.message);

    report = report.with_label(
      Label::new(diagnostic.message.clone(), diagnostic.span, diagnostic.level).ariadne_label(),
    );

    for label in &diagnostic.labels {
      report.add_label(label.ariadne_label());
    }

    for note in &diagnostic.notes {
      report.add_note(note);
    }

    let report = report.finish();
    self.write(|arena| {
      arena.get_mut(id.index()).take();
    });

    report.write(&self.sources, &mut writer).expect("");
  }
}

impl Cache<SourceFileId> for &Sources {
  type Storage = String;

  fn fetch(&mut self, id: &SourceFileId) -> Result<&Source<Self::Storage>, impl Debug> {
    if let Some(source_file) = self.get(*id) {
      let content = source_file.value().content().clone();
      Ok(Box::leak(Box::new(content)))
    } else {
      Err(Box::new(format!("Source not found: {:?}", id)))
    }
  }

  fn display<'b>(&self, id: &'b SourceFileId) -> Option<impl Display + 'b> {
    let path = self.get_unchecked(*id);
    Some(format!("{}", path.value().path().display()))
  }
}
