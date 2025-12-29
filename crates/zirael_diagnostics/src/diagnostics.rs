use generational_arena::{Arena, Index};
use std::fmt::{Debug, Display};
use std::sync::atomic::{AtomicUsize, Ordering};
use yansi::Color;
use zirael_source::arena::source_file::SourceFileId;
use zirael_source::arena::{ArenaId, GenArena};
use zirael_source::new_id;
use zirael_source::span::Span;

use crate::DiagnosticCode;

new_id!(DiagnosticId);

/// Diagnostic when dropped and not consumed by either cancel or emit panics.
#[derive(Clone, Debug)]
pub struct Diagnostic {
  pub id: DiagnosticId,
  pub diag: Option<Box<Diag>>,
}

#[derive(Clone, Debug)]
pub struct Diag {
  pub message: String,
  pub level: DiagnosticLevel,
  pub labels: Vec<Label>,
  pub notes: Vec<String>,
  pub helps: Vec<String>,
  pub code: Option<DiagnosticCode>,
}

#[derive(Clone, Debug)]
pub struct Label {
  pub span: Span,
  pub message: Option<String>,
  pub level: DiagnosticLevel,
  pub order: i32,
  pub priority: i32,

  // Optional because span already contains the file id, but this is useful for overwriting the file.
  pub file: Option<SourceFileId>,
}

impl Label {
  pub fn new(message: impl Into<String>, span: Span, level: DiagnosticLevel) -> Self {
    Label { message: Some(message.into()), span, file: None, level, order: 0, priority: 0 }
  }

  pub fn new_with_file(
    message: impl Into<String>,
    span: Span,
    level: DiagnosticLevel,
    file_id: SourceFileId,
  ) -> Self {
    Label { message: Some(message.into()), span, file: Some(file_id), level, order: 0, priority: 0 }
  }

  pub fn file(&self) -> SourceFileId {
    self.file.unwrap_or(self.span.file_id)
  }

  pub fn color(&self) -> Option<Color> {
    Some(self.level.color())
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
  pub fn name(&self) -> &'static str {
    match self {
      DiagnosticLevel::Error => "error",
      DiagnosticLevel::Warning => "warn",
      DiagnosticLevel::Bug => "bug",
    }
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
