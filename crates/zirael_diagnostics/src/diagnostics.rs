use std::fmt::Debug;
use yansi::Color;
use zirael_source::new_id;
use zirael_source::source_file::SourceFileId;
use zirael_source::span::Span;

use crate::{DiagnosticCode, ToDiagnostic};

new_id!(DiagnosticId);

/// Diagnostic when dropped and not consumed by either cancel or emit panics.
#[derive(Clone, Debug)]
pub struct Diagnostic {
  pub id: DiagnosticId,
  pub diag: Box<Diag>,
  pub(crate) emitted: bool,
  pub(crate) cancelled: bool,
}

impl ToDiagnostic for Diagnostic {
  fn to_diagnostic(&self) -> Diag {
    *self.diag.clone()
  }
}

#[derive(Clone, Debug, Default)]
pub struct Diag {
  pub message: String,
  pub level: DiagnosticLevel,
  pub labels: Vec<Label>,
  pub notes: Vec<String>,
  pub helps: Vec<String>,
  pub code: Option<DiagnosticCode>,
}

impl Diag {
  pub fn new(message: String, level: DiagnosticLevel) -> Self {
    Self {
      message,
      level,
      ..Default::default()
    }
  }
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
  pub fn new(
    message: impl Into<String>,
    span: Span,
    level: DiagnosticLevel,
  ) -> Self {
    Self {
      message: Some(message.into()),
      span,
      file: None,
      level,
      order: 0,
      priority: 0,
    }
  }

  pub fn new_with_file(
    message: impl Into<String>,
    span: Span,
    level: DiagnosticLevel,
    file_id: SourceFileId,
  ) -> Self {
    Self {
      message: Some(message.into()),
      span,
      file: Some(file_id),
      level,
      order: 0,
      priority: 0,
    }
  }

  pub fn file(&self) -> SourceFileId {
    self.file.unwrap_or(self.span.file_id)
  }

  pub fn color(&self) -> Option<Color> {
    Some(self.level.color())
  }
}

#[derive(Debug, Copy, PartialEq, Eq, Clone, Hash, Default)]
pub enum DiagnosticLevel {
  /// Error that prevents the compilation from continuing
  #[default]
  Error,

  /// Warning that doesn't affect the compilation.
  Warning,

  /// This is a bug in the compiler
  Bug,
}

impl DiagnosticLevel {
  pub fn name(&self) -> &'static str {
    match self {
      Self::Error => "error",
      Self::Warning => "warn",
      Self::Bug => "bug",
    }
  }

  pub fn color(&self) -> Color {
    match self {
      Self::Error => Color::BrightRed,
      Self::Warning => Color::BrightYellow,
      Self::Bug => Color::Red,
    }
  }
}

impl Drop for Diagnostic {
  fn drop(&mut self) {
    assert!(
      !(!self.emitted && !self.cancelled),
      "Diagnostic {:?} dropped but it wasn't emitted or cancelled",
      self.id
    );
  }
}
