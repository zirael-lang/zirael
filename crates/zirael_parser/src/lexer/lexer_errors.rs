use std::fmt;
use zirael_diagnostics::ToDiagnostic;
use zirael_diagnostics::prelude::{Diag, DiagnosticLevel, Label};
use zirael_utils::prelude::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct LexError {
  pub kind: LexErrorKind,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexErrorKind {
  // Literal errors
  UnterminatedString,
  UnterminatedChar,
  UnterminatedBlockComment,
  InvalidEscape { escape: String },
  InvalidByteValue { value: u32 },
  EmptyCharLiteral,
  MultiCharLiteral,

  // Numeric errors
  MissingDigitsAfterBase { base: String },
  InvalidDigitForBase { digit: char, base: String },
  MalformedExponent,

  // Input errors
  InvalidUtf8,
  UnexpectedBom,
  DisallowedCodePoint { code_point: char },
  NonAsciiWhitespace { code_point: char },

  // Identifier errors
  InvalidIdentifierStart { char: char },
  InvalidIdentifierContinue { char: char },
  JoinControlInIdentifier,

  // Doc comment errors
  UnattachedDocComment,

  // Generic
  UnexpectedCharacter { char: char },
}

impl LexError {
  pub fn new(kind: LexErrorKind, span: Span) -> Self {
    LexError { kind, span }
  }

  pub fn message(&self) -> String {
    match &self.kind {
      LexErrorKind::UnterminatedString => "unterminated string literal".to_string(),
      LexErrorKind::UnterminatedChar => "unterminated character literal".to_string(),
      LexErrorKind::UnterminatedBlockComment => "unterminated block comment".to_string(),
      LexErrorKind::InvalidEscape { escape } => {
        format!("invalid escape sequence '{}'", escape)
      }
      LexErrorKind::InvalidByteValue { value } => {
        format!("byte value {} is out of range (must be 0-255)", value)
      }
      LexErrorKind::EmptyCharLiteral => "empty character literal".to_string(),
      LexErrorKind::MultiCharLiteral => {
        "character literal contains multiple characters".to_string()
      }
      LexErrorKind::MissingDigitsAfterBase { base } => {
        format!("missing digits after '{}' prefix", base)
      }
      LexErrorKind::InvalidDigitForBase { digit, base } => {
        format!("invalid digit '{}' for {} literal", digit, base)
      }
      LexErrorKind::MalformedExponent => "exponent requires at least one digit".to_string(),
      LexErrorKind::InvalidUtf8 => "invalid UTF-8 encoding".to_string(),
      LexErrorKind::UnexpectedBom => {
        "unexpected BOM (U+FEFF) - only allowed at file start".to_string()
      }
      LexErrorKind::DisallowedCodePoint { code_point } => {
        format!("disallowed code point U+{:04X}", *code_point as u32)
      }
      LexErrorKind::NonAsciiWhitespace { code_point } => {
        format!("non-ASCII whitespace U+{:04X} not allowed", *code_point as u32)
      }
      LexErrorKind::InvalidIdentifierStart { char } => {
        format!("invalid character '{}' at start of identifier", char)
      }
      LexErrorKind::InvalidIdentifierContinue { char } => {
        format!("invalid character '{}' in identifier", char)
      }
      LexErrorKind::JoinControlInIdentifier => {
        "zero-width joiner/non-joiner not allowed in identifiers".to_string()
      }
      LexErrorKind::UnattachedDocComment => {
        "documentation comment not attached to any item".to_string()
      }
      LexErrorKind::UnexpectedCharacter { char } => {
        format!("unexpected character '{}'", char)
      }
    }
  }

  pub fn label(&self) -> String {
    match &self.kind {
      LexErrorKind::UnterminatedString => "string started here".to_string(),
      LexErrorKind::UnterminatedChar => "character literal started here".to_string(),
      LexErrorKind::UnterminatedBlockComment => "comment started here".to_string(),
      LexErrorKind::InvalidEscape { .. } => "invalid escape".to_string(),
      LexErrorKind::InvalidByteValue { .. } => "byte value too large".to_string(),
      LexErrorKind::UnexpectedBom => "BOM not allowed here".to_string(),
      _ => "error here".to_string(),
    }
  }

  pub fn help(&self) -> Option<String> {
    match &self.kind {
      LexErrorKind::InvalidEscape { escape } if escape.starts_with("\\u") => {
        Some("Unicode escapes must have exactly 4 hex digits: \\uXXXX".to_string())
      }
      LexErrorKind::InvalidEscape { escape } if escape.starts_with("\\U") => {
        Some("Unicode escapes must have exactly 8 hex digits: \\UXXXXXXXX".to_string())
      }
      LexErrorKind::InvalidEscape { escape } if escape.starts_with("\\x") => {
        Some("Hex escapes must have exactly 2 hex digits: \\xXX".to_string())
      }
      LexErrorKind::UnterminatedChar => {
        Some("add closing quote (') to terminate character literal".to_string())
      }
      LexErrorKind::UnterminatedBlockComment => {
        Some("add closing */ to terminate block comment".to_string())
      }
      LexErrorKind::MissingDigitsAfterBase { base } => {
        Some(format!("add at least one digit after '{}'", base))
      }
      _ => None,
    }
  }
}

impl fmt::Display for LexError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.message())
  }
}

impl std::error::Error for LexError {}

impl ToDiagnostic for LexError {
  fn to_diagnostic(&self) -> Diag {
    let mut helps = Vec::new();
    if let Some(help) = self.help() {
      helps.push(help);
    }

    Diag {
      message: self.message(),
      level: DiagnosticLevel::Error,
      labels: vec![Label::new(self.label(), self.span, DiagnosticLevel::Error)],
      notes: Vec::new(),
      helps,
      // TODO: come up with a good idea for handling error codes
      code: None,
    }
  }
}

pub type LexResult<T> = Result<T, LexError>;
