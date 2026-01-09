use std::fmt;
use zirael_diagnostics::ToDiagnostic;
use zirael_diagnostics::codes as diag_codes;
use zirael_diagnostics::prelude::{
  Diag, DiagnosticCode, DiagnosticLevel, Label,
};
use zirael_utils::prelude::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
  pub kind: LexErrorKind,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Self { kind, span }
  }

  pub fn code(&self) -> DiagnosticCode {
    match &self.kind {
      LexErrorKind::UnterminatedString => diag_codes::LEX_UNTERMINATED_STRING,
      LexErrorKind::UnterminatedChar => diag_codes::LEX_UNTERMINATED_CHAR,
      LexErrorKind::UnterminatedBlockComment => {
        diag_codes::LEX_UNTERMINATED_BLOCK_COMMENT
      }
      LexErrorKind::InvalidEscape { .. } => diag_codes::LEX_INVALID_ESCAPE,
      LexErrorKind::InvalidByteValue { .. } => {
        diag_codes::LEX_INVALID_BYTE_VALUE
      }
      LexErrorKind::EmptyCharLiteral => diag_codes::LEX_EMPTY_CHAR_LITERAL,
      LexErrorKind::MultiCharLiteral => diag_codes::LEX_MULTI_CHAR_LITERAL,
      LexErrorKind::MissingDigitsAfterBase { .. } => {
        diag_codes::LEX_MISSING_DIGITS_AFTER_BASE
      }
      LexErrorKind::InvalidDigitForBase { .. } => {
        diag_codes::LEX_INVALID_DIGIT_FOR_BASE
      }
      LexErrorKind::MalformedExponent => diag_codes::LEX_MALFORMED_EXPONENT,
      LexErrorKind::InvalidUtf8 => diag_codes::LEX_INVALID_UTF8,
      LexErrorKind::UnexpectedBom => diag_codes::LEX_UNEXPECTED_BOM,
      LexErrorKind::DisallowedCodePoint { .. } => {
        diag_codes::LEX_DISALLOWED_CODE_POINT
      }
      LexErrorKind::NonAsciiWhitespace { .. } => {
        diag_codes::LEX_NON_ASCII_WHITESPACE
      }
      LexErrorKind::InvalidIdentifierStart { .. } => {
        diag_codes::LEX_INVALID_IDENTIFIER_START
      }
      LexErrorKind::InvalidIdentifierContinue { .. } => {
        diag_codes::LEX_INVALID_IDENTIFIER_CONTINUE
      }
      LexErrorKind::JoinControlInIdentifier => {
        diag_codes::LEX_JOIN_CONTROL_IN_IDENTIFIER
      }
      LexErrorKind::UnattachedDocComment => {
        diag_codes::LEX_UNATTACHED_DOC_COMMENT
      }
      LexErrorKind::UnexpectedCharacter { .. } => {
        diag_codes::LEX_UNEXPECTED_CHARACTER
      }
    }
  }

  pub fn message(&self) -> String {
    match &self.kind {
      LexErrorKind::UnterminatedString => {
        "unterminated string literal".to_owned()
      }
      LexErrorKind::UnterminatedChar => {
        "unterminated character literal".to_owned()
      }
      LexErrorKind::UnterminatedBlockComment => {
        "unterminated block comment".to_owned()
      }
      LexErrorKind::InvalidEscape { escape } => {
        format!("invalid escape sequence '{escape}'")
      }
      LexErrorKind::InvalidByteValue { value } => {
        format!("byte value {value} is out of range (must be 0-255)")
      }
      LexErrorKind::EmptyCharLiteral => "empty character literal".to_owned(),
      LexErrorKind::MultiCharLiteral => {
        "character literal contains multiple characters".to_owned()
      }
      LexErrorKind::MissingDigitsAfterBase { base } => {
        format!("missing digits after '{base}' prefix")
      }
      LexErrorKind::InvalidDigitForBase { digit, base } => {
        format!("invalid digit '{digit}' for {base} literal")
      }
      LexErrorKind::MalformedExponent => {
        "exponent requires at least one digit".to_owned()
      }
      LexErrorKind::InvalidUtf8 => "invalid UTF-8 encoding".to_owned(),
      LexErrorKind::UnexpectedBom => {
        "unexpected BOM (U+FEFF) - only allowed at file start".to_owned()
      }
      LexErrorKind::DisallowedCodePoint { code_point } => {
        format!("disallowed code point U+{:04X}", *code_point as u32)
      }
      LexErrorKind::NonAsciiWhitespace { code_point } => {
        format!(
          "non-ASCII whitespace U+{:04X} not allowed",
          *code_point as u32
        )
      }
      LexErrorKind::InvalidIdentifierStart { char } => {
        format!("invalid character '{char}' at start of identifier")
      }
      LexErrorKind::InvalidIdentifierContinue { char } => {
        format!("invalid character '{char}' in identifier")
      }
      LexErrorKind::JoinControlInIdentifier => {
        "zero-width joiner/non-joiner not allowed in identifiers".to_owned()
      }
      LexErrorKind::UnattachedDocComment => {
        "documentation comment not attached to any item".to_owned()
      }
      LexErrorKind::UnexpectedCharacter { char } => {
        format!("unexpected character '{char}'")
      }
    }
  }

  pub fn label(&self) -> String {
    match &self.kind {
      LexErrorKind::UnterminatedString => "string started here".to_owned(),
      LexErrorKind::UnterminatedChar => {
        "character literal started here".to_owned()
      }
      LexErrorKind::UnterminatedBlockComment => {
        "comment started here".to_owned()
      }
      LexErrorKind::InvalidEscape { .. } => "invalid escape".to_owned(),
      LexErrorKind::InvalidByteValue { .. } => {
        "byte value too large".to_owned()
      }
      LexErrorKind::UnexpectedBom => "BOM not allowed here".to_owned(),
      _ => "error here".to_owned(),
    }
  }

  pub fn help(&self) -> Option<String> {
    match &self.kind {
      LexErrorKind::InvalidEscape { escape } if escape.starts_with("\\u") => {
        Some(
          "Unicode escapes must have exactly 4 hex digits: \\uXXXX".to_owned(),
        )
      }
      LexErrorKind::InvalidEscape { escape } if escape.starts_with("\\U") => {
        Some(
          "Unicode escapes must have exactly 8 hex digits: \\UXXXXXXXX"
            .to_owned(),
        )
      }
      LexErrorKind::InvalidEscape { escape } if escape.starts_with("\\x") => {
        Some("Hex escapes must have exactly 2 hex digits: \\xXX".to_owned())
      }
      LexErrorKind::UnterminatedChar => {
        Some("add closing quote (') to terminate character literal".to_owned())
      }
      LexErrorKind::UnterminatedBlockComment => {
        Some("add closing */ to terminate block comment".to_owned())
      }
      LexErrorKind::MissingDigitsAfterBase { base } => {
        Some(format!("add at least one digit after '{base}'"))
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
      code: Some(self.code()),
    }
  }
}

pub type LexResult<T> = Result<T, LexError>;
