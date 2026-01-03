use crate::TokenType;
use std::fmt;
use std::fmt::{Display, Formatter};
use zirael_diagnostic_macro::Diagnostic;
use zirael_source::span::Span;

#[derive(Diagnostic)]
#[error("{expected} {context}, but found {found}")]
#[code(PARSE_UNEXPECTED_TOKEN)]
pub struct UnexpectedToken {
  pub expected: ExpectedTokens,
  pub found: TokenType,
  pub context: String,

  #[error("found here")]
  pub span: Span,
}

#[derive(Debug)]
pub struct ExpectedTokens(Vec<TokenType>);

impl ExpectedTokens {
  pub fn one(token: TokenType) -> Self {
    Self(vec![token])
  }

  pub fn many(tokens: Vec<TokenType>) -> Self {
    Self(tokens)
  }
}

impl Display for ExpectedTokens {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self.0.as_slice() {
      [t] => write!(f, "expected {t}"),
      ts => {
        f.write_str("expected one of: ")?;
        for (i, t) in ts.iter().enumerate() {
          if i != 0 {
            f.write_str(", ")?;
          }
          write!(f, "{t}")?;
        }
        Ok(())
      }
    }
  }
}

#[derive(Diagnostic)]
#[error(
  "`mod` takes a path to the module (eg. path::to::module), not a string literal"
)]
#[code(PARSE_MOD_STRING_LIT)]
pub struct ModStringLit {
  #[error("invalid use here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected identifier found {found}")]
#[code(PARSE_MOD_EXPECTED_IDENTIFIER)]
pub struct ExpectedIdentifier {
  pub found: TokenType,
  #[error("expected identifier here")]
  pub span: Span,
}
