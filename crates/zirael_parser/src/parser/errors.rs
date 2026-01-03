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

#[derive(Diagnostic)]
#[error("found `const` alone in a type")]
#[code(PARSE_CONST_ALONE_IN_A_TYPE)]
#[help("if you meant to type a const pointer add a star like this: *const <>")]
pub struct ConstAloneInType {
  #[error("alone `const` found here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected a function or an identifier after const, found {found}")]
#[code(PARSE_CONST_EXPECTED_FUNC_OR_IDENT)]
pub struct ConstExpectedFuncOrIdent {
  pub found: TokenType,
  #[error("unexpected part found here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("const items need type annotations")]
#[code(PARSE_CONST_NEED_TYPE_ANNOTATIONS)]
pub struct ConstItemsNeedTypeAnnotation {
  #[error("valid type expected for this item")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("const items cannot be uninitialized")]
#[code(PARSE_CONST_CANNOT_BE_UNINITIALIZED)]
pub struct ConstCannotBeUninitialized {
  #[error("valid expression expected for this item")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected a type")]
#[code(PARSE_EXPECTED_TYPE)]
pub struct ExpectedType {
  #[error("type expected here")]
  pub span: Span,
}
