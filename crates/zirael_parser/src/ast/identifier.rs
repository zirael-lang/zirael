use std::fmt;
use zirael_utils::prelude::{Identifier, Span, get_or_intern, resolve as resolve_ident};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ident {
  pub ident: Identifier,
}

impl Ident {
  pub fn new(name: &str, span: Span) -> Self {
    Self { ident: get_or_intern(name, Some(span)) }
  }

  pub fn span(&self) -> &Span {
    self.ident.span()
  }

  pub fn text(&self) -> String {
    resolve_ident(&self.ident)
  }
}

impl fmt::Display for Ident {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.text())
  }
}
