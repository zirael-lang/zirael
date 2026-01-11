use crate::prelude::Span;
use lasso::{Spur, ThreadedRodeo};
use parking_lot::Mutex;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

pub struct IdentTable {
  interner: ThreadedRodeo,
}

#[derive(Clone, Copy, Eq)]
pub struct Identifier(Spur, Span, bool);

impl PartialEq for Identifier {
  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
  }
}

impl Hash for Identifier {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.0.hash(state);
  }
}

impl PartialOrd for Identifier {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.0.cmp(&other.0))
  }
}

impl Ord for Identifier {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.0.cmp(&other.0)
  }
}

impl Default for IdentTable {
  fn default() -> Self {
    Self::new()
  }
}

impl Identifier {
  pub fn span(&self) -> &Span {
    &self.1
  }

  pub fn new(text: &str, span: Span) -> Self {
    get_or_intern(text, Some(span), false)
  }

  pub fn text(&self) -> String {
    resolve(&self)
  }

  pub fn dummy() -> Self {
    get_or_intern("", None, true)
  }
}

impl IdentTable {
  pub fn new() -> Self {
    Self {
      interner: ThreadedRodeo::default(),
    }
  }

  pub fn intern(&self, name: &str) -> Spur {
    self.interner.get_or_intern(name)
  }

  pub fn resolve(&self, sym: &Spur) -> &str {
    self.interner.resolve(sym)
  }
}

impl Debug for Identifier {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", resolve(self))
  }
}

impl Display for Identifier {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", resolve(self))
  }
}

pub static GLOBAL_TABLE: std::sync::LazyLock<Mutex<IdentTable>> =
  std::sync::LazyLock::new(|| Mutex::new(IdentTable::new()));

#[inline]
pub fn get_or_intern(
  name: &str,
  span: Option<Span>,
  dummy: bool,
) -> Identifier {
  Identifier(
    GLOBAL_TABLE.lock().intern(name),
    span.unwrap_or_default(),
    dummy,
  )
}

#[inline]
pub fn resolve(sym: &Identifier) -> String {
  GLOBAL_TABLE.lock().resolve(&sym.0).to_owned()
}

#[inline]
pub fn default_ident() -> Identifier {
  get_or_intern("__default_identifier__", None, true)
}
