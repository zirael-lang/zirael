use lasso::{Spur, ThreadedRodeo};
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use std::fmt::{Debug, Formatter};

pub struct IdentTable {
    interner: ThreadedRodeo,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(Spur);

impl Default for IdentTable {
    fn default() -> Self {
        Self::new()
    }
}

impl IdentTable {
    pub fn new() -> Self {
        Self { interner: ThreadedRodeo::default() }
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

pub static GLOBAL_TABLE: Lazy<Mutex<IdentTable>> = Lazy::new(|| Mutex::new(IdentTable::new()));

#[inline]
pub fn get_or_intern(name: &str) -> Identifier {
    Identifier(GLOBAL_TABLE.lock().intern(name))
}

#[inline]
pub fn resolve(sym: &Identifier) -> String {
    GLOBAL_TABLE.lock().resolve(&sym.0).to_owned()
}

#[inline]
pub fn default_ident() -> Identifier {
    get_or_intern("__default_identifier__")
}
