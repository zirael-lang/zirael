mod expr;
mod item;
pub mod keyword;
mod operator;
mod stmt;
mod types;

pub use expr::*;
pub use item::*;
pub use keyword::*;
pub use operator::*;
pub use stmt::*;
pub use types::*;

pub(crate) use crate::ast::item::Item;
use std::fmt::Debug;

#[derive(Clone, Debug)]
pub struct Ast {
    pub items: Vec<Item>,
}

impl Ast {
    pub fn new(items: Vec<Item>) -> Self {
        Self { items }
    }
}
