pub mod import;
pub mod items;
pub mod generics;
pub mod params;
pub mod types;
pub mod statements;
pub mod identifier;
pub mod expressions;

pub use expressions::*;
pub use generics::*;
pub use identifier::*;
pub use import::*;
pub use items::*;
pub use params::*;
pub use statements::*;
pub use types::*;
use zirael_utils::prelude::Span;
use crate::ast::expressions::Expr;
use crate::ast::identifier::Ident;
use crate::ast::import::ImportDecl;
use crate::ast::items::Item;

#[derive(Debug, Clone)]
pub struct Attribute {
    pub path: AttrPath,
    pub args: Option<Vec<AttrArg>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AttrPath {
    pub segments: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum AttrArg {
    Named { name: Ident, value: Expr },
    Positional(Expr),
}

#[derive(Debug, Clone)]
pub struct ProgramNode {
    pub attributes: Vec<Attribute>,
    pub imports: Vec<ImportDecl>,
    pub items: Vec<Item>,
    pub span: Span,
}
