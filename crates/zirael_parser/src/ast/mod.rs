pub mod expressions;
pub mod generics;
pub mod import;
pub mod items;
pub mod params;
pub mod statements;
pub mod types;

use crate::ast::expressions::Expr;
use crate::ast::import::ImportDecl;
use crate::ast::items::Item;
pub use expressions::*;
pub use generics::*;
pub use import::*;
pub use items::*;
pub use params::*;
pub use statements::*;
pub use types::*;
use zirael_source::new_id;
use zirael_utils::prelude::{Identifier, Span};

new_id!(NodeId);

#[derive(Debug, Clone)]
pub struct Attribute {
  pub id: NodeId,
  pub path: AttrPath,
  pub args: Option<Vec<AttrArg>>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AttrPath {
  pub id: NodeId,
  pub segments: Vec<Identifier>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum AttrArg {
  Named { name: Identifier, value: Expr },
  Positional(Expr),
}

#[derive(Debug, Clone)]
pub struct ProgramNode {
  pub id: NodeId,
  pub attributes: Vec<Attribute>,
  pub imports: Vec<ImportDecl>,
  pub discover_modules: Vec<Path>,
  pub items: Vec<Item>,
}
