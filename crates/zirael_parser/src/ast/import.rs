use crate::ast::NodeId;
use crate::ast::identifier::Ident;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct ImportDecl {
  pub id: NodeId,
  pub path: Path,
  pub tail: Option<ImportTail>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportTail {
  Wildcard,
  Items(Vec<ImportSpec>),
}

#[derive(Debug, Clone)]
pub struct ImportSpec {
  pub id: NodeId,
  pub name: ImportName,
  pub alias: Option<Ident>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportName {
  SelfValue,
  Ident(Ident),
}

#[derive(Debug, Clone)]
pub struct Path {
  pub id: NodeId,
  pub root: Option<PathRoot>,
  pub segments: Vec<Ident>,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathRoot {
  /// Absolute from the package root
  Package,
  /// Relative from current directory
  SelfMod,
  /// Relative from the parent
  Super,
}
