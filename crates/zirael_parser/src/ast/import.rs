use zirael_utils::prelude::Span;
use crate::ast::identifier::Ident;

#[derive(Debug, Clone)]
pub struct ImportDecl {
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
  pub root: Option<PathRoot>,
  pub segments: Vec<Ident>,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathRoot {
  Package,
  SelfMod,
  Super,
}
