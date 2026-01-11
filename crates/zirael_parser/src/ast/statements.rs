use crate::ast::NodeId;
use crate::ast::expressions::Expr;
use crate::ast::types::Type;
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Block {
  pub id: NodeId,
  pub statements: Vec<Statement>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
  VarDecl(VarDecl),
  ConstDecl(ConstDecl),
  Expr(ExprStmt),
  Block(Block),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
  pub id: NodeId,
  pub is_mut: bool,
  pub name: Identifier,
  pub ty: Option<Type>,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
  pub id: NodeId,
  pub name: Identifier,
  pub ty: Option<Type>,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
  pub id: NodeId,
  pub expr: Expr,
  pub has_semicolon: bool,
  pub span: Span,
}
