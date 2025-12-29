use crate::ast::expressions::Expr;
use crate::ast::identifier::Ident;
use crate::ast::types::Type;
use crate::ast::NodeId;
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
  For(ForStmt),
  While(WhileStmt),
  Loop(LoopStmt),
  Return(ReturnStmt),
  Expr(ExprStmt),
  Block(Block),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
  pub id: NodeId,
  pub is_mut: bool,
  pub name: Ident,
  pub ty: Option<Type>,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
  pub id: NodeId,
  pub name: Ident,
  pub ty: Option<Type>,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
  pub id: NodeId,
  pub binding: Ident,
  pub iterator: Expr,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
  pub id: NodeId,
  pub condition: Expr,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LoopStmt {
  pub id: NodeId,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
  pub id: NodeId,
  pub value: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
  pub id: NodeId,
  pub expr: Expr,
  pub has_semicolon: bool,
  pub span: Span,
}
