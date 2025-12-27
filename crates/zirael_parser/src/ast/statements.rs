use crate::ast::expressions::Expr;
use crate::ast::identifier::Ident;
use crate::ast::types::Type;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Block {
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
  pub is_mut: bool,
  pub name: Ident,
  pub ty: Option<Type>,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
  pub name: Ident,
  pub ty: Option<Type>,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
  pub binding: Ident,
  pub iterator: Expr,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
  pub condition: Expr,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LoopStmt {
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
  pub value: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
  pub expr: Expr,
  pub has_semicolon: bool,
  pub span: Span,
}
