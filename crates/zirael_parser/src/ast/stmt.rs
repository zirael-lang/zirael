use crate::{
    AstId, SymbolId,
    ast::{expr::Expr, types::Type},
};
use zirael_utils::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt(pub StmtKind);

impl Stmt {
    pub fn kind(&self) -> &StmtKind {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Expr(Expr),
    Var(VarDecl),
    Return(Return),
    If(If),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub name: Identifier,
    /// Defaults to [`Type::Inferred`]
    pub ty: Type,
    pub value: Expr,
    pub span: Span,
    pub symbol_id: Option<SymbolId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Expr,
    pub then_branch: Vec<Stmt>,
    pub then_branch_id: AstId,
    pub else_branch: Option<ElseBranch>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElseBranch {
    Block(Vec<Stmt>, AstId),
    If(Box<If>),
}
