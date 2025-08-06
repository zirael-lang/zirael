use crate::{
    SymbolId,
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
