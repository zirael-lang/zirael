use crate::{
    SymbolId,
    ast::{
        operator::{BinaryOp, UnaryOp},
        stmt::Stmt,
    },
};
use colored::Colorize as _;
use std::fmt::{self, Debug, Formatter};
use zirael_utils::prelude::*;

#[derive(Clone, PartialEq, Debug)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(Identifier, Option<SymbolId>),
    Binary { left: Box<Expr>, op: BinaryOp, right: Box<Expr> },
    Block(Vec<Stmt>),
    Assign(Box<Expr>, Box<Expr>),
    AssignOp(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(Box<UnaryOp>, Box<Expr>),
    Paren(Box<Expr>),
    Call { callee: Box<Expr>, args: Vec<Expr> },
    FieldAccess(Vec<Expr>),
    CouldntParse(CouldntParse),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

impl ExprKind {
    pub fn couldnt_parse() -> Self {
        Self::CouldntParse(CouldntParse)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct CouldntParse;

impl Debug for CouldntParse {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", "CouldntParse".bright_red().bold().underline())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn as_identifier(&mut self) -> Option<(&mut Identifier, &mut Option<SymbolId>)> {
        match &mut self.kind {
            ExprKind::Identifier(ident, sym_id) => Some((ident, sym_id)),
            _ => None,
        }
    }
}
