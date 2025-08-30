use crate::{
    AstId, Type,
    ast::{
        operator::{BinaryOp, UnaryOp},
        stmt::Stmt,
    },
    symbols::SymbolId,
};
use colored::Colorize as _;
use id_arena::Id;
use std::fmt::{self, Debug, Formatter};
use zirael_utils::prelude::*;

#[derive(Clone, PartialEq, Debug)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(Identifier, Option<SymbolId>),
    Binary { left: Box<Expr>, op: BinaryOp, right: Box<Expr> },
    Ternary { condition: Box<Expr>, true_expr: Box<Expr>, false_expr: Box<Expr> },
    Block(Vec<Stmt>),
    Assign(Box<Expr>, Box<Expr>),
    AssignOp(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(Box<UnaryOp>, Box<Expr>),
    Paren(Box<Expr>),
    Call { callee: Box<Expr>, args: Vec<Expr>, call_info: Option<CallInfo> },
    FieldAccess(Vec<Expr>),
    IndexAccess(Box<Expr>, Box<Expr>),
    // the last one in the chain is the method to call
    MethodCall { chain: Vec<Expr>, args: Vec<Expr>, call_info: Option<CallInfo> },
    StaticCall { callee: Box<Expr>, args: Vec<Expr>, call_info: Option<CallInfo> },
    CouldntParse(CouldntParse),
    StructInit { name: Box<Expr>, fields: HashMap<Identifier, Expr>, call_info: Option<CallInfo> },
}

pub type MonomorphizationId = Id<()>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallInfo {
    pub original_symbol: SymbolId,
    pub monomorphized_id: Option<MonomorphizationId>,
    pub concrete_types: HashMap<Identifier, Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
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
    pub id: AstId,
    pub kind: ExprKind,
    pub span: Span,
    pub ty: Type,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span, id: AstId) -> Self {
        Self { kind, span, id, ty: Type::Inferred }
    }

    pub fn as_identifier(&self) -> Option<(&Identifier, &Option<SymbolId>)> {
        match &self.kind {
            ExprKind::Identifier(ident, sym_id) => Some((ident, sym_id)),
            _ => None,
        }
    }

    pub fn as_identifier_mut(&mut self) -> Option<(&mut Identifier, &mut Option<SymbolId>)> {
        match &mut self.kind {
            ExprKind::Identifier(ident, sym_id) => Some((ident, sym_id)),
            _ => None,
        }
    }

    // unchecked only refers to the symbol id. useful when we are sure that symbol id was assigned
    pub fn as_identifier_unchecked(&self) -> Option<(&Identifier, SymbolId)> {
        match &self.kind {
            ExprKind::Identifier(ident, sym_id) => Some((ident, sym_id.unwrap())),
            _ => None,
        }
    }
}

impl ExprKind {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Literal(_) => "literal",
            Self::Identifier(_, _) => "identifier",
            Self::Binary { .. } => "binary expression",
            Self::Ternary { .. } => "ternary expression",
            Self::Block(_) => "block",
            Self::Assign(_, _) => "assign",
            Self::AssignOp(_, _, _) => "assign with operator",
            Self::Unary(_, _) => "unary",
            Self::Paren(_) => "parenthesized expression",
            Self::Call { .. } => "call",
            Self::FieldAccess(_) => "field access",
            Self::IndexAccess(_, _) => "index access",
            Self::MethodCall { .. } => "method call",
            Self::StaticCall { .. } => "static call",
            Self::StructInit { .. } => "struct constructor",
            Self::CouldntParse(_) => "couldnt parse",
        }
    }

    pub fn can_be_borrowed(&self) -> bool {
        matches!(self, Self::Identifier(_, _) | Self::FieldAccess(_) | Self::IndexAccess(_, _))
    }
}
