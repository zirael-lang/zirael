mod drop;
mod lowering;
mod mangling;
mod monomorphization;

use zirael_hir::hir::{HirBody, HirFunctionSignature, HirParam};
use zirael_parser::{BinaryOp, Literal, Symbol, SymbolId, Type, UnaryOp};

pub use lowering::*;
use zirael_hir::hir::expr::HirExpr;

#[derive(Clone, Debug)]
pub struct IrModule {
    pub items: Vec<IrItem>,
    pub mono_items: Vec<IrItem>,
}

#[derive(Clone, Debug)]
pub struct IrItem {
    pub name: String,
    pub kind: IrItemKind,
    pub sym_id: SymbolId,
}

#[derive(Clone, Debug)]
pub enum IrItemKind {
    Function(IrFunction),
}

#[derive(Clone, Debug)]
pub struct IrFunction {
    pub parameters: Vec<IrParam>,
    pub return_type: Type,
    pub body: Option<IrBlock>,
    pub is_async: bool,
    pub is_const: bool,
    pub is_extern: bool,
    pub abi: Option<String>,
}

#[derive(Debug, Clone)]
pub struct IrParam {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct IrBlock {
    pub stmts: Vec<IrStmt>,
}

impl IrBlock {
    pub fn new(stmts: Vec<IrStmt>) -> Self {
        Self { stmts }
    }
}

#[derive(Clone, Debug)]
pub enum IrStmt {
    Var(String, IrExpr),
    Return(Option<IrExpr>),
    Expr(IrExpr),
}

#[derive(Clone, Debug)]
pub struct IrExpr {
    pub ty: Type,
    pub kind: IrExprKind,
}

impl IrExpr {
    pub fn new(ty: Type, kind: IrExprKind) -> Self {
        Self { ty, kind }
    }
}

#[derive(Clone, Debug)]
pub enum IrExprKind {
    Symbol(String),
    Block(IrBlock),
    Literal(Literal),
    Call(String, Vec<IrExpr>),
    Assign(Box<IrExpr>, Box<IrExpr>),
    Unary(UnaryOp, Box<IrExpr>),
    Binary(Box<IrExpr>, BinaryOp, Box<IrExpr>),
    CCall(String, Vec<IrExpr>),
    // type provided to, for example, a sizeof call
    Type(Type),
}
