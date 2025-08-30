mod drop;
mod lowering;
mod mangling;
mod monomorphization;

use std::collections::HashMap;
use zirael_parser::{BinaryOp, Literal, MonomorphizationId, SymbolId, Type, UnaryOp};

pub use lowering::*;
use zirael_hir::hir::HirVariant;

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
    pub mono_id: Option<MonomorphizationId>,
}

#[derive(Clone, Debug)]
pub enum IrItemKind {
    Function(IrFunction),
    Struct(IrStruct),
    TypeExtension(IrTypeExtension),
    Enum(IrEnum),
    EnumVariant(IrVariant),
}

#[derive(Clone, Debug)]
pub struct IrTypeExtension {
    pub methods: Vec<IrFunction>,
}

impl IrItemKind {
    pub fn is_type_def(&self) -> bool {
        matches!(self, IrItemKind::Struct(_))
    }
}

#[derive(Clone, Debug)]
pub struct IrFunction {
    pub name: String,
    pub parameters: Vec<IrParam>,
    pub return_type: Type,
    pub body: Option<IrBlock>,
    pub is_async: bool,
    pub is_const: bool,
    pub is_extern: bool,
    pub abi: Option<String>,
}

#[derive(Clone, Debug)]
pub struct IrStruct {
    pub name: String,
    pub fields: Vec<IrField>,
}

#[derive(Clone, Debug)]
pub struct IrEnum {
    pub variants: Vec<IrVariant>,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct IrVariant {
    pub symbol_id: SymbolId,
    pub name: String,
    pub data: IrVariantData,
}

#[derive(Clone, Debug)]
pub enum IrVariantData {
    Struct(Vec<IrField>),
    Unit,
}

#[derive(Clone, Debug)]
pub struct IrField {
    pub name: String,
    pub ty: Type,
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

    pub fn sym(sym: String) -> Self {
        IrExpr::new(Type::Inferred, IrExprKind::Symbol(sym))
    }
}

#[derive(Clone, Debug)]
pub enum IrExprKind {
    Symbol(String),
    Block(IrBlock),
    Literal(Literal),
    Call(String, Vec<IrExpr>),
    StructInit(String, HashMap<String, IrExpr>),
    Assign(Box<IrExpr>, Box<IrExpr>),
    Unary(UnaryOp, Box<IrExpr>),
    Binary(Box<IrExpr>, BinaryOp, Box<IrExpr>),
    CCall(String, Vec<IrExpr>),
    FieldAccess(Vec<IrExpr>),
    Ternary(Box<IrExpr>, Box<IrExpr>, Box<IrExpr>),
    // type provided to, for example, a sizeof call
    Type(Type),
}
