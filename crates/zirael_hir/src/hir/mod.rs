use crate::hir::expr::{HirExpr, HirExprKind};
use id_arena::Id;
use std::collections::HashMap;
use zirael_parser::{AstId, SymbolId, Type};
use zirael_utils::prelude::{SourceFileId, Span};

pub mod expr;
pub mod lowering;

#[derive(Debug, Clone)]
pub struct HirModule {
    pub items: HashMap<SymbolId, HirItem>,
    pub id: SourceFileId,
}

#[derive(Debug, Clone)]
pub struct HirItem {
    pub symbol_id: SymbolId,
    pub kind: HirItemKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirItemKind {
    Function(HirFunction),
    Struct(HirStruct),
    Enum(HirEnum),
}

#[derive(Debug, Clone)]
pub struct HirFunction {
    pub id: AstId,
    pub symbol_id: SymbolId,
    pub signature: HirFunctionSignature,
    pub body: Option<HirBody>,
    pub is_async: bool,
    pub is_const: bool,
    pub is_extern: bool,
    pub abi: Option<String>,
}

#[derive(Debug, Clone)]
pub struct HirFunctionSignature {
    pub parameters: Vec<HirParam>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct HirParam {
    pub symbol_id: SymbolId,
    pub ty: Type,
    pub is_variadic: bool,
    pub default_value: Option<HirExpr>,
}

#[derive(Debug, Clone)]
pub struct HirBody {
    pub root_expr: HirExpr,
}

#[derive(Debug, Clone)]
pub struct HirStruct {
    pub symbol_id: SymbolId,
    pub fields: Vec<HirField>,
}

#[derive(Debug, Clone)]
pub struct HirField {
    pub symbol_id: SymbolId,
    pub ty: Type,
    pub is_public: bool,
}

#[derive(Debug, Clone)]
pub struct HirEnum {
    pub symbol_id: SymbolId,
    pub variants: Vec<HirVariant>,
}

#[derive(Debug, Clone)]
pub struct HirVariant {
    pub symbol_id: SymbolId,
    pub data: HirVariantData,
}

#[derive(Debug, Clone)]
pub enum HirVariantData {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<HirField>),
}

#[derive(PartialEq)]
pub enum ExprContext {
    Stmt,
    Expr,
}
