use crate::hir::expr::HirExpr;
use std::collections::HashMap;
use zirael_parser::{AstId, Attribute, Attributes, EnumVariantData, StructField, SymbolId, Type};
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
  pub attrs: Attributes,
}

#[derive(Debug, Clone)]
pub enum HirItemKind {
  Function(HirFunction),
  Struct(HirStruct),
  Enum(HirEnum),
  TypeExtension(HirTypeExtension),
}

#[derive(Debug, Clone)]
pub struct HirTypeExtension {
  pub id: AstId,
  pub symbol_id: SymbolId,
  pub methods: Vec<HirItem>,
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
  pub id: AstId,
  pub symbol_id: SymbolId,
  pub fields: Vec<StructField>,
  pub methods: Vec<HirItem>,
}

#[derive(Debug, Clone)]
pub struct HirEnum {
  pub id: AstId,
  pub symbol_id: SymbolId,
  pub variants: Vec<HirVariant>,
  pub methods: Vec<HirItem>,
}

#[derive(Debug, Clone)]
pub struct HirVariant {
  pub symbol_id: SymbolId,
  pub data: EnumVariantData,
}

#[derive(PartialEq, Eq)]
pub enum ExprContext {
  Stmt,
  Expr,
}
