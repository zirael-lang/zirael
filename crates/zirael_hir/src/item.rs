use crate::Expr;
use crate::expr::Block;
use crate::generics::Generics;
use crate::ids::HirId;
use crate::ty::Ty;
use std::collections::HashMap;
use zirael_parser::ast::items::Visibility;
use zirael_resolver::DefId;
use zirael_utils::prelude::{Identifier, Span};

#[derive(Debug, Clone)]
pub struct Function {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub is_const: bool,
  pub generics: Generics,
  pub params: Vec<Param>,
  pub return_type: Ty,
  pub body: Option<Block>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub kind: ParamKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParamKind {
  /// Regular named parameter: `name: Type`
  Regular {
    name: Identifier,
    ty: Ty,
    default: Option<Expr>,
  },
  /// Self parameter: `self`, `mut self`, `*self`, `*mut self`
  SelfParam { kind: SelfKind },
  /// Variadic parameter: `...args: Type`
  Variadic { name: Identifier, ty: Ty },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelfKind {
  Value,
  Mut,
  Ptr,
  PtrMut,
}

#[derive(Debug, Clone)]
pub struct Struct {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub generics: Generics,
  pub fields: Vec<Field>,
  pub methods: Vec<Method>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
  pub hir_id: HirId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub ty: Ty,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Method {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub params: Vec<Param>,
  pub return_type: Option<Ty>,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Enum {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub generics: Generics,
  pub variants: Vec<Variant>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub kind: VariantKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum VariantKind {
  /// Unit variant: `None`
  Unit,
  /// Tuple variant: `Some(T)` or `Point(i32, i32)`
  Tuple(Vec<VariantField>),
  /// Struct variants: `Some { .value = 10 }`,
  Struct(HashMap<Identifier, VariantField>),
  /// Discriminant: `Value = 42`
  Discriminant(Expr),
}

#[derive(Debug, Clone)]
pub struct VariantField {
  pub hir_id: HirId,
  pub name: Option<Identifier>,
  pub ty: Ty,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Const {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub ty: Ty,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Module {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub visibility: Visibility,
  pub items: Vec<ItemId>,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(pub DefId);
