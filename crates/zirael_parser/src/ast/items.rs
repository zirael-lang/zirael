use crate::ast::Attribute;
use crate::ast::NodeId;
use crate::ast::expressions::Expr;
use crate::ast::generics::GenericParams;
use crate::ast::identifier::Ident;
use crate::ast::import::Path;
use crate::ast::params::Param;
use crate::ast::statements::Block;
use crate::ast::types::Type;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Item {
  pub id: NodeId,
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub kind: ItemKind,
  pub span: Span,
  pub doc_comments: Option<Vec<String>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
  Public(Span),
  Private,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
  Const(ConstItem),
  Function(FunctionItem),
  Struct(StructItem),
  Enum(EnumItem),
  Mod(ModItem),
}

#[derive(Debug, Clone)]
pub struct ModItem {
  pub id: NodeId,
  pub name: Ident,
  pub items: Vec<Item>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ConstItem {
  pub id: NodeId,
  pub name: Ident,
  pub ty: Type,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionItem {
  pub id: NodeId,
  pub is_const: bool,
  pub name: Ident,
  pub generics: Option<GenericParams>,
  pub params: Vec<Param>,
  pub return_type: Type,
  pub body: Option<Block>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructItem {
  pub id: NodeId,
  pub name: Ident,
  pub generics: Option<GenericParams>,
  pub members: Vec<StructMember>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StructMember {
  Field(StructField),
  Method(MethodItem),
}

#[derive(Debug, Clone)]
pub struct StructField {
  pub id: NodeId,
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: Ident,
  pub ty: Type,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodItem {
  pub id: NodeId,
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: Ident,
  pub params: Vec<Param>,
  pub return_type: Option<Type>,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumItem {
  pub id: NodeId,
  pub name: Ident,
  pub generics: Option<GenericParams>,
  pub variants: Vec<Variant>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
  pub id: NodeId,
  pub attributes: Vec<Attribute>,
  pub name: Ident,
  pub payload: Option<VariantPayload>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum VariantPayload {
  Tuple(Vec<VariantField>),
  Discriminant(Expr),
}

#[derive(Debug, Clone)]
pub enum VariantField {
  Named { name: Ident, ty: Type },
  Unnamed(Type),
}

#[derive(Debug, Clone)]
pub struct AssociatedType {
  pub id: NodeId,
  pub name: Ident,
  pub span: Span,
}
