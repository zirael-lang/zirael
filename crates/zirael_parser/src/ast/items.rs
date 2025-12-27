use crate::ast::Attribute;
use crate::ast::expressions::Expr;
use crate::ast::generics::GenericParams;
use crate::ast::identifier::Ident;
use crate::ast::params::Param;
use crate::ast::statements::Block;
use crate::ast::types::{Type, TypePath};
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Item {
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub kind: ItemKind,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
  Public,
  Private,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
  Const(ConstItem),
  Function(FunctionItem),
  Struct(StructItem),
  Enum(EnumItem),
  Interface(InterfaceItem),
  Impl(ImplItem),
}

#[derive(Debug, Clone)]
pub struct ConstItem {
  pub name: Ident,
  pub ty: Type,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionItem {
  pub is_const: bool,
  pub name: Ident,
  pub generics: Option<GenericParams>,
  pub params: Vec<Param>,
  pub return_type: Option<Type>,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructItem {
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
  pub attributes: Vec<Attribute>,
  pub visibility: Visibility,
  pub name: Ident,
  pub ty: Type,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MethodItem {
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
  pub name: Ident,
  pub generics: Option<GenericParams>,
  pub variants: Vec<Variant>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
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
pub struct InterfaceItem {
  pub name: Ident,
  pub generics: Option<GenericParams>,
  pub members: Vec<InterfaceMember>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum InterfaceMember {
  Method(InterfaceMethod),
  AssociatedType(AssociatedType),
}

#[derive(Debug, Clone)]
pub struct InterfaceMethod {
  pub name: Ident,
  pub params: Vec<Param>,
  pub return_type: Option<Type>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AssociatedType {
  pub name: Ident,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ImplItem {
  pub generics: Option<GenericParams>,
  pub interface_ref: Option<TypePath>,
  pub target_type: Type,
  pub members: Vec<ImplMember>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImplMember {
  Method(InterfaceMethodImpl),
  AssociatedType(AssociatedTypeImpl),
  InherentMethod(MethodItem),
}

#[derive(Debug, Clone)]
pub struct InterfaceMethodImpl {
  pub name: Ident,
  pub params: Vec<Param>,
  pub return_type: Option<Type>,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AssociatedTypeImpl {
  pub name: Ident,
  pub ty: Type,
  pub span: Span,
}
