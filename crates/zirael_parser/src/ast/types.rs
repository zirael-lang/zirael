use crate::ast::NodeId;
use crate::ast::expressions::Expr;
use crate::ast::import::Path;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub enum Type {
  Primitive(PrimitiveType),
  Path(TypePath),
  Function(FunctionType),
  Pointer(PointerType),
  Optional(OptionalType),
  Array(ArrayType),
  Tuple(TupleType),
  Unit(UnitType),
  // most likely means that the parsing failed
  Invalid,
}

#[derive(Debug, Clone)]
pub struct UnitType {
  pub id: NodeId,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct PrimitiveType {
  pub id: NodeId,
  pub kind: PrimitiveKind,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveKind {
  // Signed integers
  I8,
  I16,
  I32,
  I64,
  I128,
  ISize,

  // Unsigned integers
  U8,
  U16,
  U32,
  U64,
  U128,
  USize,

  // Floating point
  F32,
  F64,

  // Other primitives
  Bool,
  Char,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
  Mut,
  Const,
}

#[derive(Debug, Clone)]
pub struct TypePath {
  pub id: NodeId,
  pub path: Path,
  pub args: Option<Vec<Type>>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
  pub id: NodeId,
  pub params: Vec<Type>,
  pub return_type: Box<Type>,
  pub span: Span,
  pub is_const: bool,
}

#[derive(Debug, Clone)]
pub struct PointerType {
  pub id: NodeId,
  pub mutability: Mutability,
  pub inner: Box<Type>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct OptionalType {
  pub id: NodeId,
  pub inner: Box<Type>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayType {
  pub id: NodeId,
  pub element: Box<Type>,
  pub size: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleType {
  pub id: NodeId,
  pub elements: Vec<Type>,
  pub span: Span,
}

impl Type {
  pub fn span(&self) -> Span {
    match self {
      Self::Primitive(p) => p.span,
      Self::Path(p) => p.span,
      Self::Function(f) => f.span,
      Self::Pointer(r) => r.span,
      Self::Array(a) => a.span,
      Self::Tuple(t) => t.span,
      Self::Unit(u) => u.span,
      Self::Optional(o) => o.span,
      Self::Invalid => Span::dummy()
    }
  }
}
