use crate::ast::NodeId;
use crate::ast::expressions::Expr;
use crate::ast::import::Path;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub enum Type {
  Primitive(PrimitiveType),
  Path(TypePath),
  Function(FunctionType),
  Reference(ReferenceType),
  Array(ArrayType),
  Tuple(TupleType),
  Unit(UnitType),
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
}

#[derive(Debug, Clone)]
pub struct ReferenceType {
  pub id: NodeId,
  pub is_mut: bool,
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
      Self::Reference(r) => r.span,
      Self::Array(a) => a.span,
      Self::Tuple(t) => t.span,
      Self::Unit(u) => u.span,
    }
  }
}
