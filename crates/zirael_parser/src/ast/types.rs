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
  Unit(Span),
}

#[derive(Debug, Clone)]
pub struct PrimitiveType {
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
  pub path: Path,
  pub args: Option<Vec<Type>>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
  pub params: Vec<Type>,
  pub return_type: Box<Type>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReferenceType {
  pub is_mut: bool,
  pub inner: Box<Type>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayType {
  pub element: Box<Type>,
  pub size: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TupleType {
  pub elements: Vec<Type>,
  pub span: Span,
}

impl Type {
  pub fn span(&self) -> Span {
    match self {
      Type::Primitive(p) => p.span,
      Type::Path(p) => p.span,
      Type::Function(f) => f.span,
      Type::Reference(r) => r.span,
      Type::Array(a) => a.span,
      Type::Tuple(t) => t.span,
      Type::Unit(s) => s.clone(),
    }
  }
}
