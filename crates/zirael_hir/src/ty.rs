use crate::Expr;
use crate::ids::HirId;
use zirael_parser::ast::types::{Mutability, PrimitiveKind};
use zirael_resolver::DefId;
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Ty {
  pub hir_id: HirId,
  pub kind: TyKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TyKind {
  Primitive(PrimitiveKind),

  /// Named type with resolved DefId: `Foo`, `Vec<T>`
  Path {
    def_id: DefId,
    segments: Vec<PathSegment>,
  },

  Ptr {
    mutability: Mutability,
    ty: Box<Ty>,
  },

  /// Optional type: `T?`
  Optional(Box<Ty>),

  /// Array type: `[T; N]`
  Array {
    ty: Box<Ty>,
    len: ArrayLen,
  },

  /// Slice type: `[T]`
  Slice(Box<Ty>),

  /// Tuple type: `(A, B, C)`
  Tuple(Vec<Ty>),

  /// Function type: `fn(A, B) -> C`
  // TODO: also add modifiers like extern etc
  Fn {
    params: Vec<Ty>,
    ret: Box<Ty>,
  },

  /// Unit type: `()`
  Unit,

  /// Never type: `!`
  Never,

  /// Type to be inferred
  Infer,

  Err,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
  pub name: Identifier,
  pub args: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub enum ArrayLen {
  /// Known constant length
  Const(usize),
  /// Length from a constant expression
  ConstExpr(Box<Expr>),
}
