//! HIR patterns for destructuring and matching.

use crate::ids::HirId;

use crate::{Expr, Literal};
use zirael_resolver::DefId;
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::Span;

/// A pattern in HIR.
#[derive(Debug, Clone)]
pub struct Pat {
  pub hir_id: HirId,
  pub kind: PatKind,
  pub span: Span,
}

/// Pattern kinds.
#[derive(Debug, Clone)]
pub enum PatKind {
  /// Wildcard: `_`
  Wild,

  /// Binding: `x`, `mut x`
  Binding {
    def_id: DefId,
    name: Identifier,
    is_mut: bool,
    /// Optional subpattern: `x @ Some(_)`
    subpat: Option<Box<Pat>>,
  },

  /// Literal pattern: `42`, `"hello"`, `true`
  Literal(Literal),

  /// Tuple pattern: `(a, b, c)`
  Tuple(Vec<Pat>),

  /// Struct pattern: `Point { x, y }`
  Struct {
    def_id: DefId,
    fields: Vec<FieldPat>,
    rest: bool,
  },

  /// Tuple struct / enum variant pattern: `Some(x)`, `None`
  TupleStruct { def_id: DefId, pats: Vec<Pat> },

  /// Path pattern (unit variant, const): `None`, `CONST_VALUE`
  Path { def_id: DefId },

  /// Or pattern: `A | B | C`
  Or(Vec<Pat>),

  /// Slice pattern: `[first, .., last]`
  Slice {
    prefix: Vec<Pat>,
    middle: Option<Box<Pat>>,
    suffix: Vec<Pat>,
  },

  /// Range pattern: `1..=10`
  Range {
    start: Option<Box<Expr>>,
    end: Option<Box<Expr>>,
    inclusive: bool,
  },

  /// Error recovery
  Err,
}

#[derive(Debug, Clone)]
pub struct FieldPat {
  pub hir_id: HirId,
  pub name: Identifier,
  pub pat: Pat,
  pub span: Span,
}
