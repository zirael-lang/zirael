use crate::ids::HirId;
use crate::ty::{PathSegment, Ty};
use zirael_resolver::DefId;
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone, Default)]
pub struct Generics {
  pub params: Vec<GenericParam>,
  pub span: Span,
}

impl Generics {
  pub fn empty() -> Self {
    Self {
      params: Vec::new(),
      span: Span::default(),
    }
  }

  pub fn is_empty(&self) -> bool {
    self.params.is_empty()
  }
}

#[derive(Debug, Clone)]
pub struct GenericParam {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub name: Identifier,
  pub kind: GenericParamKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum GenericParamKind {
  /// Type parameter: `T`, `T: Bound`
  Type { bounds: Vec<TypeBound> },
  /// Const parameter: `const N: usize`
  Const { ty: Ty },
}

#[derive(Debug, Clone)]
pub struct TypeBound {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub path: Vec<PathSegment>,
  pub span: Span,
}
