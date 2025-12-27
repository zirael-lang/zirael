use crate::ast::identifier::Ident;
use crate::ast::types::TypePath;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct GenericParams {
  pub params: Vec<GenericParam>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericParam {
  pub name: Ident,
  pub bounds: Vec<TypeBound>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeBound {
  pub path: TypePath,
  pub span: Span,
}
