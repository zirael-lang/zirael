use crate::ast::identifier::Ident;
use crate::ast::types::TypePath;
use crate::ast::NodeId;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct GenericParams {
  pub id: NodeId,
  pub params: Vec<GenericParam>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericParam {
  pub id: NodeId,
  pub name: Ident,
  pub bounds: Vec<TypeBound>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeBound {
  pub id: NodeId,
  pub path: TypePath,
  pub span: Span,
}
