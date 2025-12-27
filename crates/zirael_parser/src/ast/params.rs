use crate::ast::expressions::Expr;
use crate::ast::identifier::Ident;
use crate::ast::types::Type;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub enum Param {
  SelfParam(SelfParam),
  Regular(RegularParam),
  Variadic(VariadicParam),
}

#[derive(Debug, Clone)]
pub struct SelfParam {
  pub kind: SelfKind,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SelfKind {
  Value,  // self
  Mut,    // mut self
  Ref,    // &self
  RefMut, // &mut self
}

#[derive(Debug, Clone)]
pub struct RegularParam {
  pub name: Ident,
  pub ty: Type,
  pub default: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VariadicParam {
  pub name: Ident,
  pub ty: Type,
  pub span: Span,
}
