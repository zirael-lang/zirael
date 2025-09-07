use crate::{symbols::SymbolId, Type};
use zirael_utils::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
  pub segments: Vec<PathSegment>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathSegment {
  pub identifier: Identifier,
  pub type_args: Vec<Type>,
  pub symbol_id: Option<SymbolId>,
  pub span: Span,
}

impl Path {
  pub fn new(segments: Vec<PathSegment>, span: Span) -> Self {
    Self { segments, span }
  }

  pub fn from_identifier(identifier: Identifier, span: Span) -> Self {
    Self {
      segments: vec![PathSegment {
        identifier,
        type_args: vec![],
        symbol_id: None,
        span: span.clone(),
      }],
      span,
    }
  }

  pub fn is_simple(&self) -> bool {
    self.segments.len() == 1 && self.segments[0].type_args.is_empty()
  }

  pub fn last_segment(&self) -> &PathSegment {
    self.segments.last().expect("path should have at least one segment")
  }

  pub fn last_segment_mut(&mut self) -> &mut PathSegment {
    self.segments.last_mut().expect("path should have at least one segment")
  }

  pub fn first_segment(&self) -> &PathSegment {
    self.segments.first().expect("path should have at least one segment")
  }
}

impl PathSegment {
  pub fn new(identifier: Identifier, span: Span) -> Self {
    Self {
      identifier,
      type_args: vec![],
      symbol_id: None,
      span,
    }
  }

  pub fn with_type_args(identifier: Identifier, type_args: Vec<Type>, span: Span) -> Self {
    Self {
      identifier,
      type_args,
      symbol_id: None,
      span,
    }
  }
}
