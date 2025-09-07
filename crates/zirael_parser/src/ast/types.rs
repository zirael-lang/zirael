use zirael_utils::prelude::*;

use crate::ast::monomorphized_symbol::MonomorphizedSymbol;

pub type TypeVarId = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  String,
  Char,
  Int,
  Uint,
  Float,
  Bool,
  Void,
  Pointer(Box<Type>),
  Reference(Box<Type>),
  Array(Box<Type>, Option<usize>),
  Function { params: Vec<Type>, return_type: Box<Type> },
  Named { name: Identifier, generics: Vec<Type> },

  MonomorphizedSymbol(MonomorphizedSymbol),

  Variable { id: usize, name: Identifier },
  BoundedVariable { id: usize, name: Identifier, bounds: Vec<TraitBound> },

  Inferred,
  Error,
}

impl Type {
  pub fn is_numeric(&self) -> bool {
    matches!(self, Self::Int | Self::Float | Self::Uint)
  }

  pub fn is_int(&self) -> bool {
    matches!(self, Self::Int | Self::Uint)
  }

  pub fn is_float(&self) -> bool {
    matches!(self, Self::Float)
  }

  pub fn is_reference(&self) -> bool {
    matches!(self, Self::Reference(_))
  }

  pub fn is_primitive(&self) -> bool {
    matches!(self, Self::String | Self::Char | Self::Int | Self::Uint | Self::Float | Self::Bool)
  }
  
  pub fn is_bool(&self) -> bool {
    matches!(self, Self::Bool)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericParameter {
  pub name: Identifier,
  pub constraints: Vec<TraitBound>,
  pub default_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitBound {
  pub name: Identifier,
  pub generic_args: Vec<GenericArg>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericArg {
  Type(Type),
  Named { name: Identifier, ty: Type },
}
