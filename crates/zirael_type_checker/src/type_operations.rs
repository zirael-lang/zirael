use crate::TypeInference;
use crate::symbol_table::TyId;
use std::collections::HashMap;
use zirael_parser::{GenericParameter, Type};
use zirael_utils::prelude::Identifier;

impl<'reports> TypeInference<'reports> {
  pub fn symbol_table(&self) -> &zirael_parser::SymbolTable {
    &self.symbol_table
  }

  pub fn types_equal(&self, left: &Type, right: &Type) -> bool {
    self.structural_eq(left, right)
  }

  /// Check if a type is concrete (no type variables or inferred types)
  pub fn is_concrete_type(&self, ty: &Type) -> bool {
    match ty {
      Type::Variable { .. } | Type::Inferred => false,
      Type::Named { generics, .. } => generics.iter().all(|g| self.is_concrete_type(g)),
      Type::Reference(inner) | Type::Pointer(inner) => self.is_concrete_type(inner),
      Type::Array(inner, _) => self.is_concrete_type(inner),
      Type::Function { params, return_type } => {
        params.iter().all(|p| self.is_concrete_type(p)) && self.is_concrete_type(return_type)
      }
      _ => true,
    }
  }

  pub fn has_generics(&self, ty: &Type) -> bool {
    match ty {
      Type::Variable { .. } => true,
      Type::Named { generics, .. } => {
        !generics.is_empty() || generics.iter().any(|g| self.has_generics(g))
      }
      Type::Reference(inner) | Type::Pointer(inner) => self.has_generics(inner),
      Type::Array(inner, _) => self.has_generics(inner),
      Type::Function { params, return_type } => {
        params.iter().any(|p| self.has_generics(p)) || self.has_generics(return_type)
      }
      _ => false,
    }
  }

  pub fn all_generics_concrete(&self, generics: &[Type]) -> bool {
    generics.iter().all(|g| self.is_concrete_type(g))
  }

  pub fn create_generic_mapping(
    &mut self,
    params: &[GenericParameter],
  ) -> HashMap<Identifier, TyId> {
    params
      .iter()
      .map(|p| {
        let ty = self.ctx.fresh_type_var(Some(p.name));
        (p.name, self.sym_table.intern_type(ty))
      })
      .collect()
  }

  pub fn is_numeric_type(&self, ty: &Type) -> bool {
    matches!(ty, Type::Int | Type::Uint | Type::Float)
  }

  pub fn is_integer_type(&self, ty: &Type) -> bool {
    matches!(ty, Type::Int | Type::Uint)
  }

  pub fn both_numeric(&self, left: &Type, right: &Type) -> bool {
    self.is_numeric_type(left) && self.is_numeric_type(right)
  }

  pub fn comparable_for_equality(&self, left: &Type, right: &Type) -> bool {
    match (left, right) {
      (a, b) if self.types_equal(a, b) => true,
      (a, b) if self.both_numeric(a, b) => true,
      (Type::Reference(a), Type::Reference(b)) => self.comparable_for_equality(a, b),
      (Type::Pointer(a), Type::Pointer(b)) => self.comparable_for_equality(a, b),
      (Type::Bool, Type::Bool) | (Type::Char, Type::Char) | (Type::String, Type::String) => true,
      _ => false,
    }
  }

  pub fn comparable_for_ordering(&self, left: &Type, right: &Type) -> bool {
    match (left, right) {
      (a, b) if self.both_numeric(a, b) => true,
      (Type::Char, Type::Char) => true,
      _ => false,
    }
  }

  pub fn get_inner_type<'a>(&self, ty: &'a Type) -> Option<&'a Type> {
    match ty {
      Type::Reference(inner) | Type::Pointer(inner) => Some(inner),
      _ => None,
    }
  }

  pub fn make_reference(&self, inner: Type) -> Type {
    Type::Reference(Box::new(inner))
  }

  pub fn make_pointer(&self, inner: Type) -> Type {
    Type::Pointer(Box::new(inner))
  }
}
