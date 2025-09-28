use crate::TypeInference;
use crate::symbol_table::TyId;
use std::collections::HashMap;
use zirael_parser::{GenericParameter, OriginalSymbolId, Type};
use zirael_parser::ty::Ty;
use zirael_utils::prelude::{Identifier, warn};

impl<'reports> TypeInference<'reports> {
  pub fn symbol_table(&self) -> &zirael_parser::SymbolTable {
    &self.symbol_table
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

  pub fn has_generics(&mut self, ty: &Type) -> bool {
    let ty = self.sym_table.intern_type(ty.clone());
    
    self.has_generics_ty(&self.sym_table[ty])
  }
  
  fn has_generics_ty(&self, ty: &Ty) -> bool {
    match ty {
      Ty::GenericVariable { .. } => true,
      Ty::Pointer(inner) => self.has_generics_ty(inner),
      Ty::Reference(inner) => self.has_generics_ty(inner),
      Ty::Array(inner) => self.has_generics_ty(inner),
      Ty::Function ( params, return_type ) => {
        for param in params {
          if self.has_generics_ty(param) {
            return true;
          }
        }
        self.has_generics_ty(return_type)
      }
      Ty::Symbol(id) if id.as_symbol().is_none() => true,
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
      (a, b) if a == b => true,
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

  pub fn ty_id_to_type(&self, ty_id: TyId) -> Type {
    let ty = self.sym_table.resolve(ty_id);
    self.ty_to_type(ty)
  }

  fn ty_to_type(&self, ty: &Ty) -> Type {
    match ty {
      Ty::String => Type::String,
      Ty::Char => Type::Char,
      Ty::Int => Type::Int,
      Ty::Uint => Type::Uint,
      Ty::Float => Type::Float,
      Ty::Bool => Type::Bool,
      Ty::Void => Type::Void,
      Ty::Never => Type::Never,
      Ty::Pointer(inner) => Type::Pointer(Box::new(self.ty_to_type(inner))),
      Ty::Reference(inner) => Type::Reference(Box::new(self.ty_to_type(inner))),
      Ty::Array(inner) => Type::Array(Box::new(self.ty_to_type(inner)), None),
      Ty::Function(params, return_type) => {
        let param_types = params.iter().map(|p| self.ty_to_type(p)).collect();
        Type::Function {
          params: param_types,
          return_type: Box::new(self.ty_to_type(return_type)),
        }
      }
      Ty::Symbol(original_id) => match original_id {
        OriginalSymbolId::Symbol(symbol_id) => Type::Symbol(*symbol_id),
        OriginalSymbolId::Monomorphization(mono_id) => Type::MonomorphizedSymbol(*mono_id),
      },
      Ty::GenericVariable { id, name } => Type::Variable { id: *id, name: *name },
    }
  }
}
