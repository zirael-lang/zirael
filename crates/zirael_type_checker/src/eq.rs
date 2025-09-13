use crate::TypeInference;
use crate::symbol_table::MonoSymbolTable;
use zirael_parser::Type;
use zirael_parser::ty::{Ty, TyId, TypeSymbolId};

pub trait IntoTyId {
  fn into_ty_id(self, sym_table: &mut MonoSymbolTable) -> TyId;
}

impl IntoTyId for TyId {
  fn into_ty_id(self, _sym_table: &mut MonoSymbolTable) -> TyId {
    self
  }
}

impl IntoTyId for &Type {
  fn into_ty_id(self, sym_table: &mut MonoSymbolTable) -> TyId {
    sym_table.intern_type(self.clone())
  }
}

impl<'reports> TypeInference<'reports> {
  pub fn eq<L, R>(&mut self, left: L, right: R) -> bool
  where
    L: IntoTyId,
    R: IntoTyId,
  {
    let left_id = left.into_ty_id(&mut self.sym_table);
    let right_id = right.into_ty_id(&mut self.sym_table);
    self.structural_eq(left_id, right_id)
  }

  pub(crate) fn structural_eq(&self, left: TyId, right: TyId) -> bool {
    if left == right {
      return true;
    }

    let left_ty = self.sym_table.resolve(left);
    let right_ty = self.sym_table.resolve(right);

    self.structural_eq_ty(left_ty, right_ty)
  }

  fn structural_eq_ty(&self, left_ty: &Ty, right_ty: &Ty) -> bool {
    match (left_ty, right_ty) {
      (Ty::String, Ty::String)
      | (Ty::Char, Ty::Char)
      | (Ty::Int, Ty::Int)
      | (Ty::Uint, Ty::Uint)
      | (Ty::Float, Ty::Float)
      | (Ty::Bool, Ty::Bool)
      | (Ty::Void, Ty::Void)
      | (Ty::Never, Ty::Never) => true,

      (Ty::Never, _) | (_, Ty::Never) => true,

      (Ty::Pointer(a), Ty::Pointer(b)) | (Ty::Reference(a), Ty::Reference(b)) => {
        self.structural_eq_ty(a.as_ref(), b.as_ref())
      }

      (Ty::Array(a_ty), Ty::Array(b_ty)) => self.structural_eq_ty(a_ty.as_ref(), b_ty.as_ref()),

      (Ty::Function(a_params, a_ret), Ty::Function(b_params, b_ret)) => {
        if a_params.len() != b_params.len() {
          return false;
        }

        if !self.structural_eq_ty(a_ret.as_ref(), b_ret.as_ref()) {
          return false;
        }

        for (a_param, b_param) in a_params.iter().zip(b_params.iter()) {
          if !self.structural_eq_ty(a_param, b_param) {
            return false;
          }
        }

        true
      }

      (Ty::Symbol(a_sym), Ty::Symbol(b_sym)) => match (a_sym, b_sym) {
        (TypeSymbolId::Generic(a_id), TypeSymbolId::Generic(b_id)) => a_id == b_id,
        (TypeSymbolId::Mono(a_id), TypeSymbolId::Mono(b_id)) => {
          let Some(mono1) = self.sym_table.get_monomorphized_symbol(*a_id) else { return false };
          let Some(mono2) = self.sym_table.get_monomorphized_symbol(*b_id) else { return false };

          if mono1.original_symbol_id() != mono2.original_symbol_id() {
            return false;
          }

          let concrete_types1 = mono1.concrete_types();
          let concrete_types2 = mono2.concrete_types();

          for (name, value) in concrete_types1 {
            let Some(mono2_value) = concrete_types2.get(name) else { return false };
            if !self.structural_eq(*value, *mono2_value) {
              return false;
            }
          }

          true
        }
        _ => false,
      },

      (Ty::GenericVariable { id: id_a, .. }, Ty::GenericVariable { id: id_b, .. }) => id_a == id_b,

      _ => false,
    }
  }
}
