use crate::symbol_table::{MonoSymbolTable, TypeSymbolId};
use id_arena::Arena;
use std::collections::HashMap;
use std::ops::Index;
use zirael_parser::Type;
use zirael_parser::ty::{EnumSymbolId, StructSymbolId, Ty, TyId};

impl MonoSymbolTable {
  pub fn intern_type(&mut self, parsed_type: Type) -> TyId {
    if let Type::Id(id) = parsed_type {
      return id;
    }

    let ty = match parsed_type {
      Type::Int => Ty::Int,
      Type::Float => Ty::Float,
      Type::Bool => Ty::Bool,
      Type::String => Ty::String,
      Type::Void => Ty::Void,
      Type::Never => Ty::Never,
      Type::Pointer(ptr) => {
        let pointee_ty = self.intern_type(*ptr);
        Ty::Pointer(Box::new(self.resolve(pointee_ty).clone()))
      }

      Type::Reference(referent) => {
        let referent_ty = self.intern_type(*referent);
        Ty::Reference(Box::new(self.resolve(referent_ty).clone()))
      }

      Type::Array(element_type, ..) => {
        let element_ty = self.intern_type(*element_type);
        Ty::Array(Box::new(self.resolve(element_ty).clone()))
      }
      Type::Function { params, return_type } => {
        let param_tys = params
          .into_iter()
          .map(|t| {
            let ty = self.intern_type(t);
            self.resolve(ty).clone()
          })
          .collect();
        let ret_ty = self.intern_type(*return_type);
        let return_ty = self.resolve(ret_ty).clone();
        Ty::Function(param_tys, Box::new(return_ty))
      }
      Type::Symbol(sym) => Ty::Symbol(TypeSymbolId::Generic(sym)),
      Type::MonomorphizedSymbol(mono_id) => Ty::Symbol(TypeSymbolId::Mono(mono_id)),
      Type::Variable { id, name } => Ty::GenericVariable { id, name },

      _ => panic!("Unsupported type for interning: {:?}", parsed_type),
    };

    self.intern(ty)
  }

  pub fn intern(&mut self, ty: Ty) -> TyId {
    if let Some(&id) = self.cache.get(&ty) {
      return id;
    }

    let id = self.arena.alloc(ty.clone());
    self.cache.insert(ty, id);
    id
  }

  pub fn lookup(&self, ty: &Ty) -> Option<TyId> {
    self.cache.get(ty).copied()
  }

  pub fn get(&self, id: TyId) -> Option<&Ty> {
    self.arena.get(id)
  }

  pub fn resolve(&self, id: TyId) -> &Ty {
    &self.arena[id]
  }

  pub fn int(&mut self) -> TyId {
    self.intern(Ty::Int)
  }

  pub fn float(&mut self) -> TyId {
    self.intern(Ty::Float)
  }

  pub fn bool(&mut self) -> TyId {
    self.intern(Ty::Bool)
  }

  pub fn string(&mut self) -> TyId {
    self.intern(Ty::String)
  }

  pub fn void(&mut self) -> TyId {
    self.intern(Ty::Void)
  }

  pub fn array(&mut self, element_ty: Ty) -> TyId {
    self.intern(Ty::Array(Box::new(element_ty)))
  }

  pub fn function(&mut self, params: Vec<Ty>, return_ty: Ty) -> TyId {
    self.intern(Ty::Function(params, Box::new(return_ty)))
  }

  pub fn symbol(&mut self, id: TypeSymbolId) -> TyId {
    self.intern(Ty::Symbol(id))
  }

  pub fn len(&self) -> usize {
    self.arena.len()
  }

  pub fn iter(&self) -> impl Iterator<Item = (TyId, &Ty)> {
    self.arena.iter()
  }
}

impl Index<TyId> for MonoSymbolTable {
  type Output = Ty;

  fn index(&self, index: TyId) -> &Self::Output {
    &self.arena[index]
  }
}
