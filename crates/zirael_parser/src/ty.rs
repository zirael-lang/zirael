use id_arena::Id;
use zirael_utils::ident_table::Identifier;
use crate::{MonomorphizationId, SymbolId};

pub type TyId = Id<Ty>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeSymbolId {
  Mono(MonomorphizationId),
  Generic(SymbolId),
}

impl From<MonomorphizationId> for TypeSymbolId {
  fn from(id: MonomorphizationId) -> Self {
    TypeSymbolId::Mono(id)
  }
}

impl From<SymbolId> for TypeSymbolId {
  fn from(id: SymbolId) -> Self {
    TypeSymbolId::Generic(id)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
  String,
  Char,
  Int,
  Uint,
  Float,
  Bool,
  Void,
  Never,

  Pointer(Box<Ty>),
  Reference(Box<Ty>),

  Array(Box<Ty>),
  Function(Vec<Ty>, Box<Ty>),

  Symbol(TypeSymbolId),

  GenericVariable { id: usize, name: Identifier },
}

pub type StructSymbolId = u32;
pub type EnumSymbolId = u32;
