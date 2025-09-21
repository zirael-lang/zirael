use id_arena::Id;
use zirael_utils::ident_table::Identifier;
use crate::{MonomorphizationId, OriginalSymbolId, SymbolId};

pub type TyId = Id<Ty>;

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

  Symbol(OriginalSymbolId),

  GenericVariable { id: usize, name: Identifier },
}

pub type StructSymbolId = u32;
pub type EnumSymbolId = u32;
