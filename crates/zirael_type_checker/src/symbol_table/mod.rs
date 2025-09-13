mod generic;
mod interner;

use id_arena::{Arena, Id};
use std::collections::HashMap;
use zirael_parser::{MonomorphizationId, SymbolId};

pub use generic::*;
pub use interner::*;
pub use zirael_parser::ty::*;
use zirael_utils::prelude::Identifier;

pub type FunctionId = id_arena::Id<GenericFunction>;
pub type StructId = id_arena::Id<GenericStruct>;
pub type EnumId = id_arena::Id<GenericEnum>;

#[derive(Clone, Debug)]
pub enum GenericSymbol {
  Function(GenericFunction),
  Struct(GenericStruct),
  Enum(GenericEnum),
}

#[derive(Clone, Debug)]
pub enum MonomorphizedSymbol {
  Function(MonomorphizedFunction),
  Struct(MonomorphizedStruct),
  Enum(MonomorphizedEnum),
}

impl MonomorphizedSymbol {
  pub fn original_symbol_id(&self) -> SymbolId {
    match self {
      MonomorphizedSymbol::Function(f) => f.original_symbol_id,
      MonomorphizedSymbol::Struct(s) => s.original_symbol_id,
      MonomorphizedSymbol::Enum(e) => e.original_symbol_id,
    }
  }

  pub fn name(&self) -> &Identifier {
    match self {
      MonomorphizedSymbol::Function(f) => &f.name,
      MonomorphizedSymbol::Struct(s) => &s.name,
      MonomorphizedSymbol::Enum(e) => &e.name,
    }
  }

  pub fn concrete_types(&self) -> &HashMap<Identifier, TyId> {
    match self {
      MonomorphizedSymbol::Function(f) => &f.concrete_types,
      MonomorphizedSymbol::Struct(s) => &s.concrete_types,
      MonomorphizedSymbol::Enum(e) => &e.concrete_types,
    }
  }
}

#[derive(Clone, Debug, Default)]
pub struct MonoSymbolTable {
  // Generic symbols
  pub generic_symbols: Arena<GenericSymbol>,
  pub symbol_to_generic: HashMap<SymbolId, Id<GenericSymbol>>,
  pub generic_to_symbol: HashMap<Id<GenericSymbol>, SymbolId>,

  pub monomorphized_symbols: HashMap<MonomorphizationId, MonomorphizedSymbol>,
  pub mono_ids: Arena<()>,

  pub type_cache: HashMap<(SymbolId, Vec<TyId>), TyId>,

  // Interner
  arena: Arena<Ty>,
  cache: HashMap<Ty, TyId>,
}

impl MonoSymbolTable {
  pub fn add_generic_function(
    &mut self,
    symbol_id: SymbolId,
    function: GenericFunction,
  ) -> Id<GenericSymbol> {
    let generic_id = self.generic_symbols.alloc(GenericSymbol::Function(function));
    self.symbol_to_generic.insert(symbol_id, generic_id);
    self.generic_to_symbol.insert(generic_id, symbol_id);
    generic_id
  }

  pub fn add_generic_struct(
    &mut self,
    symbol_id: SymbolId,
    struct_def: GenericStruct,
  ) -> Id<GenericSymbol> {
    let generic_id = self.generic_symbols.alloc(GenericSymbol::Struct(struct_def));
    self.symbol_to_generic.insert(symbol_id, generic_id);
    self.generic_to_symbol.insert(generic_id, symbol_id);
    generic_id
  }

  pub fn add_generic_enum(
    &mut self,
    symbol_id: SymbolId,
    enum_def: GenericEnum,
  ) -> Id<GenericSymbol> {
    let generic_id = self.generic_symbols.alloc(GenericSymbol::Enum(enum_def));
    self.symbol_to_generic.insert(symbol_id, generic_id);
    self.generic_to_symbol.insert(generic_id, symbol_id);
    generic_id
  }

  pub fn add_monomorphized_symbol(&mut self, mut symbol: MonomorphizedSymbol) -> MonomorphizationId {
    let id = self.mono_ids.alloc_with_id(|id| {
      symbol = match &mut symbol {
        MonomorphizedSymbol::Function(f) => {
          f.mono_id = Some(id);

          MonomorphizedSymbol::Function(f.clone())
        }
        MonomorphizedSymbol::Struct(s) => {
          s.mono_id = id;
          MonomorphizedSymbol::Struct(s.clone())
        }
        MonomorphizedSymbol::Enum(e) => {
          e.mono_id = id;
          MonomorphizedSymbol::Enum(e.clone())
        }
      };
    });

    self.monomorphized_symbols.insert(id, symbol);
    id
  }

  pub fn get_monomorphized_symbol(
    &self,
    mono_id: MonomorphizationId,
  ) -> Option<&MonomorphizedSymbol> {
    self.monomorphized_symbols.get(&mono_id)
  }

  pub fn get_monomorphized_function(
    &self,
    mono_id: MonomorphizationId,
  ) -> Option<&MonomorphizedFunction> {
    match self.monomorphized_symbols.get(&mono_id) {
      Some(MonomorphizedSymbol::Function(f)) => Some(f),
      _ => None,
    }
  }

  pub fn get_monomorphized_struct(
    &self,
    mono_id: MonomorphizationId,
  ) -> Option<&MonomorphizedStruct> {
    match self.monomorphized_symbols.get(&mono_id) {
      Some(MonomorphizedSymbol::Struct(s)) => Some(s),
      _ => None,
    }
  }

  pub fn get_monomorphized_enum(&self, mono_id: MonomorphizationId) -> Option<&MonomorphizedEnum> {
    match self.monomorphized_symbols.get(&mono_id) {
      Some(MonomorphizedSymbol::Enum(e)) => Some(e),
      _ => None,
    }
  }

  pub fn get_generic_symbol(&self, symbol_id: SymbolId) -> Option<&GenericSymbol> {
    self.symbol_to_generic.get(&symbol_id).and_then(|&id| self.generic_symbols.get(id))
  }

  pub fn get_generic_function(&self, symbol_id: SymbolId) -> Option<&GenericFunction> {
    match self.get_generic_symbol(symbol_id) {
      Some(GenericSymbol::Function(f)) => Some(f),
      _ => None,
    }
  }

  pub fn get_generic_struct(&self, symbol_id: SymbolId) -> Option<&GenericStruct> {
    match self.get_generic_symbol(symbol_id) {
      Some(GenericSymbol::Struct(s)) => Some(s),
      _ => None,
    }
  }

  pub fn get_generic_enum(&self, symbol_id: SymbolId) -> Option<&GenericEnum> {
    match self.get_generic_symbol(symbol_id) {
      Some(GenericSymbol::Enum(e)) => Some(e),
      _ => None,
    }
  }

  pub fn iter_monomorphized_symbols(
    &self,
  ) -> impl Iterator<Item = (&MonomorphizationId, &MonomorphizedSymbol)> {
    self.monomorphized_symbols.iter()
  }

  pub fn get_mono_generics(&self, symbol_id: MonomorphizationId) -> Option<Vec<Ty>> {
    let generic = self.get_monomorphized_symbol(symbol_id)?;
    let ids = match generic {
      MonomorphizedSymbol::Function(f) => f.concrete_types.values().cloned().collect::<Vec<_>>(),
      MonomorphizedSymbol::Struct(s) => s.concrete_types.values().cloned().collect::<Vec<_>>(),
      MonomorphizedSymbol::Enum(e) => e.concrete_types.values().cloned().collect::<Vec<_>>(),
    };

    Some(ids.iter().filter_map(|id| self.arena.get(*id)).cloned().collect::<Vec<_>>())
  }
}
