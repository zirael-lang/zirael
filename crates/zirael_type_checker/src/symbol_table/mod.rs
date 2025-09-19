mod generic;
mod interner;

use id_arena::{Arena, Id};
use std::collections::HashMap;
use zirael_parser::{FunctionSignature, GenericParameter, MonomorphizationId, SymbolId};

pub use generic::*;
pub use interner::*;
pub use zirael_parser::ty::*;
use zirael_utils::prelude::{Identifier, Span};

#[derive(Clone, Debug, Default)]
pub struct MonoSymbolTable {
  // Generic symbols
  pub generic_symbols: Arena<GenericSymbol>,
  pub symbol_to_generic: HashMap<SymbolId, Id<GenericSymbol>>,
  pub generic_to_symbol: HashMap<Id<GenericSymbol>, SymbolId>,

  pub symbol_to_mono_variants: HashMap<SymbolId, Vec<MonomorphizationId>>,
  pub monomorphized_symbols: HashMap<MonomorphizationId, MonomorphizedSymbol>,
  pub mono_ids: Arena<()>,

  pub type_cache: HashMap<(SymbolId, Vec<TyId>), TyId>,

  // Interner
  pub arena: Arena<Ty>,
  cache: HashMap<Ty, TyId>,
}

impl MonoSymbolTable {
  pub fn add_generic_function(
    &mut self,
    symbol_id: SymbolId,
    function: GenericFunction,
    is_used: bool,
    span: Span,
  ) -> Id<GenericSymbol> {
    let generic_symbol = GenericSymbol::function(
      function.symbol_id,
      function.name,
      function.signature,
      function.generics,
      function.is_extern,
      function.body_type,
      is_used,
      span,
    );
    let generic_id = self.generic_symbols.alloc(generic_symbol);
    self.symbol_to_generic.insert(symbol_id, generic_id);
    self.generic_to_symbol.insert(generic_id, symbol_id);
    generic_id
  }

  pub fn add_generic_struct(
    &mut self,
    symbol_id: SymbolId,
    struct_def: GenericStruct,
    is_used: bool,
    span: Span,
  ) -> Id<GenericSymbol> {
    let generic_symbol = GenericSymbol::struct_def(
      struct_def.symbol_id,
      struct_def.name,
      struct_def.generics,
      struct_def.fields,
      is_used,
      span,
    );
    let generic_id = self.generic_symbols.alloc(generic_symbol);
    self.symbol_to_generic.insert(symbol_id, generic_id);
    self.generic_to_symbol.insert(generic_id, symbol_id);
    generic_id
  }

  pub fn add_generic_value(
    &mut self,
    name: Identifier,
    symbol_id: SymbolId,
    ty: TyId,
    is_used: bool,
    span: Span,
  ) {
    let generic_symbol = GenericSymbol::value(symbol_id, name, ty, is_used, span);
    let generic_id = self.generic_symbols.alloc(generic_symbol);
    self.symbol_to_generic.insert(symbol_id, generic_id);
    self.generic_to_symbol.insert(generic_id, symbol_id);
  }

  pub fn add_generic_enum(
    &mut self,
    symbol_id: SymbolId,
    enum_def: GenericEnum,
    is_used: bool,
    span: Span,
  ) -> Id<GenericSymbol> {
    let generic_symbol = GenericSymbol::enum_def(
      enum_def.symbol_id,
      enum_def.name,
      enum_def.generics,
      enum_def.variants,
      is_used,
      span,
    );

    let generic_id = self.generic_symbols.alloc(generic_symbol);
    self.symbol_to_generic.insert(symbol_id, generic_id);
    self.generic_to_symbol.insert(generic_id, symbol_id);
    generic_id
  }

  pub fn add_monomorphized_symbol(
    &mut self,
    mut symbol: MonomorphizedSymbol,
  ) -> MonomorphizationId {
    let original_symbol_id = symbol.base.original_symbol_id;

    let id = self.mono_ids.alloc_with_id(|id| match &mut symbol.kind {
      MonomorphizedSymbolKind::Function { mono_id, .. } => *mono_id = Some(id),
      MonomorphizedSymbolKind::Struct { mono_id, .. } => *mono_id = id,
      MonomorphizedSymbolKind::EnumVariant { mono_id, .. } => *mono_id = id,
    });

    self.symbol_to_mono_variants
        .entry(original_symbol_id)
        .or_insert_with(Vec::new)
        .push(id);

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
  ) -> Option<(&MonomorphizedSymbolBase, &FunctionSignature, &String, bool)> {
    match self.monomorphized_symbols.get(&mono_id) {
      Some(MonomorphizedSymbol {
             base,
             kind: MonomorphizedSymbolKind::Function { signature, mangled_name, is_extern, .. },
           }) => Some((base, signature, mangled_name, *is_extern)),
      _ => None,
    }
  }

  pub fn get_monomorphized_struct(
    &self,
    mono_id: MonomorphizationId,
  ) -> Option<(&MonomorphizedSymbolBase, &String, &Vec<MonomorphizedStructField>)> {
    match self.monomorphized_symbols.get(&mono_id) {
      Some(MonomorphizedSymbol {
             base,
             kind: MonomorphizedSymbolKind::Struct { mangled_name, fields, .. },
           }) => Some((base, mangled_name, fields)),
      _ => None,
    }
  }

  pub fn get_monomorphized_enum_variant(
    &self,
    mono_id: MonomorphizationId,
  ) -> Option<(&MonomorphizedSymbolBase, SymbolId, &Vec<MonomorphizedStructField>)> {
    match self.monomorphized_symbols.get(&mono_id) {
      Some(MonomorphizedSymbol {
             base,
             kind: MonomorphizedSymbolKind::EnumVariant { symbol_id, fields, .. },
           }) => Some((base, *symbol_id, fields)),
      _ => None,
    }
  }

  pub fn get_generic_symbol(&self, symbol_id: SymbolId) -> Option<&GenericSymbol> {
    self.symbol_to_generic.get(&symbol_id).and_then(|&id| self.generic_symbols.get(id))
  }

  pub fn get_generic_function(
    &self,
    symbol_id: SymbolId,
  ) -> Option<(&GenericSymbolBase, &FunctionSignature, &Vec<GenericParameter>, bool, &Option<TyId>)>
  {
    match self.get_generic_symbol(symbol_id) {
      Some(GenericSymbol {
             base,
             kind: GenericSymbolKind::Function { signature, generics, is_extern, body_type },
           }) => Some((base, signature, generics, *is_extern, body_type)),
      _ => None,
    }
  }

  pub fn get_generic_struct(
    &self,
    symbol_id: SymbolId,
  ) -> Option<(&GenericSymbolBase, &Vec<GenericParameter>, &Vec<GenericStructField>)> {
    match self.get_generic_symbol(symbol_id) {
      Some(GenericSymbol { base, kind: GenericSymbolKind::Struct { generics, fields } }) => {
        Some((base, generics, fields))
      }
      _ => None,
    }
  }

  pub fn get_generic_enum(
    &self,
    symbol_id: SymbolId,
  ) -> Option<(&GenericSymbolBase, &Vec<GenericParameter>, &Vec<GenericEnumVariant>)> {
    match self.get_generic_symbol(symbol_id) {
      Some(GenericSymbol { base, kind: GenericSymbolKind::Enum { generics, variants } }) => {
        Some((base, generics, variants))
      }
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
    let ids = generic.concrete_types().values().cloned().collect::<Vec<_>>();

    Some(ids.iter().filter_map(|id| self.arena.get(*id)).cloned().collect::<Vec<_>>())
  }

  pub fn has_mono_variant(&self, symbol_id: SymbolId) -> bool {
    self.symbol_to_mono_variants
        .get(&symbol_id)
        .map_or(false, |variants| !variants.is_empty())
  }

  pub fn get_mono_variants(&self, symbol_id: SymbolId) -> Option<Vec<MonomorphizationId>> {
    self.symbol_to_mono_variants.get(&symbol_id).cloned()
  }
}