use crate::TypeInference;
use crate::symbol_table::TyId;
use std::collections::HashMap;
use zirael_parser::{Function, GenericParameter, Parameter, SymbolKind, Type};
use zirael_utils::prelude::{Identifier, resolve, warn};

impl<'reports> TypeInference<'reports> {
  /// Get the self type for a method based on its parent structure
  pub fn get_self_type_for_method(&mut self, func: &Function) -> Option<Type> {
    let func_symbol = self.symbol_table.lookup_symbol(&func.name)?;
    let struct_symbol_id = self.symbol_table.is_a_child_of_symbol(func_symbol.id)?;
    let struct_symbol = match self.symbol_table.get_symbol(struct_symbol_id) {
      Ok(symbol) => symbol,
      Err(_) => {
        warn!("Could not retrieve struct symbol for method {}", resolve(&func.name));
        return None;
      }
    };

    match &struct_symbol.kind {
      SymbolKind::Struct { generics, .. }
      | SymbolKind::Enum { generics, .. } => {
        let generic_types = generics
          .iter()
          .map(|g| Type::Named { name: g.name, generics: vec![] })
          .collect::<Vec<_>>();

        Some(Type::Named { name: struct_symbol.name, generics: generic_types })
      }
      SymbolKind::TypeExtension { ty, .. } => {
        if ty.is_primitive() {
          Some(ty.clone())
        } else {
          self.simple_error(
            "right now type extensions can only be used on primitive types",
            "type extension here",
            func.span.clone(),
          );
          None
        }
      }
      _ => {
        warn!("expected struct symbol, got {:?}", struct_symbol.kind);
        None
      }
    }
  }

  pub fn get_generics_for_method(&mut self, func: &Function) -> Option<HashMap<Identifier, TyId>> {
    let func_symbol = self.symbol_table.lookup_symbol(&func.name)?;
    let struct_symbol_id = self.symbol_table.is_a_child_of_symbol(func_symbol.id)?;
    let struct_symbol = match self.symbol_table.get_symbol(struct_symbol_id) {
      Ok(symbol) => symbol,
      Err(_) => {
        warn!("Could not retrieve struct symbol for method {}", resolve(&func.name));
        return None;
      }
    };

    match &struct_symbol.kind {
      SymbolKind::Struct { generics, .. } | SymbolKind::Enum { generics, .. } => {
        if !generics.is_empty() {
          Some(self.create_generic_mapping(generics))
        } else {
          Some(HashMap::new())
        }
      }
      _ => {
        warn!("expected struct symbol, got {:?}", struct_symbol.kind);
        Some(HashMap::new())
      }
    }
  }

  pub fn merge_generic_maps(
    &self,
    struct_generics: &HashMap<Identifier, TyId>,
    method_generics: &HashMap<Identifier, TyId>,
  ) -> HashMap<Identifier, TyId> {
    let mut merged = struct_generics.clone();
    merged.extend(method_generics.clone());
    merged
  }

  pub fn resolve_self_parameter_type_with_generics(
    &mut self,
    param: &Parameter,
    struct_type: &mut Option<Type>,
    struct_generics: &HashMap<Identifier, TyId>,
  ) -> Type {
    match struct_type {
      Some(struct_ty) => {
        self.substitute_type_with_map(struct_ty, struct_generics);

        match &param.ty {
          Type::Reference(_inner) => self.make_reference(struct_ty.clone()),
          _ => struct_ty.clone(),
        }
      }
      None => param.ty.clone(),
    }
  }
}
