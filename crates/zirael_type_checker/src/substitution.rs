use crate::TypeInference;
use crate::symbol_table::{MonomorphizedSymbol, TyId};
use std::collections::HashMap;
use zirael_parser::{GenericParameter, OriginalSymbolId, SymbolKind, Type};
use zirael_utils::ident_table::resolve;
use zirael_utils::prelude::{Colorize, Identifier, ReportBuilder, ReportKind, debug};

impl<'reports> TypeInference<'reports> {
  pub fn substitute_generic_params(
    &mut self,
    ty: &mut Type,
    params: &Vec<GenericParameter>,
    generics: &[Type],
  ) {
    let mut param_map = HashMap::new();
    for (param, concrete) in params.iter().zip(generics.iter()) {
      param_map.insert(param.name, self.sym_table.intern_type(concrete.clone()));
    }

    self.substitute_type_with_map(ty, &param_map);
  }

  pub fn substitute_type_with_map(&mut self, ty: &mut Type, param_map: &HashMap<Identifier, TyId>) {
    if param_map.is_empty() {
      self.try_to_symbol(ty);
      return;
    }

    match ty {
      Type::Named { name, generics } if generics.is_empty() => {
        if let Some(concrete) = param_map.get(name) {
          *ty = Type::Id(concrete.clone());
        }
      }
      Type::Named { name: _, generics } => {
        for generic in generics.iter_mut() {
          self.substitute_type_with_map(generic, param_map);
        }
      }
      Type::Pointer(inner) => {
        self.substitute_type_with_map(inner, param_map);
      }
      Type::Reference(inner) => {
        self.substitute_type_with_map(inner, param_map);
      }
      Type::Array(inner, _size) => {
        self.substitute_type_with_map(inner, param_map);
      }
      Type::Function { params, return_type } => {
        for param in params.iter_mut() {
          self.substitute_type_with_map(param, param_map);
        }
        self.substitute_type_with_map(return_type, param_map);
      }
      _ => {}
    };

    self.try_to_symbol(ty);
  }

  pub fn try_to_symbol(&mut self, ty: &mut Type) {
    match ty {
      Type::Pointer(inner) => {
        self.try_to_symbol(inner);
      }
      Type::Reference(inner) => {
        self.try_to_symbol(inner);
      }
      Type::Array(inner, _size) => {
        self.try_to_symbol(inner);
      }
      Type::Function { params, return_type } => {
        for param in params.iter_mut() {
          self.try_to_symbol(param);
        }
        self.try_to_symbol(return_type);
      }
      Type::Named { name, generics } => {
        if self.ctx.is_generic_parameter(*name) {
          return;
        }

        for generic in generics.iter_mut() {
          self.try_to_symbol(generic);
        }

        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
          if self.current_item.is_some() {
            debug!("Adding relation: {:?} -> {:?} (name: {})", self.current_item.unwrap(), symbol.id, resolve(name));
            self.symbol_table.new_relation(
              OriginalSymbolId::Symbol(self.current_item.unwrap()),
              OriginalSymbolId::Symbol(symbol.canonical_symbol),
            );
          }

          *ty = Type::Symbol(symbol.id);
        } else {
          if !self.ctx.is_generic_parameter(*name) {
            let report = ReportBuilder::builder(
              &format!("couldn't find struct or enum named {}", resolve(name).dimmed().bold()),
              ReportKind::Error,
            )
            .label("not found", name.span().clone());

            self.reports.add(self.processed_file.unwrap(), report);
          }
        }
      }
      _ => {}
    };
  }
}
