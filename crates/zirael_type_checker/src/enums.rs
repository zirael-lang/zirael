use crate::TypeInference;
use crate::symbol_table::{
  MonomorphizedStructField, MonomorphizedSymbol, MonomorphizedSymbolBase, MonomorphizedSymbolKind,
};
use std::collections::HashMap;
use zirael_parser::{CallInfo, EnumVariantData, Expr, SymbolId, SymbolKind, Type};
use zirael_utils::{
  ident_table::Identifier,
  prelude::{Span, resolve},
};

impl<'reports> TypeInference<'reports> {
  pub fn infer_enum_variant_init(
    &mut self,
    variant_id: &SymbolId,
    name_span: &Span,
    fields: &mut HashMap<Identifier, Expr>,
    call_info: &mut Option<CallInfo>,
  ) -> Type {
    let (variant_fields, enum_generics, enum_name, _enum_sym_id) = {
      let variant_symbol = match self.symbol_table.get_symbol(*variant_id) {
        Ok(symbol) => symbol,
        Err(_) => return Type::Error,
      };
      if let SymbolKind::EnumVariant { parent_enum, data, .. } = &variant_symbol.kind {
        let enum_symbol = match self.symbol_table.get_symbol(*parent_enum) {
          Ok(symbol) => symbol,
          Err(_) => return Type::Error,
        };
        if let SymbolKind::Enum { generics, .. } = &enum_symbol.kind {
          match data {
            EnumVariantData::Unit => {
              if !fields.is_empty() {
                self.error(
                  "unit variant cannot have fields",
                  vec![("unit variant".to_string(), name_span.clone())],
                  vec![],
                );
              }
              (vec![], generics.clone(), enum_symbol.name, *parent_enum)
            }
            EnumVariantData::Struct(variant_fields) => {
              (variant_fields.clone(), generics.clone(), enum_symbol.name, *parent_enum)
            }
          }
        } else {
          return Type::Error;
        }
      } else {
        return Type::Error;
      }
    };

    for field in variant_fields.iter() {
      if !fields.contains_key(&field.name) {
        self.error(
          &format!("missing field `{}`", resolve(&field.name)),
          vec![(format!("missing field `{}`", resolve(&field.name)), name_span.clone())],
          vec![],
        );
      }
    }

    for generic in enum_generics.iter() {
      if !self.ctx.is_generic_parameter(generic.name) {
        let _ = self.ctx.fresh_type_var(Some(generic.name));
      }
    }

    let mut generic_mapping: HashMap<Identifier, crate::symbol_table::TyId> = HashMap::new();
    let mut field_types = Vec::new();

    for (field_name, field_expr) in fields.iter_mut() {
      if let Some(field) = variant_fields.iter().find(|f| &f.name == field_name) {
        let expr_type = self.infer_expr(field_expr);
        field_types.push((*field_name, expr_type.clone()));

        self.infer_generic_types(&field.ty, &expr_type, &mut generic_mapping);

        if enum_generics.is_empty() && !self.eq(&field.ty, &expr_type) {
          self.type_mismatch(&field.ty, &expr_type, field_expr.span.clone());
        }
      } else {
        self.error(
          &format!("unknown field `{}`", resolve(field_name)),
          vec![(format!("unknown field `{}`", resolve(field_name)), field_expr.span.clone())],
          vec![],
        );
      }
    }

    if enum_generics.is_empty() {
      Type::Named { name: enum_name, generics: vec![] }
    } else {
      let concrete_generics: Vec<Type> = enum_generics
        .iter()
        .map(|g| {
          generic_mapping.get(&g.name).and_then(|id| Some(Type::Id(*id))).unwrap_or(Type::Inferred)
        })
        .collect();

      let all_generics_mapped =
        concrete_generics.iter().all(|ty| !matches!(ty, Type::Variable { .. }));

      if all_generics_mapped {
        let mut mono_fields: Vec<MonomorphizedStructField> = Vec::new();
        for field in variant_fields.iter() {
          let mut ty = field.ty.clone();
          self.substitute_generic_params(&mut ty, &enum_generics, &concrete_generics);
          mono_fields.push(MonomorphizedStructField {
            name: field.name,
            concrete_ty: self.sym_table.intern_type(ty),
            is_public: field.is_public,
          });
        }

        let mut concrete_tyids: HashMap<Identifier, crate::symbol_table::TyId> = HashMap::new();
        for (g, ty) in enum_generics.iter().zip(concrete_generics.iter()) {
          concrete_tyids.insert(g.name, self.sym_table.intern_type(ty.clone()));
        }

        let mono_symbol = MonomorphizedSymbol {
          base: MonomorphizedSymbolBase {
            original_symbol_id: *variant_id,
            name: enum_name,
            concrete_types: concrete_tyids.clone(),
          },
          kind: MonomorphizedSymbolKind::EnumVariant {
            mono_id: self.sym_table.mono_ids.alloc(()),
            symbol_id: *variant_id,
            fields: mono_fields.clone(),
          },
        };

        let mono_id = self.sym_table.add_monomorphized_symbol(mono_symbol);

        *call_info = Some(CallInfo {
          original_symbol: *variant_id,
          monomorphized_id: Some(mono_id),
          concrete_types: concrete_tyids,
        });

        Type::Named { name: enum_name, generics: concrete_generics }
      } else {
        Type::Named { name: enum_name, generics: concrete_generics }
      }
    }
  }
}
