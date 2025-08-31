use crate::{TypeInference, monomorphization::MonomorphizationData};
use std::collections::HashMap;
use zirael_parser::{
  AstWalker, CallInfo, EnumVariantData, Expr, SymbolId, SymbolKind, Type,
  monomorphized_symbol::MonomorphizedSymbol,
};
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
      let variant_symbol = self.symbol_table.get_symbol_unchecked(variant_id);
      if let SymbolKind::EnumVariant { parent_enum, data, .. } = &variant_symbol.kind {
        let enum_symbol = self.symbol_table.get_symbol_unchecked(parent_enum);
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

    if !enum_generics.is_empty() {
      for generic in enum_generics.iter() {
        if !self.ctx.is_generic_parameter(generic.name) {
          let _type_var = self.ctx.fresh_type_var(Some(generic.name));
        }
      }
    }

    let mut generic_mapping = HashMap::new();
    let mut field_types = Vec::new();

    for (field_name, field_expr) in fields.iter_mut() {
      if let Some(field) = variant_fields.iter().find(|f| &f.name == field_name) {
        let expr_type = self.infer_expr(field_expr);
        field_types.push((*field_name, expr_type.clone()));

        if !enum_generics.is_empty() {
          self.infer_generic_types(&field.ty, &expr_type, &mut generic_mapping);
        }

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
        .map(|g| generic_mapping.get(&g.name).cloned().unwrap_or(Type::Inferred))
        .collect();

      let all_generics_mapped =
        concrete_generics.iter().all(|ty| !matches!(ty, Type::Variable { .. }));

      if all_generics_mapped {
        let mut monomorphized_fields = variant_fields.clone();

        for field in monomorphized_fields.iter_mut() {
          self.substitute_generic_params(&mut field.ty, &enum_generics, &concrete_generics);
        }

        let mut type_mismatches = Vec::new();
        for (field_name, expr_type) in &field_types {
          if let Some(field) = monomorphized_fields.iter().find(|f| &f.name == field_name) {
            if !self.eq(&field.ty, expr_type) {
              if let Some(field_expr) = fields.get(field_name) {
                type_mismatches.push((
                  field.ty.clone(),
                  expr_type.clone(),
                  field_expr.span.clone(),
                ));
              }
            }
          }
        }

        for (expected_ty, found_ty, span) in type_mismatches {
          self.type_mismatch(&expected_ty, &found_ty, span);
        }

        let mut generic_map = HashMap::new();
        for (g, ty) in enum_generics.iter().zip(concrete_generics.iter()) {
          generic_map.insert(g.name, ty.clone());
        }

        let monomorphized_id = self.record_monomorphization_with_id(
          *variant_id,
          &generic_map,
          Some(MonomorphizationData::EnumVariant {
            variant_id: *variant_id,
            fields: monomorphized_fields,
          }),
        );

        *call_info = Some(CallInfo {
          original_symbol: *variant_id,
          monomorphized_id: Some(monomorphized_id),
          concrete_types: generic_map,
        });

        Type::MonomorphizedSymbol(MonomorphizedSymbol {
          id: monomorphized_id,
          display_ty: Box::new(Type::Named { name: enum_name, generics: concrete_generics }),
        })
      } else {
        Type::Named { name: enum_name, generics: concrete_generics }
      }
    }
  }
}
