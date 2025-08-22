use crate::{MonomorphizationData, TypeInference};
use std::{collections::HashMap, vec};
use zirael_parser::{
    AstWalker, CallInfo, Expr, ExprKind, SymbolKind, Type,
    ast::monomorphized_symbol::MonomorphizedSymbol,
};
use zirael_utils::ident_table::{Identifier, resolve};

impl<'reports> TypeInference<'reports> {
    pub fn infer_struct_init(
        &mut self,
        name_expr: &Expr,
        fields: &mut HashMap<Identifier, Expr>,
        call_info: &mut Option<CallInfo>,
    ) -> Type {
        let struct_sym_id = if let ExprKind::Identifier(_, Some(sym_id)) = &name_expr.kind {
            *sym_id
        } else {
            return Type::Error;
        };

        let (struct_fields, generics, symbol_name) = {
            let symbol = self.symbol_table.get_symbol_unchecked(&struct_sym_id);
            if let SymbolKind::Struct { fields: struct_fields, generics, .. } = &symbol.kind {
                (struct_fields.clone(), generics.clone(), symbol.name)
            } else {
                return Type::Error;
            }
        };

        for field in struct_fields.iter() {
            if !fields.contains_key(&field.name) {
                self.error(
                    &format!("missing field `{}`", resolve(&field.name)),
                    vec![(
                        format!("missing field `{}`", resolve(&field.name)),
                        name_expr.span.clone(),
                    )],
                    vec![],
                );
            }
        }

        if !generics.is_empty() {
            for generic in generics.iter() {
                if !self.ctx.is_generic_parameter(generic.name) {
                    let _type_var = self.ctx.fresh_type_var(Some(generic.name));
                }
            }
        }

        let mut generic_mapping = HashMap::new();
        let mut field_types = Vec::new();

        for (field_name, field_expr) in fields.iter_mut() {
            if let Some(field) = struct_fields.iter().find(|f| &f.name == field_name) {
                let expr_type = self.infer_expr(field_expr);
                field_types.push((*field_name, expr_type.clone()));

                if !generics.is_empty() {
                    self.infer_generic_types(&field.ty, &expr_type, &mut generic_mapping);
                }

                if generics.is_empty() && !self.eq(&field.ty, &expr_type) {
                    self.type_mismatch(&field.ty, &expr_type, field_expr.span.clone());
                }
            } else {
                self.error(
                    &format!("unknown field `{}`", resolve(field_name)),
                    vec![(
                        format!("unknown field `{}`", resolve(field_name)),
                        field_expr.span.clone(),
                    )],
                    vec![],
                );
            }
        }

        if generics.is_empty() {
            Type::Named { name: symbol_name, generics: vec![] }
        } else {
            let concrete_generics: Vec<Type> = generics
                .iter()
                .map(|g| {
                    generic_mapping
                        .get(&g.name)
                        .cloned()
                        .expect("generic mapping should contain all generics")
                })
                .collect();

            let all_generics_mapped =
                concrete_generics.iter().all(|ty| !matches!(ty, Type::TypeVariable { .. }));

            if all_generics_mapped {
                let mut monomorphized_fields = struct_fields.clone();

                for field in monomorphized_fields.iter_mut() {
                    self.substitute_generic_params(&mut field.ty, &generics, &concrete_generics);
                }

                let mut type_mismatches = Vec::new();
                for (field_name, expr_type) in &field_types {
                    if let Some(field) = monomorphized_fields.iter().find(|f| &f.name == field_name)
                    {
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
                for (g, ty) in generics.iter().zip(concrete_generics.iter()) {
                    generic_map.insert(g.name, ty.clone());
                }

                let monomorphized_id = self.record_monomorphization_with_id(
                    struct_sym_id,
                    &generic_map,
                    Some(MonomorphizationData::Fields(monomorphized_fields)),
                );

                *call_info = Some(CallInfo {
                    original_symbol: struct_sym_id,
                    monomorphized_id: Some(monomorphized_id),
                    concrete_types: generic_map,
                });

                Type::MonomorphizedSymbol(MonomorphizedSymbol {
                    id: monomorphized_id,
                    display_ty: Box::new(Type::Named {
                        name: symbol_name,
                        generics: concrete_generics,
                    }),
                })
            } else {
                Type::Named { name: symbol_name, generics: concrete_generics }
            }
        }
    }
}
