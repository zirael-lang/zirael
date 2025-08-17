use crate::TypeInference;
use std::collections::HashMap;
use zirael_parser::{
    AstWalker, CallInfo, Expr, ExprKind, SymbolId, SymbolKind, Type,
    ast::monomorphized_symbol::MonomorphizedSymbol,
};
use zirael_utils::ident_table::{Identifier, resolve};

impl<'reports> TypeInference<'reports> {
    fn unify_struct_generics(
        &mut self,
        struct_sym_id: SymbolId,
        field_values: &mut HashMap<Identifier, Expr>,
    ) -> Type {
        let symbol = self.symbol_table.get_symbol_unchecked(&struct_sym_id);

        if let SymbolKind::Struct { fields, generics } = &symbol.kind {
            if generics.is_empty() {
                return Type::Named { name: symbol.name, generics: vec![] };
            }

            let mut generic_mapping = HashMap::new();

            for field in fields {
                if let Some(mut expr) = field_values.get_mut(&field.name) {
                    let field_type = &field.ty;
                    let expr_type = self.infer_expr(&mut expr);
                    self.infer_generic_types(field_type, &expr_type, &mut generic_mapping);
                }
            }

            // For each generic parameter, use the inferred type if available, otherwise create a fresh type variable.
            let concrete_generics: Vec<Type> = generics
                .iter()
                .map(|g| {
                    if let Some(ty) = generic_mapping.get(&g.name) {
                        ty.clone()
                    } else {
                        Type::TypeVariable { id: self.ctx.next_type_var_id(), name: g.name }
                    }
                })
                .collect();

            // If all generics are mapped (i.e., no type variables remain), return MonomorphizedSymbol
            let all_generics_mapped =
                concrete_generics.iter().all(|ty| !matches!(ty, Type::TypeVariable { .. }));
            if all_generics_mapped {
                let mut generic_map = HashMap::new();
                for (g, ty) in generics.iter().zip(concrete_generics.iter()) {
                    generic_map.insert(g.name, ty.clone());
                }
                let monomorphized_id =
                    self.record_monomorphization_with_id(struct_sym_id, &generic_map);
                return Type::MonomorphizedSymbol(MonomorphizedSymbol {
                    id: monomorphized_id,
                    display_ty: Box::new(Type::Named {
                        name: symbol.name,
                        generics: concrete_generics,
                    }),
                });
            }

            // Otherwise, return Named (with type variables)
            return Type::Named { name: symbol.name, generics: concrete_generics };
        }

        Type::Named { name: symbol.name, generics: vec![] }
    }

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

        let struct_type = self.unify_struct_generics(struct_sym_id, fields);

        let symbol = self.symbol_table.get_symbol_unchecked(&struct_sym_id);
        if let SymbolKind::Struct { fields: struct_fields, .. } = &symbol.kind {
            for field in struct_fields {
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

            for (field_name, field_expr) in fields {
                if let Some(field) = struct_fields.iter().find(|f| &f.name == field_name) {
                    let expr_type = self.infer_expr(field_expr);
                    let field_type = match &struct_type {
                        Type::MonomorphizedSymbol(mono) => {
                            // Use the generics from the display_ty
                            if let Type::Named { generics: type_generics, .. } = &*mono.display_ty {
                                self.substitute_generic_params(
                                    &field.ty,
                                    &symbol.kind,
                                    type_generics,
                                )
                            } else {
                                field.ty.clone()
                            }
                        }
                        Type::Named { generics: type_generics, .. } => {
                            self.substitute_generic_params(&field.ty, &symbol.kind, type_generics)
                        }
                        _ => field.ty.clone(),
                    };

                    if !self.eq(&field_type, &expr_type) {
                        self.type_mismatch(&field_type, &expr_type, field_expr.span.clone());
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

            // Always return the monomorphized type if possible
            if let Type::MonomorphizedSymbol(_) = &struct_type {
                *call_info = Some(CallInfo {
                    original_symbol: struct_sym_id,
                    monomorphized_id: match &struct_type {
                        Type::MonomorphizedSymbol(mono) => Some(mono.id),
                        _ => None,
                    },
                    concrete_types: HashMap::new(), // Optionally fill with generics if needed
                });
                return struct_type;
            }

            struct_type
        } else {
            Type::Error
        }
    }
}
