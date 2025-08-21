mod ctx;
mod errors;
mod expressions;
mod fields;
pub mod monomorphization;
mod structs;
mod substitution;

use crate::{MonomorphizationTable, inference::ctx::TypeInferenceContext};
use id_arena::Arena;
use zirael_parser::{ast::monomorphized_symbol::MonomorphizedSymbol, *};
use zirael_utils::prelude::*;

impl_ast_walker!(TypeInference, {
    ctx: TypeInferenceContext,
    mono_table: MonomorphizationTable,
    mono_arena: Arena<()>,
});

impl<'reports> TypeInference<'reports> {
    pub fn try_monomorphize_named_type(&mut self, ty: Type) -> Type {
        match &ty {
            Type::Named { name, generics } => {
                if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
                    if let SymbolKind::Struct { generics: generic_params, .. } = &symbol.kind {
                        let all_concrete = generics
                            .iter()
                            .all(|g| !matches!(g, Type::TypeVariable { .. } | Type::Inferred));
                        if all_concrete && !generic_params.is_empty() {
                            let mut generic_map = HashMap::new();
                            for (g, ty) in generic_params.iter().zip(generics.iter()) {
                                generic_map.insert(g.name, ty.clone());
                            }
                            let monomorphized_id =
                                self.record_monomorphization_with_id(symbol.id, &generic_map, None);
                            return Type::MonomorphizedSymbol(MonomorphizedSymbol {
                                id: monomorphized_id,
                                display_ty: Box::new(ty),
                            });
                        }
                    }
                }
                ty
            }
            Type::Reference(inner) => {
                Type::Reference(Box::new(self.try_monomorphize_named_type(*inner.clone())))
            }

            _ => ty,
        }
    }

    pub fn eq(&mut self, left: &Type, right: &Type) -> bool {
        let left_mono = self.try_monomorphize_named_type(left.clone());
        let right_mono = self.try_monomorphize_named_type(right.clone());
        self.structural_eq(&left_mono, &right_mono)
    }

    fn structural_eq(&mut self, left: &Type, right: &Type) -> bool {
        match (left, right) {
            (Type::String, Type::String)
            | (Type::Char, Type::Char)
            | (Type::Int, Type::Int)
            | (Type::Int, Type::Float)
            | (Type::Float, Type::Int)
            | (Type::Float, Type::Float)
            | (Type::Bool, Type::Bool)
            | (Type::Void, Type::Void) => true,

            (Type::Pointer(a), Type::Pointer(b)) | (Type::Reference(a), Type::Reference(b)) => {
                self.structural_eq(a, b)
            }

            (Type::Array(a_ty, a_len), Type::Array(b_ty, b_len)) => {
                self.structural_eq(a_ty, b_ty) && a_len == b_len
            }

            (
                Type::Function { params: a_params, return_type: a_ret },
                Type::Function { params: b_params, return_type: b_ret },
            ) => {
                a_params.len() == b_params.len()
                    && a_params.iter().zip(b_params).all(|(a, b)| self.structural_eq(a, b))
                    && self.structural_eq(a_ret, b_ret)
            }

            (
                Type::Named { name: a_name, generics: a_generics },
                Type::Named { name: b_name, generics: b_generics },
            ) => {
                a_name == b_name
                    && a_generics.len() == b_generics.len()
                    && a_generics.iter().zip(b_generics).all(|(a, b)| self.structural_eq(a, b))
            }

            (Type::TypeVariable { name: a_name, .. }, Type::TypeVariable { name: b_name, .. }) => {
                a_name == b_name
            }

            (Type::Inferred, Type::Inferred) | (Type::Error, _) | (_, Type::Error) => true,

            (Type::MonomorphizedSymbol(sym), Type::MonomorphizedSymbol(sym2)) => {
                if let (Some(mono1), Some(mono2)) =
                    (self.mono_table.get_entry(sym.id), self.mono_table.get_entry(sym2.id))
                {
                    if mono1.original_id != mono2.original_id {
                        return false;
                    }

                    let concrete_types1 = mono1.concrete_types.clone();
                    let concrete_types2 = mono2.concrete_types.clone();

                    for (name, value) in &concrete_types1 {
                        let Some(mono2_value) = concrete_types2.get(name) else { return false };
                        if !self.eq(value, mono2_value) {
                            return false;
                        }
                    }

                    true
                } else {
                    false
                }
            }

            _ => false,
        }
    }

    fn get_struct_type_for_method(&self, func: &Function) -> Option<Type> {
        if let Some(func_symbol) = self.symbol_table.lookup_symbol(&func.name) {
            if let Some(struct_symbol_id) = self.symbol_table.is_a_method(func_symbol.id) {
                let struct_symbol = self.symbol_table.get_symbol_unchecked(&struct_symbol_id);

                if let SymbolKind::Struct { generics, .. } = &struct_symbol.kind {
                    let generic_names = generics
                        .iter()
                        .map(|g| Type::Named { name: g.name.clone(), generics: vec![] })
                        .collect::<Vec<_>>();

                    return Some(Type::Named { name: struct_symbol.name, generics: generic_names });
                }
            }
        }
        None
    }

    fn resolve_self_parameter_type(&self, param: &Parameter, struct_type: &Option<Type>) -> Type {
        match struct_type {
            Some(struct_ty) => match &param.ty {
                Type::Reference(inner) => Type::Reference(Box::new(struct_ty.clone())),
                _ => struct_ty.clone(),
            },
            None => param.ty.clone(),
        }
    }

    fn get_generic_type_vars(
        &mut self,
        generics: &Vec<GenericParameter>,
    ) -> HashMap<Identifier, Type> {
        if !generics.is_empty() {
            generics
                .iter()
                .map(|g| (g.name.clone(), self.ctx.fresh_type_var(Some(g.name.clone()))))
                .collect::<HashMap<_, _>>()
        } else {
            HashMap::new()
        }
    }
}

impl<'reports> AstWalker<'reports> for TypeInference<'reports> {
    fn walk_expr(&mut self, expr: &mut Expr) {
        let ty = self.infer_expr(expr);
        expr.ty = self.try_monomorphize_named_type(ty);
    }

    fn visit_var_decl(&mut self, _var_decl: &mut VarDecl) {
        self.infer_variable(_var_decl);
    }

    fn walk_function(&mut self, func: &mut Function) {
        self.push_scope(ScopeType::Function(func.id));

        self.walk_function_modifiers(&mut func.modifiers);
        self.walk_function_signature(&mut func.signature);

        let generic_type_vars = self.get_generic_type_vars(&func.signature.generics);

        let struct_type = self.get_struct_type_for_method(func);
        if !func.signature.is_static()
            && let Some(param) = func.signature.parameters.get_mut(0)
        {
            param.ty = self.resolve_self_parameter_type(&param, &struct_type);
        };

        for param in &mut func.signature.parameters {
            if let Some(param_id) = param.symbol_id {
                let param_type = if !generic_type_vars.is_empty() {
                    self.substitute_type_with_map(&param.ty, &generic_type_vars)
                } else {
                    param.ty.clone()
                };

                let mono_type = self.try_monomorphize_named_type(param_type);
                param.ty = mono_type.clone();
                self.ctx.add_variable(param_id, mono_type);
            }
        }

        if let Some(body) = &mut func.body {
            let body_ty = self.infer_expr(body);

            let return_type = if !generic_type_vars.is_empty() {
                self.substitute_type_with_map(&func.signature.return_type, &generic_type_vars)
            } else {
                func.signature.return_type.clone()
            };

            if !self.eq(&body_ty, &return_type) {
                self.return_type_mismatch(&return_type, &body_ty, func.span.clone());
            }
        }

        self.pop_scope();
    }

    fn walk_struct_declaration(&mut self, _struct: &mut StructDeclaration) {
        self.push_scope(ScopeType::Struct(_struct.id));
        let generic_type_vars = self.get_generic_type_vars(&_struct.generics);

        for generic in &mut _struct.generics {
            
        }

        for field in &mut _struct.fields {
            self.walk_struct_field(field);
        }

        for item in &mut _struct.methods {
            self.walk_item(item);
        }

        self.pop_scope();
    }

    fn walk_struct_field(&mut self, field: &mut StructField) {
        field.ty = self.try_monomorphize_named_type(field.ty.clone());

        self.visit_struct_field(field);
        self.walk_type(&mut field.ty);

        for attr in &mut field.attributes {
            self.walk_attribute(attr);
        }
    }
}
