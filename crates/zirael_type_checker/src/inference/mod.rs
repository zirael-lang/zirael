mod ctx;
mod errors;
mod expressions;
pub mod monomorphization;
mod structs;
mod substitution;

use crate::{MonomorphizationTable, inference::ctx::TypeInferenceContext};
use id_arena::Arena;
use zirael_parser::*;
use zirael_utils::prelude::*;

impl_ast_walker!(TypeInference, {
    ctx: TypeInferenceContext,
    mono_table: MonomorphizationTable,
    mono_arena: Arena<()>,
});

impl<'reports> TypeInference<'reports> {
    pub fn eq(&mut self, left: &Type, right: &Type) -> bool {
        let empty_map = HashMap::new();
        let left_sub = self.substitute_type_with_map(left, &empty_map);
        let right_sub = self.substitute_type_with_map(right, &empty_map);
        self.structural_eq(&left_sub, &right_sub)
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

            _ => false,
        }
    }
}

impl<'reports> TypeInference<'reports> {
    fn substitute_type_variables(&self, ty: &Type, type_vars: &HashMap<Identifier, Type>) -> Type {
        match ty {
            Type::Named { name, generics } if generics.is_empty() => {
                if let Some(var_type) = type_vars.get(name) {
                    return var_type.clone();
                }

                ty.clone()
            }
            Type::Named { name, generics } => Type::Named {
                name: *name,
                generics: generics
                    .iter()
                    .map(|g| self.substitute_type_variables(g, type_vars))
                    .collect(),
            },
            Type::Pointer(inner) => {
                Type::Pointer(Box::new(self.substitute_type_variables(inner, type_vars)))
            }
            Type::Reference(inner) => {
                Type::Reference(Box::new(self.substitute_type_variables(inner, type_vars)))
            }
            Type::Array(inner, size) => {
                Type::Array(Box::new(self.substitute_type_variables(inner, type_vars)), *size)
            }
            Type::Function { params, return_type } => Type::Function {
                params: params
                    .iter()
                    .map(|p| self.substitute_type_variables(p, type_vars))
                    .collect(),
                return_type: Box::new(self.substitute_type_variables(return_type, type_vars)),
            },
            _ => ty.clone(),
        }
    }
}

impl<'reports> AstWalker<'reports> for TypeInference<'reports> {
    fn walk_expr(&mut self, expr: &mut Expr) {
        self.infer_expr(expr);
    }

    fn visit_var_decl(&mut self, _var_decl: &mut VarDecl) {
        self.infer_variable(_var_decl);
    }

    fn walk_function(&mut self, func: &mut Function) {
        self.push_scope(ScopeType::Function(func.id));

        self.walk_function_modifiers(&mut func.modifiers);
        self.walk_function_signature(&mut func.signature);

        let generic_type_vars = if !func.signature.generics.is_empty() {
            func.signature
                .generics
                .iter()
                .map(|g| (g.name, self.ctx.fresh_type_var(Some(g.name))))
                .collect::<HashMap<_, _>>()
        } else {
            HashMap::new()
        };

        for param in &func.signature.parameters {
            if let Some(param_id) = param.symbol_id {
                let param_type = if !generic_type_vars.is_empty() {
                    self.substitute_type_variables(&param.ty, &generic_type_vars)
                } else {
                    param.ty.clone()
                };

                self.ctx.add_variable(param_id, param_type);
            }
        }

        if let Some(body) = &mut func.body {
            let body_ty = self.infer_expr(body);

            let return_type = if !generic_type_vars.is_empty() {
                self.substitute_type_variables(&func.signature.return_type, &generic_type_vars)
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

        let generic_type_vars = if !_struct.generics.is_empty() {
            _struct
                .generics
                .iter()
                .map(|g| {
                    let type_var = self.ctx.fresh_type_var(Some(g.name));

                    if !g.constraints.is_empty() {
                        warn!("constraints on generic parameters are not supported yet")
                    }

                    (g.name, type_var)
                })
                .collect::<HashMap<_, _>>()
        } else {
            HashMap::new()
        };

        for generic in &mut _struct.generics {
            self.walk_generic_parameter(generic);
        }

        for field in &mut _struct.fields {
            if !generic_type_vars.is_empty() {
                let _field_type = self.substitute_type_variables(&field.ty, &generic_type_vars);
            }

            self.walk_struct_field(field);
        }

        for item in &mut _struct.methods {
            self.walk_item(item);
        }

        self.pop_scope();
    }
}
