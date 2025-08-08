mod ctx;
mod errors;
mod expressions;

use crate::inference::ctx::TypeInferenceContext;
use zirael_parser::*;
use zirael_utils::prelude::*;

impl_ast_walker!(TypeInference, {
    ctx: TypeInferenceContext,
});

impl<'reports> TypeInference<'reports> {
    pub fn eq(&mut self, left: &Type, right: &Type) -> bool {
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
                self.eq(a, b)
            }

            (Type::Array(a_ty, a_len), Type::Array(b_ty, b_len)) => {
                self.eq(a_ty, b_ty) && a_len == b_len
            }

            (
                Type::Function { params: a_params, return_type: a_ret },
                Type::Function { params: b_params, return_type: b_ret },
            ) => {
                a_params.len() == b_params.len()
                    && a_params.iter().zip(b_params).all(|(a, b)| self.eq(a, b))
                    && self.eq(a_ret, b_ret)
            }

            (
                Type::Named { name: a_name, generics: a_generics },
                Type::Named { name: b_name, generics: b_generics },
            ) => {
                a_name == b_name
                    && a_generics.len() == b_generics.len()
                    && a_generics.iter().zip(b_generics).all(|(a, b)| self.eq(a, b))
            }

            (Type::Inferred, Type::Inferred) | (Type::Error, _) | (_, Type::Error) => true,

            _ => false,
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

        if let Some(body) = &mut func.body {
            let body_ty = self.infer_expr(body);

            if !self.eq(&body_ty, &func.signature.return_type) {
                self.return_type_mismatch(&func.signature.return_type, &body_ty, func.span.clone());
            }
        }

        self.pop_scope();
    }
}
