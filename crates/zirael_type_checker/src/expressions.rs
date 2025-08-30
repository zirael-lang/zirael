use crate::{
    TypeInference, monomorphization::MonomorphizationData, unification::UnificationResult,
};
use std::collections::HashMap;
use zirael_parser::{
    AstId, AstWalker, BinaryOp, CallInfo, EnumVariantData, Expr, ExprKind, Literal, ScopeType,
    Stmt, StmtKind, SymbolId, SymbolKind, Type, UnaryOp, VarDecl, WalkerContext,
};
use zirael_utils::prelude::{Colorize, Identifier, Span, resolve, warn};

impl<'reports> TypeInference<'reports> {
    pub(crate) fn expect_type(
        &mut self,
        expected: &Type,
        actual: &Type,
        span: &Span,
        context: &str,
    ) -> bool {
        if !self.eq(expected, actual) {
            self.error(
                &format!(
                    "type mismatch in {}: expected {}, found {}",
                    context,
                    self.format_type(expected),
                    self.format_type(actual)
                ),
                vec![(format!("in this {}", context), span.clone())],
                vec![],
            );
            false
        } else {
            true
        }
    }

    fn check_call_args(&mut self, params: &[Type], args: &mut [Expr], span: &Span) -> bool {
        if args.len() != params.len() {
            self.error(
                &format!(
                    "wrong number of arguments: expected {}, found {}",
                    params.len(),
                    args.len()
                ),
                vec![("in this call".to_string(), span.clone())],
                vec![],
            );
            return false;
        }
        let mut valid = true;
        for (i, (arg, param_type)) in args.iter_mut().zip(params.iter()).enumerate() {
            let arg_type = self.infer_expr(arg);
            self.try_monomorphize_named_type(&mut arg.ty);
            if !self.expect_type(param_type, &mut arg.ty, &arg.span, &format!("argument {}", i + 1))
            {
                valid = false;
            }
        }
        valid
    }

    pub fn infer_expr(&mut self, expr: &mut Expr) -> Type {
        let ty = match &mut expr.kind {
            ExprKind::Literal(lit) => self.infer_literal(lit),
            ExprKind::Identifier(_, symbol_id) => {
                if let Some(sym_id) = symbol_id {
                    self.infer_identifier(*sym_id, &expr.span)
                } else {
                    self.error(
                        "unresolved identifier",
                        vec![("here".to_string(), expr.span.clone())],
                        vec![],
                    );
                    Type::Error
                }
            }
            ExprKind::Assign(lhs, rhs) => self.infer_assignment(lhs, rhs),
            ExprKind::AssignOp(lhs, op, rhs) => self.infer_assign_op(lhs, op, rhs),
            ExprKind::Block(stmts) => self.infer_block(stmts, expr.id),
            ExprKind::Unary(op, expr) => self.infer_unary(op, expr),
            ExprKind::Binary { left, op, right } => self.infer_binary(left, op, right),
            ExprKind::Call { callee, args, call_info } => self.infer_call(callee, args, call_info),
            ExprKind::Paren(expr) => self.infer_expr(expr),
            ExprKind::StructInit { name, fields, call_info } => {
                self.infer_struct_init(name, fields, call_info)
            }
            ExprKind::FieldAccess(fields) => self.infer_field_access(fields),
            ExprKind::IndexAccess(expr, index) => self.infer_index_access(expr, index),
            ExprKind::MethodCall { chain, args, call_info } => {
                self.infer_method_call(chain, args, call_info)
            }
            ExprKind::StaticCall { callee, args, call_info } => {
                self.infer_static_call(callee, args, call_info)
            }
            ExprKind::Ternary { true_expr, false_expr, condition } => {
                self.infer_ternary(condition, true_expr, false_expr)
            }

            _ => {
                warn!("unimplemented expr: {:#?}", expr);
                Type::Error
            }
        };
        expr.ty = ty.clone();
        ty
    }

    fn infer_ternary(
        &mut self,
        condition: &mut Expr,
        true_expr: &mut Expr,
        false_expr: &mut Expr,
    ) -> Type {
        let condition_ty = self.infer_expr(condition);
        let true_ty = self.infer_expr(true_expr);
        let false_ty = self.infer_expr(false_expr);

        self.expect_type(&Type::Bool, &condition_ty, &condition.span, "ternary condition");

        match self.unify_types(&true_ty, &false_ty) {
            UnificationResult::Identical(ty) => ty,
            UnificationResult::Unified(ty) => {
                self.update_monomorphization_with_resolved_types(true_expr, &ty);
                self.update_monomorphization_with_resolved_types(false_expr, &ty);
                ty
            }
            UnificationResult::Incompatible => {
                self.ternary_error(&true_ty, &false_ty, true_expr, false_expr)
            }
        }
    }

    fn ternary_error(
        &mut self,
        true_ty: &Type,
        false_ty: &Type,
        true_expr: &mut Expr,
        false_expr: &mut Expr,
    ) -> Type {
        let true_expr_ty = self.format_type(true_ty).dimmed().bold();
        let false_expr_ty = self.format_type(false_ty).dimmed().bold();
        self.error(
            &format!(
                "ternary operator branches have incompatible types: {true_expr_ty} and {false_expr_ty}",
            ),
            vec![
                (format!("true branch has type {true_expr_ty}"), true_expr.span.clone()),
                (format!("false branch has type {false_expr_ty}"), false_expr.span.clone()),
            ],
            vec![],
        );
        Type::Error
    }

    fn infer_assign_op(&mut self, lhs: &mut Expr, op: &BinaryOp, rhs: &mut Expr) -> Type {
        let lhs_ty = self.infer_expr(lhs);
        let rhs_ty = self.infer_expr(rhs);

        let result_ty = match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                if lhs_ty.is_int() && rhs_ty.is_int() {
                    lhs_ty.clone()
                } else if (lhs_ty.is_float() && rhs_ty.is_int())
                    || (lhs_ty.is_int() && rhs_ty.is_float())
                {
                    Type::Float
                } else if lhs_ty.is_float() && rhs_ty.is_float() {
                    Type::Float
                } else {
                    self.error(
                        &format!(
                            "cannot perform arithmetic assignment operation on {} and {}",
                            self.format_type(&lhs_ty).dimmed().bold(),
                            self.format_type(&rhs_ty).dimmed().bold()
                        ),
                        vec![("here".to_string(), lhs.span.clone())],
                        vec![],
                    );
                    Type::Error
                }
            }

            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Shl
            | BinaryOp::Shr => {
                if lhs_ty.is_int() && rhs_ty.is_int() {
                    lhs_ty.clone()
                } else {
                    self.error(
                        &format!(
                            "bitwise assignment operations can only be applied to integer types, found {} and {}",
                            self.format_type(&lhs_ty).dimmed().bold(),
                            self.format_type(&rhs_ty).dimmed().bold()
                        ),
                        vec![("here".to_string(), lhs.span.clone())],
                        vec![],
                    );
                    Type::Error
                }
            }

            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Gt
            | BinaryOp::Ge => {
                self.error(
                    "comparison operators cannot be used in assignment operations",
                    vec![("here".to_string(), lhs.span.clone())],
                    vec![],
                );
                Type::Error
            }

            BinaryOp::And | BinaryOp::Or => {
                self.error(
                    "logical operators cannot be used in assignment operations",
                    vec![("here".to_string(), lhs.span.clone())],
                    vec![],
                );
                Type::Error
            }
        };

        if result_ty != Type::Error
            && !self.expect_type(&lhs_ty, &result_ty, &lhs.span, "assignment operation")
        {
            return Type::Error;
        }

        Type::Void
    }

    fn infer_unary(&mut self, op: &UnaryOp, expr: &mut Expr) -> Type {
        let operand_ty = self.infer_expr(expr);
        match op {
            UnaryOp::Box => operand_ty,
            UnaryOp::Deref => {
                if let Type::Reference(reference) = operand_ty {
                    *reference
                } else {
                    self.error(
                        "deref can be only applied to reference types",
                        vec![("attempted to dereference here".to_string(), expr.span.clone())],
                        vec![],
                    );
                    Type::Error
                }
            }
            UnaryOp::BitwiseNot | UnaryOp::Minus => {
                if let Type::Int = operand_ty {
                    operand_ty
                } else {
                    self.error(
                        &format!(
                            "unary operator can only be applied to integer types, found {}",
                            self.format_type(&operand_ty).dimmed().bold()
                        ),
                        vec![(
                            "attempted to apply unary operator here".to_string(),
                            expr.span.clone(),
                        )],
                        vec![],
                    );
                    Type::Error
                }
            }
            UnaryOp::Not => Type::Bool,
            // correctness of operand is check in memory pas
            UnaryOp::Ref => Type::Reference(Box::new(operand_ty)),
        }
    }

    fn infer_binary(&mut self, left: &mut Expr, op: &BinaryOp, right: &mut Expr) -> Type {
        let left_type = self.infer_expr(left);
        let right_type = self.infer_expr(right);

        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                if left_type.is_int() && right_type.is_int() {
                    left_type
                } else {
                    if (left_type.is_float() && right_type.is_int())
                        || (left_type.is_int() && right_type.is_float())
                    {
                        Type::Float
                    } else if left_type.is_float() && right_type.is_float() {
                        Type::Float
                    } else {
                        self.error(
                            &format!(
                                "cannot perform arithmetic operation on {} and {}",
                                self.format_type(&left_type).dimmed().bold(),
                                self.format_type(&right_type).dimmed().bold()
                            ),
                            vec![("here".to_string(), left.span.clone())],
                            vec![],
                        );
                        Type::Error
                    }
                }
            }

            BinaryOp::Eq | BinaryOp::Ne => {
                // this might need more logic than just .eq
                if left_type.eq(&right_type) {
                    Type::Bool
                } else {
                    self.error(
                        &format!(
                            "cannot compare {} and {}",
                            self.format_type(&left_type).dimmed().bold(),
                            self.format_type(&right_type).dimmed().bold()
                        ),
                        vec![("here".to_string(), left.span.clone())],
                        vec![],
                    );
                    Type::Bool
                }
            }

            BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                if left_type.is_numeric() && right_type.is_numeric() {
                    Type::Bool
                } else {
                    self.error(
                        &format!(
                            "cannot compare {} and {}",
                            self.format_type(&left_type).dimmed().bold(),
                            self.format_type(&right_type).dimmed().bold()
                        ),
                        vec![("here".to_string(), left.span.clone())],
                        vec![],
                    );
                    Type::Bool
                }
            }

            BinaryOp::And | BinaryOp::Or => Type::Bool,

            _ => {
                warn!("Unimplemented binary operator: {:?}", op);
                Type::Error
            }
        }
    }

    fn infer_assignment(&mut self, lhs: &mut Expr, rhs: &mut Expr) -> Type {
        let lhs_ty = self.infer_expr(lhs);
        let rhs_ty = self.infer_expr(rhs);
        if !self.expect_type(&lhs_ty, &rhs_ty, &lhs.span, "assignment") {
            return Type::Error;
        }
        Type::Void
    }

    fn infer_block(&mut self, stmts: &mut [Stmt], id: AstId) -> Type {
        self.push_scope(ScopeType::Block(id));
        if stmts.is_empty() {
            self.pop_scope();
            return Type::Void;
        }

        let mut block_type = Type::Void;

        for stmt in stmts.iter_mut() {
            match &mut stmt.0 {
                StmtKind::Expr(expr) => {
                    self.infer_expr(expr);
                }
                StmtKind::Return(ret) => {
                    if let Some(expr) = ret.value.as_mut() {
                        block_type = self.infer_expr(expr);
                    } else {
                        block_type = Type::Void;
                    }
                    break;
                }
                StmtKind::Var(var) => {
                    self.infer_variable(var);
                }
            }
        }
        self.pop_scope();

        block_type
    }

    pub fn infer_call(
        &mut self,
        callee: &mut Expr,
        args: &mut Vec<Expr>,
        call_info: &mut Option<CallInfo>,
    ) -> Type {
        if let Some((_, Some(sym_id))) = callee.as_identifier_mut() {
            let sym = self.symbol_table().get_symbol_unchecked(sym_id);
            if let SymbolKind::Function { signature, .. } = &sym.kind {
                let signature = &mut signature.clone();

                let expected_arg_count = if signature.is_static() {
                    signature.parameters.len()
                } else {
                    signature.parameters.len() - 1
                };

                if args.len() != expected_arg_count {
                    self.error(
                        &format!(
                            "wrong number of arguments: expected {}, found {}",
                            expected_arg_count,
                            args.len()
                        ),
                        vec![("in this call".to_string(), callee.span.clone())],
                        vec![],
                    );
                    return Type::Error;
                }

                for arg in args.iter_mut() {
                    self.infer_expr(arg);
                    self.try_monomorphize_named_type(&mut arg.ty);
                }

                let mut generic_mapping = HashMap::new();

                if !signature.generics.is_empty() {
                    for generic in signature.generics.iter() {
                        if !self.ctx.is_generic_parameter(generic.name) {
                            let _type_var = self.ctx.fresh_type_var(Some(generic.name));
                        }
                    }
                }

                let params_to_check = if signature.is_static() {
                    &mut signature.parameters[..]
                } else {
                    &mut signature.parameters[1..]
                };

                let mut valid = true;
                for (arg, param) in args.iter().zip(params_to_check.iter()) {
                    self.infer_generic_types(&param.ty, &arg.ty, &mut generic_mapping);
                }

                let mut concrete_params = params_to_check.to_vec();
                for param in concrete_params.iter_mut() {
                    self.substitute_type_with_map(&mut param.ty, &generic_mapping);
                }

                for (i, (arg, param)) in args.iter().zip(concrete_params.iter()).enumerate() {
                    if !self.expect_type(
                        &param.ty,
                        &arg.ty,
                        &arg.span,
                        &format!("argument {}", i + 1),
                    ) {
                        valid = false;
                    }
                }
                signature.parameters = concrete_params.clone();

                if !valid {
                    return Type::Error;
                }

                let mut return_type = signature.return_type.clone();
                self.substitute_type_with_map(&mut return_type, &generic_mapping);
                signature.return_type = return_type.clone();

                let monomorphized_id = if !generic_mapping.is_empty() && valid {
                    let all_generics_mapped =
                        signature.generics.iter().all(|g| generic_mapping.contains_key(&g.name));

                    if all_generics_mapped {
                        Some(self.record_monomorphization_with_id(
                            *sym_id,
                            &generic_mapping,
                            Some(MonomorphizationData::Signature(signature.clone())),
                        ))
                    } else {
                        None
                    }
                } else {
                    None
                };

                *call_info = Some(CallInfo {
                    original_symbol: *sym_id,
                    monomorphized_id,
                    concrete_types: generic_mapping.clone(),
                });

                return return_type;
            } else {
                self.error(
                    &format!("cannot call non-function type: {}", sym.kind.name()),
                    vec![("in this call".to_string(), callee.span.clone())],
                    vec![],
                );
                return Type::Error;
            }
        }

        let callee_type = self.infer_expr(callee);
        match callee_type {
            Type::Function { params, return_type } => {
                if !self.check_call_args(&params, args, &callee.span) {
                    return Type::Error;
                }
                *return_type
            }
            _ => {
                self.error(
                    &format!("cannot call non-function type: {}", self.format_type(&callee_type)),
                    vec![("in this call".to_string(), callee.span.clone())],
                    vec![],
                );
                Type::Error
            }
        }
    }

    pub fn infer_variable(&mut self, decl: &mut VarDecl) -> Type {
        let value_ty = &self.infer_expr(&mut decl.value);
        let variable_ty =
            if let Type::Inferred = decl.ty { value_ty.clone() } else { decl.ty.clone() };

        if let Some(symbol_id) = decl.symbol_id {
            let symbol = self.symbol_table.get_symbol_unchecked(&symbol_id);
            self.ctx.add_variable(symbol.id, variable_ty.clone());
            if !self.expect_type(&variable_ty, value_ty, &decl.span, "variable declaration") {
                return Type::Error;
            }

            if self.eq(&variable_ty, &Type::Void) {
                self.error(
                    "cannot initialize variable with void type",
                    vec![("here".to_string(), decl.span.clone())],
                    vec![],
                );
            }

            decl.ty = value_ty.clone();
            variable_ty.clone()
        } else {
            self.error(
                "unresolved variable declaration",
                vec![("here".to_string(), decl.span.clone())],
                vec![],
            );
            Type::Error
        }
    }

    fn infer_identifier(&mut self, sym_id: SymbolId, span: &Span) -> Type {
        let sym = self.symbol_table().get_symbol_unchecked(&sym_id);

        match &sym.kind {
            SymbolKind::Parameter { .. } | SymbolKind::Variable { .. } => {
                if let Some(var_type) = self.ctx.get_variable(sym_id) {
                    var_type.clone()
                } else {
                    self.error(
                        &format!("variable '{}' used before initialization", resolve(&sym.name)),
                        vec![("here".to_string(), span.clone())],
                        vec![],
                    );
                    Type::Error
                }
            }
            _ => {
                self.error(
                    &format!("identifier '{}' cannot be used in this context", resolve(&sym.name)),
                    vec![("here".to_string(), span.clone())],
                    vec![],
                );
                Type::Error
            }
        }
    }

    fn infer_literal(&mut self, lit: &Literal) -> Type {
        let ty = match lit {
            Literal::Bool(_) => Type::Bool,
            Literal::Char(_) => Type::Char,
            Literal::Float(_) => Type::Float,
            Literal::Integer(_) => Type::Int,
            Literal::String(_) => Type::String,
        };
        ty
    }

    pub fn infer_generic_types(
        &mut self,
        expected: &Type,
        actual: &Type,
        mapping: &mut HashMap<Identifier, Type>,
    ) {
        match (expected, actual) {
            (Type::Named { name, generics }, concrete_type) => {
                if generics.is_empty() && self.ctx.is_generic_parameter(*name) {
                    mapping.insert(*name, concrete_type.clone());
                } else if !generics.is_empty() {
                    if let Type::Named { name: a_name, generics: a_generics } = concrete_type {
                        if name == a_name && generics.len() == a_generics.len() {
                            for (e_gen, a_gen) in generics.iter().zip(a_generics.iter()) {
                                self.infer_generic_types(e_gen, a_gen, mapping);
                            }
                        }
                    }
                }
            }

            (Type::TypeVariable { name, .. }, concrete_type) => {
                mapping.insert(*name, concrete_type.clone());
            }

            (Type::Pointer(e_inner), Type::Pointer(a_inner)) => {
                self.infer_generic_types(e_inner, a_inner, mapping);
            }

            (Type::Reference(e_inner), Type::Reference(a_inner)) => {
                self.infer_generic_types(e_inner, a_inner, mapping);
            }

            (Type::Array(e_inner, _), Type::Array(a_inner, _)) => {
                self.infer_generic_types(e_inner, a_inner, mapping);
            }

            _ => {}
        }
    }
}
