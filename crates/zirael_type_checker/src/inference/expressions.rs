use crate::inference::TypeInference;
use std::collections::HashMap;
use zirael_parser::{
    AstId, AstWalker, BinaryOp, CallInfo, Expr, ExprKind, Literal, ScopeType, Stmt, StmtKind,
    StructField, SymbolId, SymbolKind, Type, UnaryOp, VarDecl, WalkerContext,
};
use zirael_utils::prelude::{Colorize, Identifier, Span, resolve, warn};

impl<'reports> TypeInference<'reports> {
    fn expect_type(&mut self, expected: &Type, actual: &Type, span: &Span, context: &str) -> bool {
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
            let mono_type = self.try_monomorphize_named_type(arg_type);
            arg.ty = mono_type.clone();
            if !self.expect_type(param_type, &mono_type, &arg.span, &format!("argument {}", i + 1))
            {
                valid = false;
            }
        }
        valid
    }

    pub fn infer_expr(&mut self, expr: &mut Expr) -> Type {
        let ty = match &mut expr.kind {
            ExprKind::Literal(lit) => self.infer_literal(lit),
            ExprKind::Identifier(_, symbol_id) => self.infer_identifier(symbol_id.unwrap()),
            ExprKind::Assign(lhs, rhs) => self.infer_assignment(lhs, rhs),
            ExprKind::Block(stmts) => self.infer_block(stmts, expr.id),
            ExprKind::Unary(op, expr) => self.infer_unary(op, expr),
            ExprKind::Binary { left, op, right } => self.infer_binary(left, op, right),
            ExprKind::Call { callee, args, call_info } => self.infer_call(callee, args, call_info),
            ExprKind::Paren(expr) => self.infer_expr(expr),
            ExprKind::StructInit { name, fields, call_info } => {
                self.infer_struct_init(name, fields, call_info)
            }
            ExprKind::FieldAccess(fields) => self.infer_field(fields),
            _ => {
                warn!("unimplemented expr: {:#?}", expr);
                Type::Error
            }
        };
        expr.ty = ty.clone();
        ty
    }

    fn check_field(&mut self, current_type: &Type, expr_fields: &[Expr], index: usize) -> Type {
        if index >= expr_fields.len() {
            return current_type.clone();
        }

        let (field_name, _) = match expr_fields[index].as_identifier() {
            Some(id) => id,
            None => {
                self.non_struct_type(expr_fields[index].span.clone());
                return Type::Error;
            }
        };

        let field_span = expr_fields[index].span.clone();
        let field_type = self.get_field_type(current_type, &resolve(&field_name), field_span);

        match field_type {
            Type::Error => Type::Error,
            _ => self.check_field(&field_type, expr_fields, index + 1),
        }
    }

    fn get_field_type(&mut self, current_type: &Type, field_name: &str, field_span: Span) -> Type {
        let struct_fields = match current_type {
            Type::Named { name, .. } => {
                let sym = match self.symbol_table.lookup_symbol(name) {
                    Some(sym) => sym,
                    None => return Type::Error,
                };

                match sym.kind.clone() {
                    SymbolKind::Struct { fields, .. } => fields,
                    _ => {
                        self.non_struct_type(field_span);
                        return Type::Error;
                    }
                }
            }
            Type::MonomorphizedSymbol(sym) => {
                let entry = self.mono_table.get_entry(sym.id);
                let sym = self.symbol_table.get_symbol_unchecked(&entry.original_id);

                match sym.kind.clone() {
                    SymbolKind::Struct { fields, .. } => {
                        entry.monomorphized_fields.clone().unwrap_or(fields)
                    }
                    _ => {
                        self.non_struct_type(field_span);
                        return Type::Error;
                    }
                }
            }
            _ => {
                self.non_struct_type(field_span);
                return Type::Error;
            }
        };

        match struct_fields.iter().find(|f| resolve(&f.name) == field_name) {
            Some(field) => field.ty.clone(),
            None => {
                self.field_not_found_error(field_name, current_type, field_span);
                Type::Error
            }
        }
    }

    fn infer_field(&mut self, fields: &[Expr]) -> Type {
        if fields.is_empty() {
            return Type::Error;
        }

        let base = fields[0].as_identifier();
        let first_span = fields[0].span.clone();

        if let Some((_, Some(sym_id))) = base {
            let var = match self.ctx.get_variable(sym_id.clone()) {
                Some(var) => var,
                None => return Type::Error,
            }
            .clone();

            if fields.len() > 1 { self.check_field(&var, fields, 1) } else { var.clone() }
        } else {
            self.non_struct_type(first_span);
            Type::Error
        }
    }

    fn non_struct_type(&mut self, span: Span) {
        self.error(
            "cannot access field of non-struct type",
            vec![("in this field access".to_string(), span)],
            vec![],
        )
    }

    fn field_not_found_error(&mut self, field_name: &str, ty: &Type, span: Span) {
        self.error(
            &format!("couldn't find field {field_name} on type {}", self.format_type(ty)),
            vec![("in this field access".to_string(), span)],
            vec![],
        )
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

    fn infer_call(
        &mut self,
        callee: &mut Expr,
        args: &mut Vec<Expr>,
        call_info: &mut Option<CallInfo>,
    ) -> Type {
        // Handles both direct function calls and generic function instantiations.
        if let Some((_, Some(sym_id))) = callee.as_identifier_mut() {
            let sym = self.symbol_table().get_symbol_unchecked(sym_id);
            if let SymbolKind::Function { signature, .. } = &sym.kind {
                // Map for tracking generic parameter substitutions during this call.
                let mut generic_mapping = HashMap::new();
                let mut param_types: Vec<Type> = Vec::with_capacity(signature.parameters.len());
                for param in &signature.parameters {
                    let param_type = if !generic_mapping.is_empty() {
                        self.substitute_type_with_map(&param.ty, &generic_mapping)
                    } else {
                        param.ty.clone()
                    };
                    param_types.push(param_type);
                }
                let mut valid = true;
                for (i, (arg, param)) in
                    args.iter_mut().zip(signature.parameters.iter()).enumerate()
                {
                    let arg_type = self.infer_expr(arg);
                    let mono_type = self.try_monomorphize_named_type(arg_type);
                    arg.ty = mono_type.clone();
                    if !signature.generics.is_empty() {
                        self.infer_generic_types(&param.ty, &mono_type, &mut generic_mapping);
                    }
                    let param_type = if !generic_mapping.is_empty() {
                        self.substitute_type_with_map(&param.ty, &generic_mapping)
                    } else {
                        param.ty.clone()
                    };
                    if !self.expect_type(
                        &param_type,
                        &mono_type,
                        &arg.span,
                        &format!("argument {}", i + 1),
                    ) {
                        valid = false;
                    }
                }
                if !valid {
                    return Type::Error;
                }

                let monomorphized_id = if !generic_mapping.is_empty() && valid {
                    let all_generics_mapped =
                        signature.generics.iter().all(|g| generic_mapping.contains_key(&g.name));

                    if all_generics_mapped {
                        Some(self.record_monomorphization_with_id(*sym_id, &generic_mapping, None))
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

                return if !generic_mapping.is_empty() {
                    self.substitute_type_with_map(&signature.return_type, &generic_mapping)
                } else {
                    signature.return_type.clone()
                };
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
        let symbol = self.symbol_table.get_symbol_unchecked(&decl.symbol_id.unwrap());
        self.ctx.add_variable(symbol.id, variable_ty.clone());
        if !self.expect_type(&variable_ty, value_ty, &decl.span, "variable declaration") {
            return Type::Error;
        }
        decl.ty = value_ty.clone();
        variable_ty.clone()
    }

    fn infer_identifier(&mut self, sym_id: SymbolId) -> Type {
        let sym = self.symbol_table().get_symbol_unchecked(&sym_id);

        match &sym.kind {
            SymbolKind::Parameter { .. } | SymbolKind::Variable { .. } => {
                self.ctx.get_variable(sym_id).unwrap().clone()
            }
            _ => unreachable!(),
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
            (Type::Named { name, generics: _ }, concrete_type) => {
                if self.ctx.is_generic_parameter(*name) {
                    mapping.insert(*name, concrete_type.clone());
                }
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

            (
                Type::Named { name: e_name, generics: e_generics },
                Type::Named { name: a_name, generics: a_generics },
            ) if e_name == a_name && e_generics.len() == a_generics.len() => {
                for (e_gen, a_gen) in e_generics.iter().zip(a_generics.iter()) {
                    self.infer_generic_types(e_gen, a_gen, mapping);
                }
            }
            _ => {}
        }
    }
}
