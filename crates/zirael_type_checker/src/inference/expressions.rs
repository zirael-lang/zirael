use crate::inference::{TypeInference, TypeInferenceContext};
use zirael_parser::{
    AstWalker, BinaryOp, Expr, ExprId, ExprKind, Literal, ScopeType, Stmt, StmtKind, SymbolId,
    SymbolKind, Type, UnaryOp, VarDecl, WalkerContext,
};
use zirael_utils::prelude::{Colorize, SourceFileId, warn};

impl<'reports> TypeInference<'reports> {
    pub fn infer_expr(&mut self, expr: &mut Expr) -> Type {
        match &mut expr.kind {
            ExprKind::Literal(lit) => self.infer_literal(lit),
            ExprKind::Identifier(_, symbol_id) => self.infer_identifier(symbol_id.unwrap()),
            ExprKind::Assign(lhs, rhs) => self.infer_assignment(lhs, rhs),
            ExprKind::Block(stmts) => self.infer_block(stmts),
            ExprKind::Unary(op, expr) => self.infer_unary(op, expr),
            ExprKind::Binary { left, op, right } => self.infer_binary(left, op, right),
            _ => {
                warn!("unimplemented expr: {:#?}", expr);
                Type::Error
            }
        }
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

        if !self.eq(&lhs_ty, &rhs_ty) {
            self.type_mismatch(&lhs_ty, &rhs_ty, lhs.span.clone());
            return Type::Error;
        }

        // assigment is more of a statement
        Type::Void
    }

    fn infer_block(&mut self, stmts: &mut [Stmt]) -> Type {
        self.push_scope(ScopeType::Block);
        if stmts.is_empty() {
            return Type::Void;
        }

        let mut block_type = Type::Void;

        for stmt in stmts.iter_mut() {
            match &mut stmt.0 {
                StmtKind::Expr(expr) => {
                    block_type = self.infer_expr(expr);
                }
                StmtKind::Return(ret) => {
                    if let Some(expr) = ret.value.as_mut() {
                        block_type = self.infer_expr(expr);
                    } else {
                        block_type = Type::Void;
                    }
                    break;
                }
                _ => {
                    self.infer_stmt(stmt);
                }
            }
        }
        self.pop_scope();

        block_type
    }

    pub fn infer_variable(&mut self, decl: &mut VarDecl) -> Type {
        let value_ty = &self.infer_expr(&mut decl.value);
        let variable_ty = if let Type::Inferred = decl.ty { &value_ty } else { &decl.ty };

        let symbol = self.symbol_table.lookup_symbol(&decl.name).unwrap();
        self.ctx.add_variable(symbol.id, variable_ty.clone());

        if !self.eq(value_ty, variable_ty) {
            self.type_mismatch(variable_ty, value_ty, decl.span.clone());
            return Type::Error;
        }

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

    fn infer_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.0 {
            StmtKind::Var(decl) => {
                self.infer_variable(decl);
            }
            StmtKind::Expr(expr) => {
                self.infer_expr(expr);
            }
            StmtKind::Return(ret) => {
                if let Some(expr) = ret.value.as_mut() {
                    self.infer_expr(expr);
                }
            }
            _ => {
                warn!("unimplemented stmt: {:#?}", stmt);
            }
        }
    }
}
