use crate::hir::{
    HirBody, HirFunction, HirFunctionSignature, HirItem, HirItemKind, HirLocal, HirModule,
    HirParam,
    expr::{HirExpr, HirExprKind, HirStmt},
};
use id_arena::Arena;
use std::{collections::HashMap, ops::Range};
use zirael_parser::{ast::item::Item, *};
use zirael_utils::prelude::*;

pub struct AstLowering<'reports> {
    symbol_table: SymbolTable,
    reports: Reports<'reports>,
    sources: Sources,
    processed_file: Option<SourceFileId>,
}

impl<'reports> AstLowering<'reports> {
    pub fn new(symbol_table: &SymbolTable, reports: &Reports<'reports>, sources: &Sources) -> Self {
        Self {
            symbol_table: symbol_table.clone(),
            reports: reports.clone(),
            sources: sources.clone(),
            processed_file: None,
        }
    }

    pub fn lower_modules(&mut self, lexed_modules: &mut Vec<LexedModule>) -> Vec<HirModule> {
        lexed_modules.iter_mut().map(|module| self.lower_module(module)).collect()
    }

    fn push_scope(&mut self, scope_type: ScopeType) {
        self.symbol_table.push_scope(scope_type);
    }

    fn pop_scope(&mut self) {
        if let Err(err) = self.symbol_table.pop_scope() {
            self.error(&format!("Failed to pop scope: {:?}", err), vec![], vec![]);
        }
    }

    fn lower_module(&mut self, lexed_module: &mut LexedModule) -> HirModule {
        let mut hir_module = HirModule { items: HashMap::new(), id: lexed_module.file().unwrap() };
        let id = lexed_module.id.as_file().unwrap();
        self.processed_file = Some(id);

        self.push_scope(ScopeType::Module(id));
        for item in &mut lexed_module.ast.items {
            if let Some(hir_item) = self.lower_item(item) {
                hir_module.items.insert(hir_item.symbol_id, hir_item);
            }
        }
        self.pop_scope();

        hir_module
    }

    fn lower_item(&mut self, item: &mut Item) -> Option<HirItem> {
        if let ItemKind::Import(..) = &item.kind {
            return None;
        }

        let symbol_id = item.symbol_id.unwrap();

        match &mut item.kind {
            ItemKind::Function(func) => {
                self.push_scope(ScopeType::Function(func.id));

                let hir_function = self.lower_function(func, symbol_id);
                self.pop_scope();
                Some(HirItem {
                    symbol_id,
                    kind: HirItemKind::Function(hir_function),
                    span: item.span.clone(),
                })
            }
            _ => None,
        }
    }

    fn lower_function(&mut self, func: &mut Function, symbol_id: SymbolId) -> HirFunction {
        let parameters = func
            .signature
            .parameters
            .iter_mut()
            .filter_map(|param| self.lower_parameter(param))
            .collect();

        let hir_signature =
            HirFunctionSignature { parameters, return_type: func.signature.return_type.clone() };

        let body =
            func.body.as_mut().map(|body_expr| self.lower_function_body(body_expr, symbol_id));

        HirFunction {
            id: func.id,
            symbol_id,
            signature: hir_signature,
            body,
            is_async: func.modifiers.is_async,
            is_const: func.modifiers.is_const,
            is_extern: func.modifiers.is_extern,
            abi: func.modifiers.abi.as_ref().map(|a| a.0.clone()),
        }
    }

    fn lower_parameter(&mut self, param: &mut Parameter) -> Option<HirParam> {
        let symbol_id = param.symbol_id.unwrap();
        let default_value = param.default_value.as_mut().map(|expr| self.lower_expr(expr));

        Some(HirParam {
            symbol_id,
            ty: param.ty.clone(),
            is_variadic: matches!(param.kind, ParameterKind::Variadic),
            default_value,
        })
    }

    fn lower_function_body(&mut self, body_expr: &mut Expr, _function_symbol: SymbolId) -> HirBody {
        let mut locals = HashMap::new();
        let root_expr = self.lower_expr_with_locals(body_expr, &mut locals);

        HirBody { locals, root_expr }
    }

    fn lower_expr_with_locals(
        &mut self,
        ast_expr: &mut Expr,
        locals: &mut HashMap<SymbolId, HirLocal>,
    ) -> HirExpr {
        let hir_kind = match &mut ast_expr.kind {
            ExprKind::Literal(lit) => HirExprKind::Literal(lit.clone()),

            ExprKind::Identifier(_name, Some(symbol_id)) => HirExprKind::Symbol(*symbol_id),

            ExprKind::Identifier(name, None) => {
                if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
                    HirExprKind::Symbol(symbol.id)
                } else {
                    self.error("Unresolved identifier", vec![], vec![]);
                    HirExprKind::Error
                }
            }

            ExprKind::Binary { left, op, right } => {
                let left_expr = Box::new(self.lower_expr_with_locals(left, locals));
                let right_expr = Box::new(self.lower_expr_with_locals(right, locals));
                HirExprKind::Binary { left: left_expr, op: op.clone(), right: right_expr }
            }

            ExprKind::Unary(op, operand) => {
                let operand_expr = Box::new(self.lower_expr_with_locals(operand, locals));
                HirExprKind::Unary { op: *op.clone(), operand: operand_expr }
            }

            ExprKind::Assign(lhs, rhs) => {
                let lhs_expr = Box::new(self.lower_expr_with_locals(lhs, locals));
                let rhs_expr = Box::new(self.lower_expr_with_locals(rhs, locals));
                HirExprKind::Assign { lhs: lhs_expr, rhs: rhs_expr }
            }

            ExprKind::AssignOp(lhs, op, rhs) => {
                let lhs_expr = Box::new(self.lower_expr_with_locals(lhs, locals));
                let rhs_expr = Box::new(self.lower_expr_with_locals(rhs, locals));
                HirExprKind::Assign {
                    lhs: lhs_expr.clone(),
                    rhs: Box::new(HirExpr {
                        kind: HirExprKind::Binary {
                            left: lhs_expr,
                            op: op.clone(),
                            right: rhs_expr,
                        },
                        ty: ast_expr.ty.clone(),
                        span: ast_expr.span.clone(),
                        id: ast_expr.id,
                    }),
                }
            }

            ExprKind::Call { callee, args } => {
                let callee_expr = Box::new(self.lower_expr_with_locals(callee, locals));
                let arg_exprs: Vec<HirExpr> =
                    args.iter_mut().map(|arg| self.lower_expr_with_locals(arg, locals)).collect();
                HirExprKind::Call { callee: callee_expr, args: arg_exprs }
            }

            ExprKind::FieldAccess(exprs) => {
                todo!("Implement field access lowering")
            }

            ExprKind::IndexAccess(object, index) => {
                let object_expr = Box::new(self.lower_expr_with_locals(object, locals));
                let index_expr = Box::new(self.lower_expr_with_locals(index, locals));
                HirExprKind::IndexAccess { object: object_expr, index: index_expr }
            }

            ExprKind::Block(stmts) => {
                self.push_scope(ScopeType::Block(ast_expr.id));
                let mut hir_stmts = vec![];
                let mut has_return = false;

                let stmts_len = stmts.len();
                for (i, stmt) in stmts.iter_mut().enumerate() {
                    match &mut stmt.0 {
                        StmtKind::Expr(expr) => {
                            let is_last_stmt = i == stmts_len - 1;
                            let lowered_expr = self.lower_expr(expr);

                            if is_last_stmt
                                && !has_return
                                && lowered_expr.kind.can_be_wrapped_in_return()
                            {
                                hir_stmts.push(HirStmt::Return(Some(lowered_expr)));
                            } else {
                                hir_stmts.push(HirStmt::Expr(lowered_expr));
                            }
                        }
                        StmtKind::Return(ret) => {
                            hir_stmts.push(HirStmt::Return(
                                ret.value.clone().map(|mut e| self.lower_expr(&mut e)),
                            ));
                            has_return = true;
                            break;
                        }
                        StmtKind::Var(var_stmt) => {
                            let symbol_id = var_stmt.symbol_id.unwrap();

                            hir_stmts.push(HirStmt::Var {
                                symbol_id,
                                init: self.lower_expr(&mut var_stmt.value),
                            });
                        }
                    }
                }

                self.pop_scope();
                HirExprKind::Block(hir_stmts)
            }
            ExprKind::Paren(inner) => {
                return self.lower_expr_with_locals(inner, locals);
            }

            ExprKind::CouldntParse(_) => {
                self.error("Could not parse expression", vec![], vec![]);
                HirExprKind::Error
            }
        };

        HirExpr {
            kind: hir_kind,
            ty: ast_expr.ty.clone(),
            span: ast_expr.span.clone(),
            id: ast_expr.id,
        }
    }

    fn lower_expr(&mut self, ast_expr: &mut Expr) -> HirExpr {
        let mut locals = HashMap::new();
        self.lower_expr_with_locals(ast_expr, &mut locals)
    }

    fn error(&mut self, message: &str, labels: Vec<(String, Range<usize>)>, notes: Vec<String>) {
        if let Some(file_id) = self.processed_file {
            let mut report = ReportBuilder::builder(message, ReportKind::Error);
            for note in notes {
                report = report.note(&note);
            }
            for (msg, span) in labels {
                report = report.label(&msg, span);
            }
            self.reports.add(file_id, report);
        } else {
            warn!("Report outside of a file: {}", message);
        }
    }
}

pub fn lower_ast_to_hir<'reports>(
    lexed_modules: &mut Vec<LexedModule>,
    symbol_table: &SymbolTable,
    reports: &Reports<'reports>,
    sources: &Sources,
) -> Vec<HirModule> {
    let mut lowering = AstLowering::new(symbol_table, reports, sources);
    lowering.lower_modules(lexed_modules)
}
