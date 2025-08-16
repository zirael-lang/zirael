use crate::prelude::{Color, ReportKind, WalkerContext, debug};
use zirael_parser::{
    AstWalker, Expr, ExprKind, Return, SymbolId, SymbolKind, SymbolTable, UnaryOp, VarDecl,
    VariableMove, impl_ast_walker,
};
use zirael_utils::{
    ident_table::Identifier,
    prelude::{
        Colorize as _, ReportBuilder, Reports, SourceFileId, Sources, Span, article, resolve,
    },
};

impl_ast_walker!(MemoryAnalysis);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum ExprContext {
    VarDecl,
    Expr,
}

impl<'reports> MemoryAnalysis<'reports> {
    fn visit_unary_impl(
        &mut self,
        op: &mut UnaryOp,
        expr: &mut Expr,
        ctx: ExprContext,
        mark_heap: &mut bool,
    ) {
        match (op, ctx) {
            (UnaryOp::Ref, _) => {
                self.handle_borrow_operation(expr);
            }
            (UnaryOp::Box, ExprContext::VarDecl) => {
                self.walk_expr(expr);
                *mark_heap = true;
            }
            (UnaryOp::Box, ExprContext::Expr) => {
                self.handle_invalid_box_operation(expr);
            }
            _ => {}
        }
    }

    fn handle_borrow_operation(&mut self, expr: &mut Expr) {
        self.walk_expr(expr);

        if !expr.kind.can_be_borrowed() {
            self.error(
                &format!("cannot borrow {}", article(expr.kind.name())),
                vec![("attempted to borrow here".to_owned(), expr.span.clone())],
                vec![
                    "you can only borrow: variables, fields, array elements, and function parameters".to_owned()
                ],
            );
            return;
        }

        let sym_id = match expr.kind {
            ExprKind::Identifier(_, symbol_id) => symbol_id.unwrap(),
            ExprKind::FieldAccess(_) | ExprKind::IndexAccess(_, _) => {
                todo!("not implemented")
            }
            _ => unreachable!(),
        };
    }

    fn handle_invalid_box_operation(&mut self, expr: &Expr) {
        self.error(
            "can only use box on variable declarations",
            vec![("attempted to box here".to_owned(), expr.span.clone())],
            vec![
                "box expressions create heap-allocated values and can only be used when declaring variables".to_owned(),
                "try assigning this to a variable first".to_owned(),
            ],
        );
    }

    fn handle_move_semantics(&mut self, expr: &Expr) -> bool {
        if let Some((mut symbol, sym_id)) = self.symbol_table.symbol_from_expr(expr) {
            if let SymbolKind::Variable { ref mut is_moved, .. } = symbol.kind {
                if let Some(entry) = self.symbol_table.is_borrowed(sym_id) {
                    self.report_move_of_borrowed_value(expr, &entry.drop_span);
                    return false;
                }

                if let Some(existing_move) = is_moved {
                    self.report_use_of_moved_value(expr.span.clone(), existing_move);
                    return false;
                }

                debug!("moving variable {}", resolve(&symbol.name));
                self.mark_variable_as_moved(&mut symbol, expr);
            }

            self.symbol_table
                .update_symbol_kind(sym_id, |_| symbol.kind.clone())
                .expect("failed to update symbol kind");

            true
        } else {
            false
        }
    }

    fn report_move_of_borrowed_value(&mut self, expr: &Expr, borrow_span: &Span) {
        let report = ReportBuilder::builder("cannot move a borrowed value", ReportKind::Error)
            .label("attempted to move here", expr.span.clone())
            .label_color_custom("borrowed here", borrow_span.clone(), Color::BrightCyan)
            .note("values cannot be moved while they are borrowed")
            .note("the borrow must end before the value can be moved");

        self.report(report);
    }

    fn mark_variable_as_moved(&self, symbol: &mut zirael_parser::Symbol, expr: &Expr) {
        if let SymbolKind::Variable { ref mut is_moved, .. } = symbol.kind {
            *is_moved = Some(VariableMove {
                from: symbol.source_location.clone().unwrap(),
                to: expr.span.clone(),
            });
        }
    }

    fn extract_symbol_from_return_value(&self, value: &Expr) -> Option<SymbolId> {
        match &value.kind {
            ExprKind::Identifier(_, sym_id) => *sym_id,
            ExprKind::Unary(op, expr) => {
                if **op == UnaryOp::Ref {
                    self.symbol_table.symbol_from_expr(expr).map(|(_, sym_id)| sym_id)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn report_return_of_local_reference(&mut self, value: &Expr, symbol: &zirael_parser::Symbol) {
        let report = ReportBuilder::builder(
            format!(
                "{} is dropped when function ends",
                resolve(&symbol.name).dimmed().bold()
            ),
            ReportKind::Error,
        )
            .label(
                "returns a reference to data that will be dropped at the end of this function",
                value.span.clone()
            )
            .label_color_custom(
                "declared here",
                symbol.source_location.clone().unwrap(),
                Color::BrightCyan
            )
            .note("Local variables, temporary values, and function parameters are dropped when the function ends")
            .note("consider returning the value directly instead of a reference");

        self.report(report);
    }

    fn report_use_of_moved_value(&mut self, span: Span, moved: &VariableMove) {
        let report = ReportBuilder::builder("cannot use moved values", ReportKind::Error)
            .label("attempted to use moved value here", span)
            .label_color_custom("declared here", moved.from.clone(), Color::BrightCyan)
            .label_color_custom("moved here", moved.to.clone(), Color::BrightCyan)
            .note("values cannot be used after they have been moved")
            .note("consider cloning the value before moving, or use a reference instead");

        self.report(report);
    }

    fn report_assignment_to_borrowed_value(&mut self, lhs: &Expr, borrow_span: &Span) {
        let report = ReportBuilder::builder("cannot assign to a borrowed value", ReportKind::Error)
            .label("attempted to assign here", lhs.span.clone())
            .label_color_custom("borrowed here", borrow_span.clone(), Color::BrightCyan)
            .note("borrowed values cannot be modified")
            .note("the borrow must end before the value can be assigned to");

        self.report(report);
    }
}

impl<'reports> AstWalker<'reports> for MemoryAnalysis<'reports> {
    fn walk_var_decl(&mut self, var: &mut VarDecl) {
        self.walk_type(&mut var.ty);
        let is_heap = &mut false;

        if let ExprKind::Unary(op, expr) = &mut var.value.kind {
            self.visit_unary_impl(op, expr, ExprContext::VarDecl, is_heap);

            if *is_heap {
                let sym_id = var.symbol_id.unwrap();
                self.symbol_table.mark_heap_variable(sym_id).unwrap();
                self.symbol_table.mark_drop(sym_id, expr.span.clone());
            }
        } else if !self.handle_move_semantics(&mut var.value) {
            self.walk_expr(&mut var.value);
        }
    }

    fn visit_unary(&mut self, op: &mut UnaryOp, expr: &mut Expr) {
        self.visit_unary_impl(op, expr, ExprContext::Expr, &mut false);
    }

    fn visit_identifier(&mut self, id: &mut Identifier, sym_id: &mut Option<SymbolId>, span: Span) {
        let sym_id = sym_id.unwrap();
        let symbol = self.symbol_table.get_symbol_unchecked(&sym_id);

        if let SymbolKind::Variable { is_moved, .. } = symbol.kind
            && let Some(moved) = is_moved
        {
            self.report_use_of_moved_value(span, &moved);
        }
    }

    fn visit_return(&mut self, ret: &mut Return) {
        if let Some(value) = ret.value.as_mut() {
            let sym_id = self.extract_symbol_from_return_value(value);

            if let Some(sym_id) = sym_id
                && let Some(symbol) = self.symbol_table.get_symbol(sym_id)
                && symbol.kind.is_value()
                && self.symbol_table.is_borrowed(sym_id).is_some()
            {
                self.report_return_of_local_reference(value, &symbol);
            }
        }
    }

    fn visit_assign(&mut self, lhs: &mut Expr, rhs: &mut Expr) {
        if let Some((_, sym_id)) = self.symbol_table.symbol_from_expr(lhs) {
            if let Some(entry) = self.symbol_table.is_borrowed(sym_id) {
                self.report_assignment_to_borrowed_value(lhs, &entry.drop_span);
            }
        }

        if !self.handle_move_semantics(rhs) {
            self.walk_expr(rhs);
        }

        self.walk_expr(lhs);
    }
}
