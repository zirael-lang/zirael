use crate::{
    ExprKind, Return, TokenKind,
    ast::{Keyword, Stmt, StmtKind, Type, VarDecl},
    parser::Parser,
    span::SpanUtils as _,
};
use zirael_utils::prelude::*;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Stmt {
        let _start_position = self.position;

        if self.match_keyword(Keyword::Var) {
            let span_start = self.prev_span();
            let identifier = self.expect_identifier().unwrap_or(default_ident());

            let ty = if self.match_token(TokenKind::Colon) {
                self.parse_type().unwrap_or_else(|| {
                    self.error_at_current("expected type after ':'");
                    Type::Error
                })
            } else {
                Type::Inferred
            };

            if !self.match_token(TokenKind::Equals) {
                self.error_at_current("every variable must be initialized with '='");
            }

            let value = self.parse_expr();
            self.match_token(TokenKind::Semicolon);
            let span_end = self.prev_span();

            Stmt(StmtKind::Var(VarDecl {
                name: identifier,
                value,
                ty,
                span: span_start.to(span_end),
                symbol_id: None,
            }))
        } else if self.match_keyword(Keyword::Return) {
            let span = self.prev_span();

            let value = if self.match_token(TokenKind::Semicolon) {
                None
            } else {
                let expr = self.parse_logical_or();
                self.match_token(TokenKind::Semicolon);
                Some(expr)
            };

            Stmt(StmtKind::Return(Return { value, span: span.to(self.prev_span()) }))
        } else {
            let expr = self.parse_expr();

            if !self.match_token(TokenKind::Semicolon)
                && !matches!(expr.kind, ExprKind::Assign(_, _) | ExprKind::AssignOp(..))
            {
                return Stmt(StmtKind::Return(Return {
                    value: Some(expr.clone()),
                    span: expr.span,
                }));
            }

            self.match_token(TokenKind::Semicolon);
            Stmt(StmtKind::Expr(expr))
        }
    }
}
