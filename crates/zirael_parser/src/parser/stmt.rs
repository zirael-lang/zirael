use crate::{
    Return, TokenKind,
    ast::{Keyword, Stmt, StmtKind, Type, VarDecl},
    parser::Parser,
    span::SpanUtils,
};
use zirael_utils::prelude::*;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Stmt {
        if self.match_keyword(Keyword::Var) {
            let span_start = self.prev_span();
            let identifier = self.expect_identifier().unwrap_or(default_ident());

            let ty = if self.match_token(TokenKind::Colon) {
                self.parse_type().expect("handle this better")
            } else {
                Type::Inferred
            };

            self.expect_message(TokenKind::Equals, "every variable must be initialized");
            let value = self.parse_expr();
            self.match_token(TokenKind::Semicolon);
            let span_end = self.prev_span();

            Stmt(StmtKind::Var(VarDecl {
                name: identifier,
                value,
                ty,
                span: span_start.to(span_end),
            }))
        } else if self.match_keyword(Keyword::Return) {
            let span = self.prev_span();

            let value = if self.match_token(TokenKind::Semicolon) {
                None
            } else {
                Some(self.parse_logical_or())
            };

            self.match_token(TokenKind::Semicolon);

            Stmt(StmtKind::Return(Return { value, span: span.to(self.prev_span()) }))
        } else {
            let expr = self.parse_expr();
            self.match_token(TokenKind::Semicolon);
            Stmt(StmtKind::Expr(expr))
        }
    }
}
