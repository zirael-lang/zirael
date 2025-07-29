use crate::{
    SymbolTableError, TokenKind,
    ast::{Keyword, Stmt, StmtKind, Type, VarDecl},
    parser::Parser,
};
use zirael_utils::prelude::*;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Stmt {
        if self.match_keyword(Keyword::Var) {
            let span_start = self.peek_span();
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
                span: span_start.start..span_end.end,
            }))
        } else {
            Stmt(StmtKind::Expr(self.parse_expr()))
        }
    }
}
