use crate::{
    Keyword, TokenKind,
    ast::{BinaryOp, Expr, ExprKind, Literal, UnaryOp},
    parser::Parser,
    span::SpanUtils as _,
};
use std::collections::HashMap;
use zirael_utils::{
    ident_table::{Identifier, get_or_intern},
    prelude::Span,
};

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Expr {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Expr {
        let start_span = self.peek_span();
        let expr = self.parse_ternary();

        if let Some(token) = self.peek() {
            let op = match &token.kind {
                TokenKind::Equals => {
                    self.advance();
                    let right = self.parse_assignment();
                    let end_span = self.prev_span();
                    return self.new_expr(
                        ExprKind::Assign(Box::new(expr), Box::new(right)),
                        start_span.to(end_span),
                    );
                }
                TokenKind::PlusEquals => Some(BinaryOp::Add),
                TokenKind::MinusEquals => Some(BinaryOp::Sub),
                TokenKind::MultiplyEquals => Some(BinaryOp::Mul),
                TokenKind::DivideEquals => Some(BinaryOp::Div),
                TokenKind::ModuloEquals => Some(BinaryOp::Rem),
                _ => None,
            };

            if let Some(op) = op {
                self.advance();
                let right = self.parse_assignment();
                let end_span = self.prev_span();
                return self.new_expr(
                    ExprKind::AssignOp(Box::new(expr), op, Box::new(right)),
                    start_span.to(end_span),
                );
            }
        }

        expr
    }

    fn parse_ternary(&mut self) -> Expr {
        let condition = self.parse_logical_or();

        if self.match_token(TokenKind::Question) {
            let start_span = condition.span.clone();
            let true_expr = self.parse_ternary();

            if !self.match_token(TokenKind::Colon) {
                self.error_at_current("expected ':' after true expression in ternary operator");
                return self.new_expr(ExprKind::couldnt_parse(), start_span);
            }

            let false_expr = self.parse_ternary();
            let end_span = false_expr.span.clone();

            return self.new_expr(
                ExprKind::Ternary {
                    condition: Box::new(condition),
                    true_expr: Box::new(true_expr),
                    false_expr: Box::new(false_expr),
                },
                start_span.to(end_span),
            );
        }

        condition
    }

    pub fn parse_logical_or(&mut self) -> Expr {
        self.parse_binary_expr(1)
    }

    fn get_binary_op_precedence(&self, token: &TokenKind) -> Option<(BinaryOp, u8)> {
        match token {
            TokenKind::LogicalOr => Some((BinaryOp::Or, 1)),
            TokenKind::LogicalAnd => Some((BinaryOp::And, 2)),
            TokenKind::BitwiseOr => Some((BinaryOp::BitOr, 3)),
            TokenKind::BitwiseXor => Some((BinaryOp::BitXor, 4)),
            TokenKind::BitwiseAnd => Some((BinaryOp::BitAnd, 5)),
            TokenKind::EqualsEquals => Some((BinaryOp::Eq, 6)),
            TokenKind::NotEquals => Some((BinaryOp::Ne, 6)),
            TokenKind::LessThan => Some((BinaryOp::Lt, 7)),
            TokenKind::LessThanOrEqual => Some((BinaryOp::Le, 7)),
            TokenKind::GreaterThan => Some((BinaryOp::Gt, 7)),
            TokenKind::GreaterThanOrEqual => Some((BinaryOp::Ge, 7)),
            TokenKind::LeftShift => Some((BinaryOp::Shl, 8)),
            TokenKind::RightShift => Some((BinaryOp::Shr, 8)),
            TokenKind::Plus => Some((BinaryOp::Add, 9)),
            TokenKind::Minus => Some((BinaryOp::Sub, 9)),
            TokenKind::Multiply => Some((BinaryOp::Mul, 10)),
            TokenKind::Divide => Some((BinaryOp::Div, 10)),
            TokenKind::Modulo => Some((BinaryOp::Rem, 10)),
            _ => None,
        }
    }

    fn parse_binary_expr(&mut self, min_precedence: u8) -> Expr {
        let mut left = if min_precedence <= 10 {
            self.parse_unary()
        } else {
            return self.parse_unary();
        };

        while let Some(token) = self.peek() {
            let (op, precedence) = match self.get_binary_op_precedence(&token.kind) {
                Some((op, prec)) if prec >= min_precedence => (op, prec),
                _ => break,
            };

            let start_span = left.span.clone();
            self.advance();
            let right = self.parse_binary_expr(precedence + 1);
            let end_span = right.span.clone();
            left = self.new_expr(
                ExprKind::Binary { left: Box::new(left), op, right: Box::new(right) },
                start_span.to(end_span),
            );
        }

        left
    }

    fn parse_unary(&mut self) -> Expr {
        if let Some(token) = self.peek() {
            let start_span = self.peek_span();
            let unary_op = match &token.kind {
                TokenKind::Minus => Some(UnaryOp::Minus),
                TokenKind::LogicalNot => Some(UnaryOp::Not),
                TokenKind::BitwiseNot => Some(UnaryOp::BitwiseNot),
                TokenKind::Multiply => Some(UnaryOp::Deref),
                TokenKind::BitwiseAnd => Some(UnaryOp::Ref),
                TokenKind::Keyword(Keyword::Box) => Some(UnaryOp::Box),
                _ => None,
            };

            if let Some(op) = unary_op {
                self.advance();
                let expr = self.parse_unary();
                let end_span = expr.span.clone();
                return self.new_expr(
                    ExprKind::Unary(Box::new(op), Box::new(expr)),
                    start_span.to(end_span),
                );
            }
        }

        self.parse_postfix()
    }

    fn parse_function_call(&mut self, callee: Expr) -> Expr {
        let start_span = callee.span.clone();
        self.advance();
        let mut args = Vec::new();

        if !self.check(&TokenKind::ParenClose) {
            loop {
                args.push(self.parse_expr());

                if self.match_token(TokenKind::Comma) {
                    continue;
                }

                break;
            }
        }

        if !self.match_token(TokenKind::ParenClose) {
            self.error_at_current("expected ')' after function arguments");
        }

        let end_span = self.prev_span();
        self.new_expr(
            ExprKind::Call { callee: Box::new(callee), args, call_info: None },
            start_span.to(end_span),
        )
    }

    fn parse_method_call(&mut self, base: Expr) -> Expr {
        let start_span = base.span.clone();

        let mut chain = vec![base];

        self.advance();
        let method_name = if let Some(ident) = self.expect_identifier() {
            let method_span = self.prev_span();
            self.new_expr(ExprKind::Identifier(ident, None), method_span)
        } else {
            self.error_at_current("expected method name after '.'");
            let error_span = self.prev_span();
            self.new_expr(ExprKind::couldnt_parse(), error_span)
        };

        chain.push(method_name);

        self.advance();
        let mut args = Vec::new();

        if !self.check(&TokenKind::ParenClose) {
            loop {
                args.push(self.parse_expr());

                if self.match_token(TokenKind::Comma) {
                    continue;
                }

                break;
            }
        }

        if !self.match_token(TokenKind::ParenClose) {
            self.error_at_current("expected ')' after method arguments");
        }

        let end_span = self.prev_span();
        self.new_expr(
            ExprKind::MethodCall { chain, args, call_info: None },
            start_span.to(end_span),
        )
    }

    fn parse_field_access(&mut self, base: Expr) -> Expr {
        let start_span = base.span.clone();
        let mut field_chain = vec![base];

        while self.check(&TokenKind::Dot) {
            if let Some(next_token) = self.peek_ahead(1) {
                if let TokenKind::Identifier(_) = &next_token.kind {
                    if let Some(third_token) = self.peek_ahead(2) {
                        if third_token.kind == TokenKind::ParenOpen {
                            break;
                        }
                    }
                }
            }

            self.advance();
            let field_expr = self.parse_primary();
            field_chain.push(field_expr);
        }

        if field_chain.len() > 1 {
            let end_span = field_chain.last().unwrap().span.clone();
            self.new_expr(ExprKind::FieldAccess(field_chain), start_span.to(end_span))
        } else {
            field_chain.into_iter().next().unwrap()
        }
    }

    fn parse_index_access(&mut self, base: Expr) -> Expr {
        let start_span = base.span.clone();
        self.advance();

        let index = self.parse_expr();

        if !self.match_token(TokenKind::BracketClose) {
            self.error_at_current("expected ']' to close index access");
        }

        let end_span = self.prev_span();
        self.new_expr(
            ExprKind::IndexAccess(Box::new(base), Box::new(index)),
            start_span.to(end_span),
        )
    }

    fn parse_postfix(&mut self) -> Expr {
        let mut expr = self.parse_primary();

        loop {
            if let Some(token) = self.peek() {
                match &token.kind {
                    TokenKind::ParenOpen => {
                        expr = self.parse_function_call(expr);
                    }
                    TokenKind::DoubleColon => {
                        expr = self.parse_static_call(expr);
                    }
                    TokenKind::Dot => {
                        if let Some(next_token) = self.peek_ahead(1) {
                            if let TokenKind::Identifier(_) = &next_token.kind {
                                if let Some(third_token) = self.peek_ahead(2) {
                                    if third_token.kind == TokenKind::ParenOpen {
                                        expr = self.parse_method_call(expr);
                                        continue;
                                    }
                                }
                            }
                        }
                        expr = self.parse_field_access(expr);
                    }
                    TokenKind::BracketOpen => {
                        expr = self.parse_index_access(expr);
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }

        expr
    }

    fn parse_primary(&mut self) -> Expr {
        if let Some(token) = self.peek() {
            let span = self.peek_span();

            match &token.kind {
                TokenKind::Integer(value) => {
                    let value = *value;
                    self.advance();
                    let span = self.prev_span();
                    self.new_expr(ExprKind::Literal(Literal::Integer(value)), span)
                }
                TokenKind::Float(value) => {
                    let value = *value;
                    self.advance();
                    let span = self.prev_span();
                    self.new_expr(ExprKind::Literal(Literal::Float(value)), span)
                }
                TokenKind::Char(value) => {
                    let value = *value;
                    self.advance();
                    let span = self.prev_span();
                    self.new_expr(ExprKind::Literal(Literal::Char(value)), span)
                }
                TokenKind::String(value) => {
                    let value = value.clone();
                    self.advance();
                    let span = self.prev_span();
                    self.new_expr(ExprKind::Literal(Literal::String(value)), span)
                }
                TokenKind::Bool(value) => {
                    let value = *value;
                    self.advance();
                    let span = self.prev_span();
                    self.new_expr(ExprKind::Literal(Literal::Bool(value)), span)
                }

                TokenKind::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    let identifier_span = self.prev_span();
                    let identifier = get_or_intern(&name);

                    if self.check(&TokenKind::BraceOpen) {
                        return self.parse_struct_initializer(identifier, identifier_span);
                    }

                    self.new_expr(ExprKind::Identifier(identifier, None), identifier_span)
                }

                TokenKind::ParenOpen => {
                    let start_span = span;
                    self.advance();
                    let expr = self.parse_expr();

                    if !self.match_token(TokenKind::ParenClose) {
                        self.error_at_current("expected ')' after expression");
                    }

                    let end_span = self.prev_span();
                    self.new_expr(ExprKind::Paren(Box::new(expr)), start_span.to(end_span))
                }

                TokenKind::BraceOpen => {
                    self.advance();
                    self.parse_block()
                }

                _ => {
                    self.error_at_peek(format!("unexpected token in expression: {:?}", token.kind));
                    self.advance();
                    let span = self.prev_span();
                    self.new_expr(ExprKind::couldnt_parse(), span)
                }
            }
        } else {
            self.error_at_current("unexpected end of input in expression");
            let span = self.peek_span();
            self.new_expr(ExprKind::couldnt_parse(), span)
        }
    }

    pub fn parse_block(&mut self) -> Expr {
        let start_span = self.prev_span();
        let mut stmts = vec![];

        while !self.check(&TokenKind::BraceClose) && !self.is_at_end() {
            let start_position = self.position;
            let stmt = self.parse_stmt();
            stmts.push(stmt);

            if self.position == start_position {
                self.error_at_peek("unable to parse statement, skipping token");
                self.advance();
            }
        }

        if !self.match_token(TokenKind::BraceClose) {
            self.error_at_current("expected '}' to close block");
        }

        let end_span = self.prev_span();
        self.new_expr(ExprKind::Block(stmts), start_span.to(end_span))
    }

    fn parse_struct_initializer(&mut self, struct_name: Identifier, start_span: Span) -> Expr {
        self.expect(TokenKind::BraceOpen);

        let mut fields = HashMap::new();

        if !self.check(&TokenKind::BraceClose) {
            loop {
                let start_position = self.position;

                let field_name = if let Some(ident) = self.expect_identifier() {
                    ident
                } else {
                    self.error_at_current("expected field name in struct initializer");
                    self.synchronize(&[TokenKind::Comma, TokenKind::BraceClose]);

                    if self.position == start_position && !self.is_at_end() {
                        self.advance();
                    }

                    if self.check(&TokenKind::BraceClose) {
                        break;
                    }
                    continue;
                };

                if !self.match_token(TokenKind::Colon) && !self.match_token(TokenKind::Equals) {
                    self.error_at_current("expected ':' or '=' after field name");
                    self.synchronize(&[TokenKind::Comma, TokenKind::BraceClose]);
                    if self.check(&TokenKind::BraceClose) {
                        break;
                    }
                    continue;
                }

                let value = self.parse_expr();
                fields.insert(field_name, value);

                if !self.match_token(TokenKind::Comma) {
                    break;
                }

                if self.check(&TokenKind::BraceClose) {
                    break;
                }
            }
        }

        if !self.match_token(TokenKind::BraceClose) {
            self.error_at_current("expected '}' to close struct initializer");
        }

        let end_span = self.prev_span();
        let expr_id = self.fresh_id();
        self.new_expr(
            ExprKind::StructInit {
                name: Box::new(Expr::new(
                    ExprKind::Identifier(struct_name, None),
                    start_span.clone(),
                    expr_id,
                )),
                call_info: None,
                fields,
            },
            start_span.to(end_span),
        )
    }

    fn parse_static_call(&mut self, type_expr: Expr) -> Expr {
        let start_span = type_expr.span.clone();

        self.advance();

        let method_name = if let Some(ident) = self.expect_identifier() {
            let method_span = self.prev_span();
            self.new_expr(ExprKind::Identifier(ident, None), method_span)
        } else {
            self.error_at_current("expected method name after '::'");
            let error_span = self.prev_span();
            self.new_expr(ExprKind::couldnt_parse(), error_span)
        };

        let callee = {
            let combined_span = start_span.clone().to(method_name.span.clone());
            self.new_expr(ExprKind::FieldAccess(vec![type_expr, method_name]), combined_span)
        };

        if !self.match_token(TokenKind::ParenOpen) {
            self.error_at_current("expected '(' after static method name");
            return callee;
        }

        let mut args = Vec::new();

        if !self.check(&TokenKind::ParenClose) {
            loop {
                args.push(self.parse_expr());

                if self.match_token(TokenKind::Comma) {
                    continue;
                }

                break;
            }
        }

        if !self.match_token(TokenKind::ParenClose) {
            self.error_at_current("expected ')' after static method arguments");
        }

        let end_span = self.prev_span();
        self.new_expr(
            ExprKind::StaticCall { callee: Box::new(callee), args, call_info: None },
            start_span.to(end_span),
        )
    }
}
