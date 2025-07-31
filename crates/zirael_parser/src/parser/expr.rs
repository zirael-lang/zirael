use crate::{
    Keyword, ScopeType, TokenKind,
    ast::{BinaryOp, Expr, ExprKind, Literal, UnaryOp},
    parser::Parser,
    span::SpanUtils,
};

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Expr {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Expr {
        let start_span = self.peek_span();
        let expr = self.parse_logical_or();

        if let Some(token) = self.peek() {
            let op = match &token.kind {
                TokenKind::Equals => {
                    self.advance();
                    let right = self.parse_assignment();
                    let end_span = self.prev_span();
                    return Expr::new(
                        ExprKind::Assign(Box::new(expr), Box::new(right)),
                        start_span.to(end_span),
                    );
                }
                TokenKind::PlusEquals => Some(BinaryOp::Add),
                TokenKind::MinusEquals => Some(BinaryOp::Subtract),
                TokenKind::MultiplyEquals => Some(BinaryOp::Multiply),
                TokenKind::DivideEquals => Some(BinaryOp::Divide),
                TokenKind::ModuloEquals => Some(BinaryOp::Modulo),
                _ => None,
            };

            if let Some(op) = op {
                self.advance();
                let right = self.parse_assignment();
                let end_span = self.prev_span();
                return Expr::new(
                    ExprKind::AssignOp(Box::new(expr), op, Box::new(right)),
                    start_span.to(end_span),
                );
            }
        }

        expr
    }

    fn parse_logical_or(&mut self) -> Expr {
        self.parse_binary_expr(1)
    }

    fn get_binary_op_precedence(&self, token: &TokenKind) -> Option<(BinaryOp, u8)> {
        match token {
            TokenKind::LogicalOr => Some((BinaryOp::LogicalOr, 1)),
            TokenKind::LogicalAnd => Some((BinaryOp::LogicalAnd, 2)),
            TokenKind::BitwiseOr => Some((BinaryOp::BitwiseOr, 3)),
            TokenKind::BitwiseXor => Some((BinaryOp::BitwiseXor, 4)),
            TokenKind::BitwiseAnd => Some((BinaryOp::BitwiseAnd, 5)),
            TokenKind::EqualsEquals => Some((BinaryOp::Equal, 6)),
            TokenKind::NotEquals => Some((BinaryOp::NotEqual, 6)),
            TokenKind::LessThan => Some((BinaryOp::LessThan, 7)),
            TokenKind::LessThanOrEqual => Some((BinaryOp::LessThanOrEqual, 7)),
            TokenKind::GreaterThan => Some((BinaryOp::GreaterThan, 7)),
            TokenKind::GreaterThanOrEqual => Some((BinaryOp::GreaterThanOrEqual, 7)),
            TokenKind::LeftShift => Some((BinaryOp::LeftShift, 8)),
            TokenKind::RightShift => Some((BinaryOp::RightShift, 8)),
            TokenKind::Plus => Some((BinaryOp::Add, 9)),
            TokenKind::Minus => Some((BinaryOp::Subtract, 9)),
            TokenKind::Multiply => Some((BinaryOp::Multiply, 10)),
            TokenKind::Divide => Some((BinaryOp::Divide, 10)),
            TokenKind::Modulo => Some((BinaryOp::Modulo, 10)),
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
            left = Expr::new(
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
                return Expr::new(
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
        Expr::new(ExprKind::Call { callee: Box::new(callee), args }, start_span.to(end_span))
    }

    fn parse_field_access(&mut self, base: Expr) -> Expr {
        let start_span = base.span.clone();
        let mut field_chain = vec![base];

        while self.match_token(TokenKind::Dot) {
            let field_expr = self.parse_primary();
            field_chain.push(field_expr);
        }

        if field_chain.len() > 1 {
            let end_span = field_chain.last().unwrap().span.clone();
            Expr::new(ExprKind::FieldAccess(field_chain), start_span.to(end_span))
        } else {
            field_chain.into_iter().next().unwrap()
        }
    }

    fn parse_postfix(&mut self) -> Expr {
        let mut expr = self.parse_primary();

        loop {
            if let Some(token) = self.peek() {
                match &token.kind {
                    TokenKind::ParenOpen => {
                        expr = self.parse_function_call(expr);
                    }
                    TokenKind::Dot => {
                        expr = self.parse_field_access(expr);
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
                    Expr::new(ExprKind::Literal(Literal::Integer(value)), span)
                }
                TokenKind::Float(value) => {
                    let value = *value;
                    self.advance();
                    let span = self.prev_span();
                    Expr::new(ExprKind::Literal(Literal::Float(value)), span)
                }
                TokenKind::String(value) => {
                    let value = value.clone();
                    self.advance();
                    let span = self.prev_span();
                    Expr::new(ExprKind::Literal(Literal::String(value)), span)
                }
                TokenKind::Bool(value) => {
                    let value = *value;
                    self.advance();
                    let span = self.prev_span();
                    Expr::new(ExprKind::Literal(Literal::Bool(value)), span)
                }

                TokenKind::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    let span = self.prev_span();
                    let identifier = zirael_utils::prelude::get_or_intern(&name);
                    Expr::new(ExprKind::Identifier(identifier, None), span)
                }

                TokenKind::ParenOpen => {
                    let start_span = span;
                    self.advance();
                    let expr = self.parse_expr();

                    if !self.match_token(TokenKind::ParenClose) {
                        self.error_at_current("expected ')' after expression");
                    }

                    let end_span = self.prev_span();
                    Expr::new(ExprKind::Paren(Box::new(expr)), start_span.to(end_span))
                }

                TokenKind::BraceOpen => {
                    self.advance();
                    self.parse_block()
                }

                _ => {
                    self.error_at_peek(format!("unexpected token in expression: {:?}", token.kind));
                    self.advance();
                    let span = self.prev_span();
                    Expr::new(ExprKind::couldnt_parse(), span)
                }
            }
        } else {
            self.error_at_current("unexpected end of input in expression");
            let span = self.peek_span();
            Expr::new(ExprKind::couldnt_parse(), span)
        }
    }

    pub fn parse_block(&mut self) -> Expr {
        let start_span = self.prev_span();
        let mut stmts = vec![];

        while !self.check(&TokenKind::BraceClose) && !self.is_at_end() {
            let stmt = self.parse_stmt();
            stmts.push(stmt);
        }

        if !self.match_token(TokenKind::BraceClose) {
            self.error_at_current("expected '}' to close block");
        }

        let end_span = self.prev_span();
        Expr::new(ExprKind::Block(stmts), start_span.to(end_span))
    }
}
