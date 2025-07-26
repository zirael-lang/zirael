use crate::{
    TokenKind,
    ast::{BinaryOp, Expr, ExprKind, Literal, UnaryOp},
    parser::Parser,
};

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Expr {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Expr {
        let expr = self.parse_logical_or();

        if let Some(token) = self.peek() {
            let op = match &token.kind {
                TokenKind::Equals => {
                    self.advance();
                    let right = self.parse_assignment();
                    return Expr(ExprKind::Assign(Box::new(expr), Box::new(right)));
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
                return Expr(ExprKind::AssignOp(Box::new(expr), op, Box::new(right)));
            }
        }

        expr
    }

    fn parse_logical_or(&mut self) -> Expr {
        self.parse_binary_expr(1)
    }

    fn get_binary_op_precedence(&self, token: &TokenKind) -> Option<(BinaryOp, u8)> {
        match token {
            // Precedence 1: Logical OR
            TokenKind::LogicalOr => Some((BinaryOp::LogicalOr, 1)),

            // Precedence 2: Logical AND
            TokenKind::LogicalAnd => Some((BinaryOp::LogicalAnd, 2)),

            // Precedence 3: Bitwise OR
            TokenKind::BitwiseOr => Some((BinaryOp::BitwiseOr, 3)),

            // Precedence 4: Bitwise XOR
            TokenKind::BitwiseXor => Some((BinaryOp::BitwiseXor, 4)),

            // Precedence 5: Bitwise AND
            TokenKind::BitwiseAnd => Some((BinaryOp::BitwiseAnd, 5)),

            // Precedence 6: Equality
            TokenKind::EqualsEquals => Some((BinaryOp::Equal, 6)),
            TokenKind::NotEquals => Some((BinaryOp::NotEqual, 6)),

            // Precedence 7: Comparison
            TokenKind::LessThan => Some((BinaryOp::LessThan, 7)),
            TokenKind::LessThanOrEqual => Some((BinaryOp::LessThanOrEqual, 7)),
            TokenKind::GreaterThan => Some((BinaryOp::GreaterThan, 7)),
            TokenKind::GreaterThanOrEqual => Some((BinaryOp::GreaterThanOrEqual, 7)),

            // Precedence 8: Shift
            TokenKind::LeftShift => Some((BinaryOp::LeftShift, 8)),
            TokenKind::RightShift => Some((BinaryOp::RightShift, 8)),

            // Precedence 9: Term
            TokenKind::Plus => Some((BinaryOp::Add, 9)),
            TokenKind::Minus => Some((BinaryOp::Subtract, 9)),

            // Precedence 10: Factor
            TokenKind::Multiply => Some((BinaryOp::Multiply, 10)),
            TokenKind::Divide => Some((BinaryOp::Divide, 10)),
            TokenKind::Modulo => Some((BinaryOp::Modulo, 10)),

            _ => None,
        }
    }

    fn parse_binary_expr(&mut self, min_precedence: u8) -> Expr {
        let mut left = if min_precedence == 0 {
            self.parse_unary()
        } else {
            self.parse_binary_expr(min_precedence - 1)
        };

        while let Some(token) = self.peek() {
            let (op, precedence) = match self.get_binary_op_precedence(&token.kind) {
                Some((op, prec)) if prec == min_precedence => (op, prec),
                _ => break,
            };

            self.advance();
            let right = self.parse_binary_expr(min_precedence);
            left = Expr(ExprKind::Binary { left: Box::new(left), op, right: Box::new(right) });
        }

        left
    }

    fn parse_unary(&mut self) -> Expr {
        if let Some(token) = self.peek() {
            let unary_op = match &token.kind {
                TokenKind::Minus => Some(UnaryOp::Minus),
                TokenKind::LogicalNot => Some(UnaryOp::Not),
                TokenKind::BitwiseNot => Some(UnaryOp::BitwiseNot),
                TokenKind::Multiply => Some(UnaryOp::Deref),
                TokenKind::BitwiseAnd => Some(UnaryOp::Ref),
                _ => None,
            };

            if let Some(op) = unary_op {
                self.advance();
                let expr = self.parse_unary();
                return Expr(ExprKind::Unary(Box::new(op), Box::new(expr)));
            }
        }

        self.parse_postfix()
    }

    fn parse_function_call(&mut self, callee: Expr) -> Expr {
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
            self.error_at_current("Expected ')' after function arguments");
        }

        Expr(ExprKind::Call { callee: Box::new(callee), args })
    }

    fn parse_field_access(&mut self, base: Expr) -> Expr {
        let mut field_chain = vec![base];

        while self.match_token(TokenKind::Dot) {
            let field_expr = self.parse_primary();
            field_chain.push(field_expr);
        }

        if field_chain.len() > 1 {
            Expr(ExprKind::FieldAccess(field_chain))
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
            match &token.kind {
                TokenKind::Integer(value) => {
                    let value = *value;
                    self.advance();
                    Expr(ExprKind::Literal(Literal::Integer(value)))
                }
                TokenKind::Float(value) => {
                    let value = *value;
                    self.advance();
                    Expr(ExprKind::Literal(Literal::Float(value)))
                }
                TokenKind::String(value) => {
                    let value = value.clone();
                    self.advance();
                    Expr(ExprKind::Literal(Literal::String(value)))
                }
                TokenKind::Bool(value) => {
                    let value = *value;
                    self.advance();
                    Expr(ExprKind::Literal(Literal::Bool(value)))
                }

                TokenKind::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    let identifier = zirael_utils::prelude::get_or_intern(&name);
                    Expr(ExprKind::Identifier(identifier))
                }

                TokenKind::ParenOpen => {
                    self.advance();
                    let expr = self.parse_expr();

                    if !self.match_token(TokenKind::ParenClose) {
                        self.error_at_current("Expected ')' after expression");
                    }

                    Expr(ExprKind::Paren(Box::new(expr)))
                }

                TokenKind::BraceOpen => {
                    self.advance();
                    self.parse_block()
                }

                _ => {
                    self.error_at_peek(format!("Unexpected token in expression: {:?}", token.kind));
                    self.advance();
                    Expr(ExprKind::couldnt_parse())
                }
            }
        } else {
            self.error_at_current("Unexpected end of input in expression");
            Expr(ExprKind::couldnt_parse())
        }
    }

    /// The caller should have consumed the opening brace
    pub fn parse_block(&mut self) -> Expr {
        let mut stmts = vec![];

        while !self.check(&TokenKind::BraceClose) && !self.is_at_end() {
            stmts.push(self.parse_stmt());
        }

        if !self.match_token(TokenKind::BraceClose) {
            self.error_at_current("Expected '}' to close block");
        }

        Expr(ExprKind::Block(stmts))
    }
}
