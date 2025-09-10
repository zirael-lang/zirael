use crate::{
  GenericCall, Keyword, TokenKind, Type,
  ast::{
    BinaryOp, Expr, ExprKind, Literal, MatchArm, Path, PathSegment, Pattern, PatternField, UnaryOp,
  },
  parser::Parser,
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
          return self
            .new_expr(ExprKind::Assign(Box::new(expr), Box::new(right)), start_span.to(end_span));
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
      let start_span = condition.span;
      let true_expr = self.parse_ternary();

      if !self.match_token(TokenKind::Colon) {
        self.error_at_current("expected ':' after true expression in ternary operator");
        return self.new_expr(ExprKind::couldnt_parse(), start_span);
      }

      let false_expr = self.parse_ternary();
      let end_span = false_expr.span;

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

      let start_span = left.span;
      self.advance();
      let right = self.parse_binary_expr(precedence + 1);
      let end_span = right.span;
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
        let end_span = expr.span;
        return self
          .new_expr(ExprKind::Unary(Box::new(op), Box::new(expr)), start_span.to(end_span));
      }
    }

    self.parse_postfix()
  }

  fn parse_type_annotations(&mut self) -> Vec<Type> {
    let mut type_annotations = vec![];

    if self.check(&TokenKind::LessThan) {
      self.advance();

      if !self.check(&TokenKind::GreaterThan) {
        loop {
          if let Some(ty) = self.parse_type() {
            type_annotations.push(ty);
          } else {
            self.error_at_current("expected type in type annotation");
            break;
          }

          if self.match_token(TokenKind::Comma) {
            continue;
          }

          break;
        }
      }

      if !self.match_token(TokenKind::GreaterThan) {
        self.error_at_current("expected '>' after function type annotations");
      }
    }

    type_annotations
  }

  fn parse_function_call(&mut self, callee: Expr) -> Expr {
    let start_span = callee.span;
    let type_annotations = self.parse_type_annotations();

    if !self.match_token(TokenKind::ParenOpen) {
      self.error_at_current("expected '(' after function name");
      let end_span = self.prev_span();
      return self.new_expr(
        ExprKind::Call {
          callee: Box::new(callee),
          call: GenericCall { args: vec![], call_info: None, type_annotations },
        },
        start_span.to(end_span),
      );
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
      self.error_at_current("expected ')' after function arguments");
    }

    let end_span = self.prev_span();
    self.new_expr(
      ExprKind::Call {
        callee: Box::new(callee),
        call: GenericCall { args, call_info: None, type_annotations },
      },
      start_span.to(end_span),
    )
  }

  fn parse_method_call(&mut self, base: Expr) -> Expr {
    let start_span = base.span;

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

    let type_annotations = self.parse_type_annotations();

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
      ExprKind::MethodCall { chain, call: GenericCall { args, call_info: None, type_annotations } },
      start_span.to(end_span),
    )
  }

  fn parse_field_access(&mut self, base: Expr) -> Expr {
    let start_span = base.span;
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
      let end_span = field_chain.last().unwrap().span;
      self.new_expr(ExprKind::FieldAccess(field_chain), start_span.to(end_span))
    } else {
      field_chain.into_iter().next().unwrap()
    }
  }

  fn parse_index_access(&mut self, base: Expr) -> Expr {
    let start_span = base.span;
    self.advance();

    let index = self.parse_expr();

    if !self.match_token(TokenKind::BracketClose) {
      self.error_at_current("expected ']' to close index access");
    }

    let end_span = self.prev_span();
    self.new_expr(ExprKind::IndexAccess(Box::new(base), Box::new(index)), start_span.to(end_span))
  }

  fn parse_postfix(&mut self) -> Expr {
    let mut expr = self.parse_primary();

    loop {
      if let Some(token) = self.peek() {
        match &token.kind {
          TokenKind::ParenOpen => {
            expr = self.parse_function_call(expr);
          }
          TokenKind::LessThan => {
            // Look ahead to see if this is a function call with type annotations
            if self.is_function_call_with_generics() {
              expr = self.parse_function_call(expr);
            } else {
              break;
            }
          }
          TokenKind::DoubleColon => {
            expr = self.parse_path_continuation(expr);
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
          let identifier = get_or_intern(&name, Some(identifier_span));

          if self.check(&TokenKind::DoubleColon) {
            return self.parse_path_from_identifier(identifier, identifier_span);
          }

          if self.check(&TokenKind::BraceOpen) {
            return self.parse_struct_initializer_from_path(Path::from_identifier(
              identifier,
              identifier_span,
            ));
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

        TokenKind::Keyword(Keyword::Match) => self.parse_match(),

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

  fn parse_match(&mut self) -> Expr {
    let start_span = self.peek_span();
    self.advance();

    let scrutinee = Box::new(self.parse_match_scrutinee());

    if !self.match_token(TokenKind::BraceOpen) {
      self.error_at_current("expected '{' after match expression");
    }

    let mut arms = Vec::new();

    while !self.check(&TokenKind::BraceClose) && !self.is_at_end() {
      let arm = self.parse_match_arm();
      arms.push(arm);

      if self.check(&TokenKind::Comma) {
        self.advance();
      }
    }

    if !self.match_token(TokenKind::BraceClose) {
      self.error_at_current("expected '}' after match arms");
    }

    let end_span = self.prev_span();
    self.new_expr(ExprKind::Match { scrutinee, arms }, start_span.to(end_span))
  }

  fn parse_match_scrutinee(&mut self) -> Expr {
    if let Some(token) = self.peek() {
      match &token.kind {
        TokenKind::Identifier(_) => {
          let checkpoint = self.position;
          let expr = self.parse_primary_for_match_scrutinee();
          
          if self.check(&TokenKind::DoubleColon) {
            self.position = checkpoint;
            return self.parse_path_expression_for_match_scrutinee();
          }
          
          expr
        }
        _ => {
          self.parse_primary()
        }
      }
    } else {
      self.error_at_current("unexpected end of input");
      let span = self.prev_span();
      self.new_expr(ExprKind::couldnt_parse(), span)
    }
  }

  fn parse_primary_for_match_scrutinee(&mut self) -> Expr {
    if let Some(token) = self.peek() {
      match &token.kind {
        TokenKind::Identifier(name) => {
          let name = name.clone();
          self.advance();
          let identifier_span = self.prev_span();
          let identifier = get_or_intern(&name, Some(identifier_span));

          self.new_expr(ExprKind::Identifier(identifier, None), identifier_span)
        }
        _ => {
          self.parse_primary()
        }
      }
    } else {
      self.error_at_current("unexpected end of input");
      let span = self.prev_span();
      self.new_expr(ExprKind::couldnt_parse(), span)
    }
  }

  fn parse_path_expression_for_match_scrutinee(&mut self) -> Expr {
    if let Some(token) = self.peek() {
      if let TokenKind::Identifier(name) = &token.kind {
        let name = name.clone();
        self.advance();
        let identifier_span = self.prev_span();
        let identifier = get_or_intern(&name, Some(identifier_span));

        if self.check(&TokenKind::DoubleColon) {
          return self.parse_path_from_identifier(identifier, identifier_span);
        } else {
          return self.new_expr(ExprKind::Identifier(identifier, None), identifier_span);
        }
      }
    }

    self.error_at_current("unexpected token in match scrutinee");
    let span = self.prev_span();
    self.new_expr(ExprKind::couldnt_parse(), span)
  }

  fn parse_match_arm(&mut self) -> MatchArm {
    let start_span = self.peek_span();
    let pattern = self.parse_pattern();
    let end_span = self.prev_span();

    if !self.match_token(TokenKind::FatArrow) {
      self.error_at_current("expected '=>' after match pattern");
    }

    let body = self.parse_expr();

    MatchArm { pattern, body, span: start_span.to(end_span) }
  }

  fn parse_pattern(&mut self) -> Pattern {
    if let Some(token) = self.peek() {
      match &token.kind {
        TokenKind::Underscore => {
          self.advance();
          Pattern::Wildcard
        }
        TokenKind::Identifier(name) => {
          let name = name.clone();
          self.advance();
          let identifier = get_or_intern(&name, Some(self.prev_span()));
          let identifier_span = self.prev_span();

          if self.check(&TokenKind::DoubleColon) {
            let path = self.parse_path_from_identifier_in_pattern(identifier, identifier_span);

            let fields = if self.check(&TokenKind::BraceOpen) {
              Some(self.parse_pattern_fields())
            } else {
              None
            };

            Pattern::EnumVariant { path, fields, resolved_variant: None }
          } else if self.check(&TokenKind::BraceOpen) {
            self.parse_struct_pattern(identifier)
          } else {
            Pattern::Identifier(identifier)
          }
        }
        TokenKind::Integer(value) => {
          let value = *value;
          self.advance();
          Pattern::Literal(Literal::Integer(value))
        }
        TokenKind::Float(value) => {
          let value = *value;
          self.advance();
          Pattern::Literal(Literal::Float(value))
        }
        TokenKind::String(value) => {
          let value = value.clone();
          self.advance();
          Pattern::Literal(Literal::String(value))
        }
        TokenKind::Char(value) => {
          let value = *value;
          self.advance();
          Pattern::Literal(Literal::Char(value))
        }
        TokenKind::Bool(value) => {
          let value = *value;
          self.advance();
          Pattern::Literal(Literal::Bool(value))
        }
        _ => {
          self.error_at_peek(format!("unexpected token in pattern: {:?}", token.kind));
          self.advance();
          Pattern::Wildcard
        }
      }
    } else {
      self.error_at_current("unexpected end of input in pattern");
      Pattern::Wildcard
    }
  }

  fn parse_path_from_identifier_in_pattern(
    &mut self,
    first_identifier: Identifier,
    first_span: Span,
  ) -> Path {
    let start_span = first_span;
    let mut segments = vec![PathSegment::new(first_identifier, first_span)];

    while self.match_token(TokenKind::DoubleColon) {
      if let Some(ident) = self.expect_identifier() {
        let segment_span = self.prev_span();
        let segment = PathSegment::new(ident, segment_span);
        segments.push(segment);
      } else {
        self.error_at_current("expected identifier after '::'");
        break;
      }
    }

    let end_span = segments.last().map(|s| s.span).unwrap_or(start_span);
    Path::new(segments, start_span.to(end_span))
  }

  fn parse_path_continuation(&mut self, expr: Expr) -> Expr {
    let start_span = expr.span;

    let mut segments = match &expr.kind {
      ExprKind::Identifier(ident, _) => {
        vec![PathSegment::new(*ident, expr.span)]
      }
      ExprKind::Path(path) => path.segments.clone(),
      _ => {
        return self.parse_static_call_from_expr(expr);
      }
    };

    while self.match_token(TokenKind::DoubleColon) {
      if let Some(ident) = self.expect_identifier() {
        let segment_span = self.prev_span();
        let mut segment = PathSegment::new(ident, segment_span);

        if self.check(&TokenKind::LessThan) && !self.is_function_call_with_generics() {
          segment.type_args = self.parse_type_annotations();
        }

        segments.push(segment);
      } else {
        self.error_at_current("expected identifier after '::'");
        break;
      }
    }

    let end_span = segments.last().map(|s| s.span).unwrap_or(start_span);
    let path = Path::new(segments, start_span.clone().to(end_span));

    if self.check(&TokenKind::BraceOpen) {
      return self.parse_struct_initializer_from_path(path);
    }

    self.new_expr(ExprKind::Path(path), start_span.to(end_span))
  }

  fn parse_static_call_from_expr(&mut self, expr: Expr) -> Expr {
    let start_span = expr.span;

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
      let combined_span = start_span.clone().to(method_name.span);
      self.new_expr(ExprKind::FieldAccess(vec![expr, method_name]), combined_span)
    };

    let type_annotations = self.parse_type_annotations();

    let args = if self.match_token(TokenKind::ParenOpen) {
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

      args
    } else {
      vec![]
    };

    let end_span = self.prev_span();
    self.new_expr(
      ExprKind::StaticCall {
        callee: Box::new(callee),
        call: GenericCall { args, call_info: None, type_annotations },
      },
      start_span.to(end_span),
    )
  }

  fn parse_struct_pattern(&mut self, name: Identifier) -> Pattern {
    let fields = self.parse_pattern_fields();
    Pattern::Struct { name, fields }
  }

  fn parse_pattern_fields(&mut self) -> Vec<PatternField> {
    if !self.match_token(TokenKind::BraceOpen) {
      self.error_at_current("expected '{' for pattern fields");
      return vec![];
    }

    let mut fields = Vec::new();

    while !self.check(&TokenKind::BraceClose) && !self.is_at_end() {
      let start_span = self.peek_span();

      if let Some(token) = self.peek() {
        if let TokenKind::Identifier(field_name) = &token.kind {
          let field_name = field_name.clone();
          self.advance();
          let identifier = get_or_intern(&field_name, Some(self.prev_span()));
          let end_span = self.prev_span();

          let pattern = if self.match_token(TokenKind::Colon) {
            Some(Box::new(self.parse_pattern()))
          } else {
            None
          };

          fields.push(PatternField {
            name: identifier,
            pattern,
            span: start_span.to(end_span),
            sym_id: None,
            ty: None,
          });

          if !self.match_token(TokenKind::Comma) {
            break;
          }
        } else {
          self.error_at_peek("expected field name in pattern");
          break;
        }
      } else {
        self.error_at_current("unexpected end of input in pattern fields");
        break;
      }
    }

    if !self.match_token(TokenKind::BraceClose) {
      self.error_at_current("expected '}' after pattern fields");
    }

    fields
  }

  fn is_function_call_with_generics(&mut self) -> bool {
    let mut depth = 0;
    let mut position = 0;

    while let Some(token) = self.peek_ahead(position) {
      match &token.kind {
        TokenKind::LessThan => {
          depth += 1;
        }
        TokenKind::GreaterThan => {
          depth -= 1;
          if depth == 0 {
            if let Some(next_token) = self.peek_ahead(position + 1) {
              return next_token.kind == TokenKind::ParenOpen;
            }
            return false;
          }
        }
        TokenKind::RightShift => {
          depth -= 2;
          if depth <= 0 {
            if depth == 0 {
              if let Some(next_token) = self.peek_ahead(position + 1) {
                return next_token.kind == TokenKind::ParenOpen;
              }
            }
            return false;
          }
        }
        TokenKind::Semicolon
        | TokenKind::BraceOpen
        | TokenKind::BraceClose
        | TokenKind::ParenClose
        | TokenKind::BracketClose => {
          return false;
        }
        _ => {}
      }

      position += 1;

      if position > 50 {
        return false;
      }
    }

    false
  }

  fn parse_path_from_identifier(&mut self, first_identifier: Identifier, first_span: Span) -> Expr {
    let start_span = first_span;
    let mut segments = vec![PathSegment::new(first_identifier, first_span)];

    while self.match_token(TokenKind::DoubleColon) {
      if let Some(ident) = self.expect_identifier() {
        let segment_span = self.prev_span();
        let mut segment = PathSegment::new(ident, segment_span);

        if self.check(&TokenKind::LessThan) && !self.is_function_call_with_generics() {
          segment.type_args = self.parse_type_annotations();
        }

        segments.push(segment);
      } else {
        self.error_at_current("expected identifier after '::'");
        break;
      }
    }

    let end_span = segments.last().map(|s| s.span).unwrap_or(start_span);
    let path = Path::new(segments, start_span.clone().to(end_span));

    if self.check(&TokenKind::BraceOpen) {
      return self.parse_struct_initializer_from_path(path);
    }

    self.new_expr(ExprKind::Path(path), start_span.to(end_span))
  }

  fn parse_struct_initializer_from_path(&mut self, path: Path) -> Expr {
    let start_span = path.span;
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
        name: Box::new(Expr::new(ExprKind::Path(path), start_span, expr_id)),
        call_info: None,
        fields,
      },
      start_span.to(end_span),
    )
  }
}
