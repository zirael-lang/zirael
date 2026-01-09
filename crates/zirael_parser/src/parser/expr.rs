use crate::ast::expressions::*;
use crate::ast::identifier::Ident;
use crate::ast::types::Mutability;
use crate::lexer::IntBase as LexIntBase;
use crate::parser::Parser;
use crate::parser::errors::{
  DuplicateNamedArg, EmptyMatch, ExpectedBuiltinName, ExpectedExpressionFound,
  ExpectedFatArrow, ExpectedFieldName, ExpectedPattern, InvalidAssignTarget,
  MissingColonInTernary, MissingInKeyword,
};
use crate::{NodeId, Path, TokenType, TypePath};
use std::collections::HashMap;
use zirael_source::prelude::Span;

impl Parser<'_> {
  pub fn parse_expr(&mut self) -> Expr {
    self.parse_expr_with_precedence(0)
  }

  pub fn parse_const_expr(&mut self) -> Expr {
    let mut expr = self.parse_expr();
    expr.is_const = true;
    expr
  }

  fn parse_expr_with_precedence(&mut self, min_precedence: u8) -> Expr {
    let start = self.current_span();

    // Parse range prefix (..x or ..=x)
    if self.check(&TokenType::DotDot) || self.check(&TokenType::DotDotEq) {
      return self.parse_range_expr(None, start);
    }

    let mut left = self.parse_unary_expr();

    loop {
      if self.check(&TokenType::DotDot) || self.check(&TokenType::DotDotEq) {
        left = self.parse_range_expr(Some(left), start);
        continue;
      }

      if let Some(op) = self.peek_assign_op() {
        if op.precedence() >= min_precedence {
          self.advance();
          let value = self.parse_expr_with_precedence(op.precedence());

          // Validate assignment target
          if !Self::is_valid_assign_target(&left) {
            self.emit(InvalidAssignTarget { span: left.span });
          }

          left = Expr::new(
            ExprKind::Assign {
              op,
              target: Box::new(left),
              value: Box::new(value),
            },
            self.span_from(start),
          );
          continue;
        }
      }

      if self.check(&TokenType::Question) && min_precedence <= 13 {
        left = self.parse_ternary_expr(left, start);
        continue;
      }

      if let Some(op) = self.peek_binary_op() {
        let precedence = op.precedence();
        if precedence >= min_precedence {
          self.advance();
          let next_min = if op.is_left_associative() {
            precedence + 1
          } else {
            precedence
          };
          let right = self.parse_expr_with_precedence(next_min);
          left = Expr::new(
            ExprKind::Binary {
              op,
              left: Box::new(left),
              right: Box::new(right),
            },
            self.span_from(start),
          );
          continue;
        }
      }

      break;
    }

    left
  }

  fn parse_ternary_expr(&mut self, condition: Expr, start: Span) -> Expr {
    self.eat(TokenType::Question);
    let then_expr = self.parse_expr();
    if !self.eat(TokenType::Colon) {
      self.emit(MissingColonInTernary {
        span: self.peek().span,
      });
    }
    let else_expr = self.parse_expr_with_precedence(13);
    Expr::new(
      ExprKind::Ternary {
        condition: Box::new(condition),
        then_expr: Box::new(then_expr),
        else_expr: Box::new(else_expr),
      },
      self.span_from(start),
    )
  }

  fn parse_unary_expr(&mut self) -> Expr {
    let start = self.current_span();

    if let Some(op) = self.peek_unary_op() {
      self.advance();

      let op = if op == UnaryOp::Deref && self.peek().kind == TokenType::Amp {
        UnaryOp::Deref
      } else {
        op
      };

      let operand = self.parse_unary_expr();
      return Expr::new(
        ExprKind::Unary {
          op,
          operand: Box::new(operand),
        },
        self.span_from(start),
      );
    }

    if self.eat(TokenType::Amp) {
      let mutability = if self.eat(TokenType::Mut) {
        Mutability::Mut
      } else if self.eat(TokenType::Const) {
        Mutability::Const
      } else {
        todo!("proper diagnostic")
      };
      let operand = self.parse_unary_expr();
      return Expr::new(
        ExprKind::AddrOf {
          mutability,
          operand: Box::new(operand),
        },
        self.span_from(start),
      );
    }

    self.parse_postfix_expr()
  }

  fn parse_postfix_expr(&mut self) -> Expr {
    let start = self.current_span();
    let mut expr = self.parse_primary_expr();

    loop {
      if self.eat(TokenType::LeftParen) {
        // Function call
        let args = self.parse_call_args();
        self.expect(TokenType::RightParen, "to close function call");
        expr = Expr::new(
          ExprKind::Call {
            callee: Box::new(expr),
            args,
          },
          self.span_from(start),
        );
      } else if self.eat(TokenType::Dot) {
        // Field access or tuple index
        if self.is_identifier() {
          let field = self.parse_identifier();
          expr = Expr::new(
            ExprKind::Field {
              object: Box::new(expr),
              field,
            },
            self.span_from(start),
          );
        } else if let TokenType::IntegerLiteral(base) = &self.peek().kind {
          // tuple field access
          let value = if let LexIntBase::Decimal(s) = base {
            s.clone()
          } else {
            self.emit(ExpectedFieldName {
              found: self.peek().kind.clone(),
              span: self.peek().span,
            });
            "0".to_owned()
          };
          let span = self.advance().span;
          expr = Expr::new(
            ExprKind::Field {
              object: Box::new(expr),
              field: Ident::new(&value, span),
            },
            self.span_from(start),
          );
        } else {
          self.advance();
          self.emit(ExpectedFieldName {
            found: self.peek().kind.clone(),
            span: self.peek().span,
          });
        }
      } else if self.eat(TokenType::LeftBracket) {
        let index = self.parse_expr();
        self.expect(TokenType::RightBracket, "to close index expression");
        expr = Expr::new(
          ExprKind::Index {
            object: Box::new(expr),
            index: Box::new(index),
          },
          self.span_from(start),
        );
      } else if self.eat(TokenType::As) {
        let target_type = self.parse_type();
        expr = Expr::new(
          ExprKind::Cast {
            expr: Box::new(expr),
            target_type: Box::new(target_type),
          },
          self.span_from(start),
        );
      } else {
        break;
      }
    }

    expr
  }

  fn parse_primary_expr(&mut self) -> Expr {
    let start = self.current_span();
    let token = self.peek().clone();

    match &token.kind {
      // Literals
      TokenType::IntegerLiteral(_) => self.parse_int_literal(),
      TokenType::FloatLiteral(_) => self.parse_float_literal(),
      TokenType::StringLiteral(_) => self.parse_string_literal(),
      TokenType::CharLiteral(_) => self.parse_char_literal(),
      TokenType::ByteLiteral(_) => self.parse_byte_literal(),
      TokenType::True | TokenType::False => self.parse_bool_literal(),

      TokenType::SelfValue => {
        self.advance();
        Expr::new(ExprKind::SelfValue, self.span_from(start))
      }

      TokenType::LeftParen => self.parse_paren_or_tuple(),
      TokenType::LeftBracket => self.parse_array_expr(),
      TokenType::LeftBrace => {
        let block = self.parse_block();
        Expr::new(ExprKind::Block(block), self.span_from(start))
      }

      TokenType::If => self.parse_if_expr(),
      TokenType::Match => self.parse_match_expr(),
      TokenType::Loop => self.parse_loop_expr(),
      TokenType::While => self.parse_while_expr(),
      TokenType::For => self.parse_for_expr(),
      TokenType::Break => self.parse_break_expr(),
      TokenType::Continue => self.parse_continue_expr(),
      TokenType::Return => self.parse_return_expr(),

      TokenType::At => self.parse_builtin_expr(),

      TokenType::Identifier(_) | TokenType::Package | TokenType::Super => {
        self.parse_path_or_struct_expr()
      }

      _ => {
        self.emit(ExpectedExpressionFound {
          found: token.kind.clone(),
          span: token.span,
        });
        Expr::dummy()
      }
    }
  }

  fn parse_int_literal(&mut self) -> Expr {
    let start = self.current_span();
    let token = self.advance();

    if let TokenType::IntegerLiteral(base) = token.kind {
      let (value, int_base) = match base {
        LexIntBase::Decimal(s) => (s, IntBase::Decimal),
        LexIntBase::Binary(s) => (s, IntBase::Binary),
        LexIntBase::Octal(s) => (s, IntBase::Octal),
        LexIntBase::Hexadecimal(s) => (s, IntBase::Hexadecimal),
      };

      let suffix = IntSuffix::parse_int_suffix(&token.lexeme, &value);
      Expr::new_const(
        ExprKind::Literal(Literal::Int(IntLit {
          id: NodeId::new(),
          value: value.to_string(),
          base: int_base,
          suffix,
          span: token.span,
        })),
        self.span_from(start),
      )
    } else {
      Expr::dummy()
    }
  }

  fn parse_float_literal(&mut self) -> Expr {
    let start = self.current_span();
    let token = self.advance();

    if let TokenType::FloatLiteral(value) = token.kind {
      let suffix = if token.lexeme.ends_with("f32") {
        Some(FloatSuffix::F32)
      } else if token.lexeme.ends_with("f64") {
        Some(FloatSuffix::F64)
      } else {
        None
      };

      Expr::new_const(
        ExprKind::Literal(Literal::Float(FloatLit {
          id: NodeId::new(),
          value,
          suffix,
          span: token.span,
        })),
        self.span_from(start),
      )
    } else {
      Expr::dummy()
    }
  }

  fn parse_string_literal(&mut self) -> Expr {
    let start = self.current_span();
    let token = self.advance();

    if let TokenType::StringLiteral(value) = token.kind {
      Expr::new_const(
        ExprKind::Literal(Literal::String(StringLit {
          id: NodeId::new(),
          value: value.clone(),
          raw: token.lexeme.clone(),
          span: token.span,
        })),
        self.span_from(start),
      )
    } else {
      Expr::dummy()
    }
  }

  fn parse_char_literal(&mut self) -> Expr {
    let start = self.current_span();
    let token = self.advance();

    if let TokenType::CharLiteral(value) = token.kind {
      Expr::new_const(
        ExprKind::Literal(Literal::Char(CharLit {
          id: NodeId::new(),
          value,
          raw: token.lexeme.clone(),
          span: token.span,
        })),
        self.span_from(start),
      )
    } else {
      Expr::dummy()
    }
  }

  fn parse_byte_literal(&mut self) -> Expr {
    let start = self.current_span();
    let token = self.advance();

    if let TokenType::ByteLiteral(value) = token.kind {
      Expr::new_const(
        ExprKind::Literal(Literal::Byte(ByteLit {
          id: NodeId::new(),
          value,
          raw: token.lexeme.clone(),
          span: token.span,
        })),
        self.span_from(start),
      )
    } else {
      Expr::dummy()
    }
  }

  fn parse_bool_literal(&mut self) -> Expr {
    let start = self.current_span();
    let token = self.advance();
    let value = matches!(token.kind, TokenType::True);

    Expr::new_const(
      ExprKind::Literal(Literal::Bool(BoolLit {
        id: NodeId::new(),
        value,
        span: token.span,
      })),
      self.span_from(start),
    )
  }

  fn parse_paren_or_tuple(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::LeftParen);

    // Empty tuple / unit
    if self.eat(TokenType::RightParen) {
      return Expr::new_const(
        ExprKind::Literal(Literal::Unit(UnitLit {
          id: NodeId::new(),
          span: self.span_from(start),
        })),
        self.span_from(start),
      );
    }

    let first = self.parse_expr();

    if self.eat(TokenType::Comma) {
      let mut elements = vec![first];

      while !self.check(&TokenType::RightParen) && !self.is_at_end() {
        elements.push(self.parse_expr());
        if !self.eat(TokenType::Comma) {
          break;
        }
      }

      self.expect(TokenType::RightParen, "to close tuple");

      return Expr::new(ExprKind::Tuple(elements), self.span_from(start));
    }

    self.expect(TokenType::RightParen, "to close parenthesized expression");
    first
  }

  fn parse_array_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::LeftBracket);

    // Empty array
    if self.eat(TokenType::RightBracket) {
      return Expr::new_const(ExprKind::Array(vec![]), self.span_from(start));
    }

    let first = self.parse_expr();

    // repeat syntax: [value; count]
    // TODO: implement repeat syntax and add it to reference
    if self.eat(TokenType::Semicolon) {
      let _count = self.parse_expr();
      self.expect(TokenType::RightBracket, "to close array");
      return Expr::new(ExprKind::Array(vec![first]), self.span_from(start));
    }

    let mut elements = vec![first];

    while self.eat(TokenType::Comma) {
      if self.check(&TokenType::RightBracket) {
        break;
      }
      elements.push(self.parse_expr());
    }

    self.expect(TokenType::RightBracket, "to close array");

    Expr::new(ExprKind::Array(elements), self.span_from(start))
  }

  fn parse_path_or_struct_expr(&mut self) -> Expr {
    let start = self.current_span();
    let path = self.parse_path();

    if self.check(&TokenType::LeftBrace)
      && self
        .peek_ahead(1)
        .is_some_and(|t| matches!(t.kind, TokenType::Dot))
    {
      self.eat(TokenType::LeftBrace);
      let type_path = TypePath {
        id: NodeId::new(),
        path: path.clone(),
        args: None,
        span: self.span_from(start),
      };
      let fields = self.parse_struct_field_inits();
      self.expect(TokenType::RightBrace, "to close struct literal");

      return Expr::new(
        ExprKind::Struct {
          path: type_path,
          fields,
        },
        self.span_from(start),
      );
    }

    Expr::new(ExprKind::Path(path), self.span_from(start))
  }

  fn parse_struct_field_inits(&mut self) -> Vec<StructFieldInit> {
    let mut fields = vec![];

    while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
      if !self.eat(TokenType::Dot) {
        break;
      }

      let field_start = self.current_span();
      let name = self.parse_identifier();

      let value = if self.eat(TokenType::Assign) {
        self.parse_expr()
      } else {
        Expr::new(
          ExprKind::Path(Path {
            id: NodeId::new(),
            root: None,
            segments: vec![name],
            span: *name.span(),
          }),
          *name.span(),
        )
      };

      fields.push(StructFieldInit {
        id: NodeId::new(),
        name,
        value,
        span: self.span_from(field_start),
      });

      if !self.eat(TokenType::Comma) {
        break;
      }
    }

    fields
  }

  fn parse_if_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::If);

    let condition = self.parse_expr();
    let then_block = self.parse_block();

    let else_branch = if self.eat(TokenType::Else) {
      if self.check(&TokenType::If) {
        let else_if = self.parse_if_expr();
        if let ExprKind::If(if_expr) = else_if.kind {
          Some(ElseBranch::If(Box::new(if_expr)))
        } else {
          None
        }
      } else {
        // else block
        Some(ElseBranch::Block(self.parse_block()))
      }
    } else {
      None
    };

    Expr::new(
      ExprKind::If(IfExpr {
        id: NodeId::new(),
        condition: Box::new(condition),
        then_block,
        else_branch,
        span: self.span_from(start),
      }),
      self.span_from(start),
    )
  }

  fn parse_match_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::Match);

    let scrutinee = self.parse_expr();
    let brace_span = self.current_span();
    self.expect(TokenType::LeftBrace, "to open match body");

    let mut arms = vec![];

    while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
      let arm_start = self.current_span();
      let pattern = self.parse_pattern();

      if !self.eat(TokenType::Arrow) {
        self.emit(ExpectedFatArrow {
          found: self.peek().kind.clone(),
          span: self.peek().span,
        });
      }

      let body = self.parse_expr();

      arms.push(MatchArm {
        id: NodeId::new(),
        pattern,
        body,
        span: self.span_from(arm_start),
      });

      if !self.eat(TokenType::Comma) {
        break;
      }
    }

    self.expect(TokenType::RightBrace, "to close match");

    if arms.is_empty() {
      self.emit(EmptyMatch {
        span: self.span_from(brace_span),
      });
    }

    Expr::new(
      ExprKind::Match(MatchExpr {
        id: NodeId::new(),
        scrutinee: Box::new(scrutinee),
        arms,
        span: self.span_from(start),
      }),
      self.span_from(start),
    )
  }

  fn parse_pattern(&mut self) -> Pattern {
    let start = self.current_span();
    let token = self.peek().clone();

    match &token.kind {
      TokenType::Underscore => {
        self.advance();
        Pattern::Wildcard(WildcardPat {
          id: NodeId::new(),
          span: token.span,
        })
      }
      TokenType::True | TokenType::False => {
        let value = matches!(token.kind, TokenType::True);
        self.advance();
        Pattern::Literal(Literal::Bool(BoolLit {
          id: NodeId::new(),
          value,
          span: token.span,
        }))
      }
      TokenType::IntegerLiteral(_) => {
        let expr = self.parse_int_literal();
        if let ExprKind::Literal(lit) = expr.kind {
          Pattern::Literal(lit)
        } else {
          Pattern::Wildcard(WildcardPat {
            id: NodeId::new(),
            span: token.span,
          })
        }
      }
      TokenType::StringLiteral(_) => {
        let expr = self.parse_string_literal();
        if let ExprKind::Literal(lit) = expr.kind {
          Pattern::Literal(lit)
        } else {
          Pattern::Wildcard(WildcardPat {
            id: NodeId::new(),
            span: token.span,
          })
        }
      }
      TokenType::CharLiteral(_) => {
        let expr = self.parse_char_literal();
        if let ExprKind::Literal(lit) = expr.kind {
          Pattern::Literal(lit)
        } else {
          Pattern::Wildcard(WildcardPat {
            id: NodeId::new(),
            span: token.span,
          })
        }
      }
      TokenType::LeftParen => {
        self.advance();
        let mut patterns = vec![];

        while !self.check(&TokenType::RightParen) && !self.is_at_end() {
          patterns.push(self.parse_pattern());
          if !self.eat(TokenType::Comma) {
            break;
          }
        }

        self.expect(TokenType::RightParen, "to close tuple pattern");

        Pattern::Tuple(TuplePattern {
          id: NodeId::new(),
          patterns,
          span: self.span_from(start),
        })
      }
      TokenType::Identifier(_) | TokenType::Package | TokenType::Super => {
        let path = self.parse_path();
        let type_path = crate::TypePath {
          id: NodeId::new(),
          path: path.clone(),
          args: None,
          span: self.span_from(start),
        };

        if self.eat(TokenType::LeftBrace) {
          let mut fields = vec![];

          while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            if !self.eat(TokenType::Dot) {
              break;
            }

            let name = self.parse_identifier();

            if self.eat(TokenType::Assign) {
              let pattern = self.parse_pattern();
              fields.push(StructPatternField::Full { name, pattern });
            } else {
              fields.push(StructPatternField::Shorthand(name));
            }

            if !self.eat(TokenType::Comma) {
              break;
            }
          }

          self.expect(TokenType::RightBrace, "to close struct pattern");

          Pattern::Struct(StructPattern {
            id: NodeId::new(),
            path: type_path,
            fields,
            span: self.span_from(start),
          })
        } else if self.eat(TokenType::LeftParen) {
          // Enum pattern
          let mut patterns = vec![];

          while !self.check(&TokenType::RightParen) && !self.is_at_end() {
            patterns.push(self.parse_pattern());
            if !self.eat(TokenType::Comma) {
              break;
            }
          }

          self.expect(TokenType::RightParen, "to close enum pattern");

          Pattern::Enum(EnumPattern {
            id: NodeId::new(),
            path: type_path,
            patterns,
            span: self.span_from(start),
          })
        } else if path.segments.len() == 1 && path.root.is_none() {
          Pattern::Ident(path.segments.into_iter().next().unwrap())
        } else {
          Pattern::Enum(EnumPattern {
            id: NodeId::new(),
            path: type_path,
            patterns: vec![],
            span: self.span_from(start),
          })
        }
      }
      _ => {
        self.emit(ExpectedPattern {
          found: token.kind.clone(),
          span: token.span,
        });
        Pattern::Wildcard(WildcardPat {
          id: NodeId::new(),
          span: token.span,
        })
      }
    }
  }

  fn parse_loop_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::Loop);

    let body = self.parse_block();

    Expr::new(
      ExprKind::Loop(LoopExpr {
        id: NodeId::new(),
        body,
        span: self.span_from(start),
      }),
      self.span_from(start),
    )
  }

  fn parse_while_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::While);

    let condition = self.parse_expr();
    let body = self.parse_block();

    Expr::new(
      ExprKind::While(WhileExpr {
        id: NodeId::new(),
        condition: Box::new(condition),
        body,
        span: self.span_from(start),
      }),
      self.span_from(start),
    )
  }

  fn parse_for_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::For);

    let binding = self.parse_identifier();
    if !self.eat(TokenType::In) {
      self.emit(MissingInKeyword {
        found: self.peek().kind.clone(),
        span: self.peek().span,
      });
    }
    let iterator = self.parse_expr();
    let body = self.parse_block();

    Expr::new(
      ExprKind::For(ForExpr {
        id: NodeId::new(),
        binding,
        iterator: Box::new(iterator),
        body,
        span: self.span_from(start),
      }),
      self.span_from(start),
    )
  }

  fn parse_break_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::Break);

    let value = if !self.at_expr_terminator() {
      Some(Box::new(self.parse_expr()))
    } else {
      None
    };

    Expr::new(
      ExprKind::Break(BreakExpr {
        id: NodeId::new(),
        value,
        span: self.span_from(start),
      }),
      self.span_from(start),
    )
  }

  fn parse_continue_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::Continue);

    Expr::new(
      ExprKind::Continue(ContinueExpr {
        id: NodeId::new(),
        span: self.span_from(start),
      }),
      self.span_from(start),
    )
  }

  fn parse_return_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::Return);

    let value = if !self.at_expr_terminator() {
      Some(Box::new(self.parse_expr()))
    } else {
      None
    };

    Expr::new(
      ExprKind::Return(ReturnExpr {
        id: NodeId::new(),
        value,
        span: self.span_from(start),
      }),
      self.span_from(start),
    )
  }

  fn parse_range_expr(
    &mut self,
    start_expr: Option<Expr>,
    start: Span,
  ) -> Expr {
    let inclusive = self.eat(TokenType::DotDotEq);
    if !inclusive {
      self.eat(TokenType::DotDot);
    }

    let end = if self.is_range_end_start() {
      Some(Box::new(self.parse_unary_expr()))
    } else {
      None
    };

    Expr::new(
      ExprKind::Range(RangeExpr {
        id: NodeId::new(),
        start: start_expr.map(Box::new),
        end,
        inclusive,
        span: self.span_from(start),
      }),
      self.span_from(start),
    )
  }

  fn is_range_end_start(&self) -> bool {
    matches!(
      self.peek().kind,
      TokenType::IntegerLiteral(_)
        | TokenType::FloatLiteral(_)
        | TokenType::StringLiteral(_)
        | TokenType::CharLiteral(_)
        | TokenType::ByteLiteral(_)
        | TokenType::True
        | TokenType::False
        | TokenType::Identifier(_)
        | TokenType::SelfValue
        | TokenType::Package
        | TokenType::Super
        | TokenType::LeftParen
        | TokenType::LeftBracket
        | TokenType::LeftBrace
        | TokenType::Not
        | TokenType::Minus
        | TokenType::Star
        | TokenType::Amp
        | TokenType::Tilde
        | TokenType::Plus
        | TokenType::At
    )
  }

  fn parse_builtin_expr(&mut self) -> Expr {
    let start = self.current_span();
    self.eat(TokenType::At);

    if !self.is_identifier() {
      self.emit(ExpectedBuiltinName {
        found: self.peek().kind.clone(),
        span: self.peek().span,
      });
      return Expr::dummy();
    }

    let name = self.parse_identifier();

    self.expect(TokenType::LeftParen, "after builtin name");

    let mut args = vec![];

    while !self.check(&TokenType::RightParen) && !self.is_at_end() {
      if self.is_type_start() {
        let ty = self.parse_type();
        args.push(BuiltinArg::Type(ty));
      } else {
        let expr = self.parse_expr();
        args.push(BuiltinArg::Expr(expr));
      }

      if !self.eat(TokenType::Comma) {
        break;
      }
    }

    self.expect(TokenType::RightParen, "to close builtin call");

    Expr::new_const(ExprKind::Builtin { name, args }, self.span_from(start))
  }

  fn is_type_start(&self) -> bool {
    matches!(
      self.peek().kind,
      TokenType::Star
        | TokenType::Const
        | TokenType::Func
        | TokenType::LeftParen
        | TokenType::LeftBracket
        | TokenType::Identifier(_)
        | TokenType::Package
        | TokenType::SelfValue
        | TokenType::Super
    )
  }

  fn parse_call_args(&mut self) -> Vec<Argument> {
    let mut args = vec![];
    let mut named_args: HashMap<String, Span> = HashMap::new();

    while !self.check(&TokenType::RightParen) && !self.is_at_end() {
      let arg_start = self.current_span();

      let (name, value) = if self.is_identifier() {
        if self
          .peek_ahead(1)
          .is_some_and(|t| t.kind == TokenType::Assign)
        {
          let name = self.parse_identifier();
          self.advance(); // consume =
          let value = self.parse_expr();

          let name_str = name.ident.to_string();
          if let Some(&first_span) = named_args.get(&name_str) {
            self.emit(DuplicateNamedArg {
              name: name_str.clone(),
              first_span,
              span: *name.span(),
            });
          } else {
            named_args.insert(name_str, *name.span());
          }

          (Some(name), value)
        } else {
          (None, self.parse_expr())
        }
      } else {
        (None, self.parse_expr())
      };

      args.push(Argument {
        id: NodeId::new(),
        name,
        value,
        span: self.span_from(arg_start),
      });

      if !self.eat(TokenType::Comma) {
        break;
      }
    }

    args
  }

  fn peek_binary_op(&self) -> Option<BinaryOp> {
    match self.peek().kind {
      TokenType::Plus => Some(BinaryOp::Add),
      TokenType::Minus => Some(BinaryOp::Sub),
      TokenType::Star => Some(BinaryOp::Mul),
      TokenType::Slash => Some(BinaryOp::Div),
      TokenType::Percent => Some(BinaryOp::Mod),
      TokenType::EqEq => Some(BinaryOp::Eq),
      TokenType::NotEq => Some(BinaryOp::Ne),
      TokenType::Lt => Some(BinaryOp::Lt),
      TokenType::LtEq => Some(BinaryOp::Le),
      TokenType::Gt => Some(BinaryOp::Gt),
      TokenType::GtEq => Some(BinaryOp::Ge),
      TokenType::AndAnd => Some(BinaryOp::And),
      TokenType::OrOr => Some(BinaryOp::Or),
      TokenType::Amp => Some(BinaryOp::BitAnd),
      TokenType::Pipe => Some(BinaryOp::BitOr),
      TokenType::Caret => Some(BinaryOp::BitXor),
      TokenType::Shl => Some(BinaryOp::Shl),
      TokenType::Shr => Some(BinaryOp::Shr),
      _ => None,
    }
  }

  fn peek_unary_op(&self) -> Option<UnaryOp> {
    match self.peek().kind {
      TokenType::Not => Some(UnaryOp::Not),
      TokenType::Tilde => Some(UnaryOp::BitNot),
      TokenType::Minus => Some(UnaryOp::Neg),
      TokenType::Plus => Some(UnaryOp::Plus),
      TokenType::Star => Some(UnaryOp::Deref),
      _ => None,
    }
  }

  fn peek_assign_op(&self) -> Option<AssignOp> {
    match self.peek().kind {
      TokenType::Assign => Some(AssignOp::Assign),
      TokenType::PlusAssign => Some(AssignOp::AddAssign),
      TokenType::MinusAssign => Some(AssignOp::SubAssign),
      TokenType::StarAssign => Some(AssignOp::MulAssign),
      TokenType::SlashAssign => Some(AssignOp::DivAssign),
      TokenType::PercentAssign => Some(AssignOp::ModAssign),
      TokenType::AmpAssign => Some(AssignOp::BitAndAssign),
      TokenType::PipeAssign => Some(AssignOp::BitOrAssign),
      TokenType::CaretAssign => Some(AssignOp::BitXorAssign),
      TokenType::ShlAssign => Some(AssignOp::ShlAssign),
      TokenType::ShrAssign => Some(AssignOp::ShrAssign),
      TokenType::AndAssign => Some(AssignOp::AndAssign),
      TokenType::OrAssign => Some(AssignOp::OrAssign),
      _ => None,
    }
  }

  fn is_valid_assign_target(expr: &Expr) -> bool {
    matches!(
      expr.kind,
      ExprKind::Path(_)
        | ExprKind::Field { .. }
        | ExprKind::Index { .. }
        | ExprKind::Unary {
          op: UnaryOp::Deref,
          ..
        }
        | ExprKind::SelfValue
    )
  }
}
