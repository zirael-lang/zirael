use crate::ast::{
  Argument, AssignOp, BinaryOp, BoolLit, ByteLit, CharLit, ElseBranch, EnumPattern, ExprKind,
  FloatLit, IfExpr, IntBase, IntLit, Literal, MatchArm, MatchExpr, PathExpr, Pattern,
  StructPattern, StructPatternField, TuplePattern, UnaryOp, UnitLit, WildcardPat, NodeId,
};
use crate::expressions::Expr;
use crate::identifier::Ident;
use crate::lexer::{IntBase as LexIntBase, TokenType};
use crate::parser::{ParseResult, Parser, ParserError, ParserErrorKind};
use crate::{FloatSuffix, IntSuffix};
use zirael_utils::prelude::Span;

impl Parser {
  pub fn parse_expr(&mut self) -> ParseResult<Expr> {
    self.parse_expr_bp(0)
  }

  /// Parse expression with binding power (Pratt parsing)
  fn parse_expr_bp(&mut self, min_bp: u8) -> ParseResult<Expr> {
    let mut lhs = self.parse_expr_atom()?;

    loop {
      let token = self.peek();

      // Handle postfix operators
      if matches!(token.token_type, TokenType::LeftParen) {
        // Function call
        lhs = self.parse_call_expr(lhs)?;
        continue;
      }

      if matches!(token.token_type, TokenType::Dot) {
        // Field access
        lhs = self.parse_field_expr(lhs)?;
        continue;
      }

      if matches!(token.token_type, TokenType::ColonColon) {
        // Path qualifier
        lhs = self.parse_path_qualifier_expr(lhs)?;
        continue;
      }

      if matches!(token.token_type, TokenType::LeftBracket) {
        // Array indexing
        lhs = self.parse_index_expr(lhs)?;
        continue;
      }

      // Ternary operator
      if matches!(token.token_type, TokenType::Question) {
        let r_bp = 13; // Ternary has low precedence
        if r_bp < min_bp {
          break;
        }
        self.advance();
        let then_expr = Box::new(self.parse_expr_bp(0)?);
        self.expect(TokenType::Colon, "in ternary expression")?;
        let else_expr = Box::new(self.parse_expr_bp(r_bp)?);
        let span = lhs.span.to(else_expr.span);
        lhs = Expr {
          id: NodeId::new(),
          span,
          kind: ExprKind::Ternary { condition: Box::new(lhs), then_expr, else_expr },
        };
        continue;
      }

      // Handle type cast (as)
      if self.check(&TokenType::As) {
        let l_bp = 20;
        if l_bp < min_bp {
          break;
        }
        self.advance();
        let target_type = Box::new(self.parse_type()?);
        let span = lhs.span.to(target_type.span());
        lhs = Expr { id: NodeId::new(), span, kind: ExprKind::Cast { expr: Box::new(lhs), target_type } };
        continue;
      }

      // Handle binary operators
      if let Some(op) = self.try_parse_binary_op() {
        let (l_bp, r_bp) = self.infix_binding_power(op);
        if l_bp < min_bp {
          break;
        }
        self.advance();
        let rhs = self.parse_expr_bp(r_bp)?;
        let span = lhs.span.to(rhs.span);
        lhs = Expr {
          id: NodeId::new(),
          span,
          kind: ExprKind::Binary { op, left: Box::new(lhs), right: Box::new(rhs) },
        };
        continue;
      }

      // Handle assignment operators
      if let Some(op) = self.try_parse_assign_op() {
        let r_bp = 14; // Right associative
        if r_bp < min_bp {
          break;
        }
        self.advance();
        let value = Box::new(self.parse_expr_bp(r_bp)?);
        lhs = Expr {
          id: NodeId::new(),
          span: lhs.span.clone(),
          kind: ExprKind::Assign { op, target: Box::new(lhs), value },
        };
        continue;
      }

      break;
    }

    Ok(lhs)
  }

  /// Parse atomic expressions (literals, identifiers, prefix ops, etc.)
  fn parse_expr_atom(&mut self) -> ParseResult<Expr> {
    let start = self.current_span();

    // Prefix unary operators
    if let Some(op) = self.try_parse_unary_op() {
      self.advance();
      let ((), r_bp) = self.prefix_binding_power(op);
      let operand = Box::new(self.parse_expr_bp(r_bp)?);
      return Ok(Expr { id: NodeId::new(), kind: ExprKind::Unary { op, operand }, span: self.span_from(start) });
    }

    // Parenthesized expression or tuple
    if self.eat(TokenType::LeftParen) {
      return self.parse_paren_or_tuple_expr(start);
    }

    // Array literal
    if self.eat(TokenType::LeftBracket) {
      return self.parse_array_expr(start);
    }

    // Block expression
    if self.check(&TokenType::LeftBrace) {
      let block = self.parse_block()?;
      return Ok(Expr { id: NodeId::new(), span: block.span.clone(), kind: ExprKind::Block(block) });
    }

    // If expression
    if self.check(&TokenType::If) {
      let if_expr = self.parse_if_expr()?;
      return Ok(Expr { id: NodeId::new(), span: if_expr.span.clone(), kind: ExprKind::If(if_expr) });
    }

    // Match expression
    if self.check(&TokenType::Match) {
      let match_expr = self.parse_match_expr()?;
      return Ok(Expr { id: NodeId::new(), span: match_expr.span.clone(), kind: ExprKind::Match(match_expr) });
    }

    // Literals
    if let Some(lit) = self.try_parse_literal()? {
      return Ok(Expr { id: NodeId::new(), span: self.span_from(start), kind: ExprKind::Literal(lit) });
    }

    // Self keyword
    if self.check(&TokenType::SelfValue) {
      let token = self.advance();
      return Ok(Expr { id: NodeId::new(), span: token.span, kind: ExprKind::SelfValue });
    }

    // Path or identifier
    if let TokenType::Identifier(_) = self.peek().token_type {
      // Could be a path or just an identifier
      if self.peek_ahead(1).map_or(false, |t| matches!(t.token_type, TokenType::ColonColon)) {
        let path = self.parse_path()?;
        return Ok(Expr {
          id: NodeId::new(),
          span: path.span.clone(),
          kind: ExprKind::Path(PathExpr { id: NodeId::new(), path, span: start }),
        });
      } else {
        let ident = self.parse_identifier()?;
        return Ok(Expr { id: NodeId::new(), span: ident.span.clone(), kind: ExprKind::Ident(ident) });
      }
    }

    Err(ParserError {
      kind: ParserErrorKind::ExpectedExpression,
      span: self.current_span(),
      message: format!("Expected expression, found {}", self.peek().token_type),
    })
  }

  fn parse_paren_or_tuple_expr(&mut self, start: Span) -> ParseResult<Expr> {
    // Empty tuple/unit
    if self.eat(TokenType::RightParen) {
      return Ok(Expr {
        id: NodeId::new(),
        span: self.span_from(start),
        kind: ExprKind::Literal(Literal::Unit(UnitLit { id: NodeId::new(), span: self.span_from(start) })),
      });
    }

    let first = self.parse_expr()?;

    // Parenthesized expression
    if self.eat(TokenType::RightParen) {
      return Ok(Expr { id: NodeId::new(), span: self.span_from(start), kind: ExprKind::Paren(Box::new(first)) });
    }

    // Tuple expression
    self.expect(TokenType::Comma, "in tuple expression")?;
    let mut elements = vec![first];

    if !self.check(&TokenType::RightParen) {
      loop {
        elements.push(self.parse_expr()?);
        if !self.eat(TokenType::Comma) {
          break;
        }
        if self.check(&TokenType::RightParen) {
          break;
        }
      }
    }

    self.expect(TokenType::RightParen, "after tuple")?;

    Ok(Expr { id: NodeId::new(), span: self.span_from(start), kind: ExprKind::Tuple(elements) })
  }

  fn parse_array_expr(&mut self, start: Span) -> ParseResult<Expr> {
    let mut elements = Vec::new();

    if !self.check(&TokenType::RightBracket) {
      loop {
        elements.push(self.parse_expr()?);
        if !self.eat(TokenType::Comma) {
          break;
        }
        if self.check(&TokenType::RightBracket) {
          break;
        }
      }
    }

    self.expect(TokenType::RightBracket, "after array elements")?;

    Ok(Expr { id: NodeId::new(), span: self.span_from(start), kind: ExprKind::Array(elements) })
  }

  fn parse_call_expr(&mut self, callee: Expr) -> ParseResult<Expr> {
    let start = callee.span.clone();
    self.expect(TokenType::LeftParen, "in call expression")?;

    let mut args = Vec::new();

    if !self.check(&TokenType::RightParen) {
      loop {
        args.push(self.parse_argument()?);
        if !self.eat(TokenType::Comma) {
          break;
        }
        if self.check(&TokenType::RightParen) {
          break;
        }
      }
    }

    self.expect(TokenType::RightParen, "after arguments")?;

    Ok(Expr {
      id: NodeId::new(),
      span: self.span_from(start),
      kind: ExprKind::Call { callee: Box::new(callee), args },
    })
  }

  fn parse_argument(&mut self) -> ParseResult<Argument> {
    let start = self.current_span();

    // Named argument: name = value
    if let TokenType::Identifier(_) = &self.peek().token_type {
      if let Some(next) = self.peek_ahead(1) {
        if matches!(next.token_type, TokenType::Assign) {
          let name = self.parse_identifier()?;
          self.expect(TokenType::Assign, "in named argument")?;
          let value = self.parse_expr()?;
          return Ok(Argument { id: NodeId::new(), name: Some(name), value, span: self.span_from(start) });
        }
      }
    }

    // Positional argument
    let value = self.parse_expr()?;
    Ok(Argument { id: NodeId::new(), name: None, value, span: self.span_from(start) })
  }

  fn parse_field_expr(&mut self, object: Expr) -> ParseResult<Expr> {
    let start = object.span.clone();
    self.expect(TokenType::Dot, "in field access")?;
    let field = self.parse_identifier()?;

    Ok(Expr {
      id: NodeId::new(),
      span: self.span_from(start),
      kind: ExprKind::Field { object: Box::new(object), field },
    })
  }

  fn parse_path_qualifier_expr(&mut self, base: Expr) -> ParseResult<Expr> {
    let start = base.span.clone();
    self.expect(TokenType::ColonColon, "in path qualifier")?;
    let segment = self.parse_identifier()?;

    // Type arguments
    let type_args = if self.eat(TokenType::Lt) { Some(self.parse_type_args()?) } else { None };

    Ok(Expr {
      id: NodeId::new(),
      span: self.span_from(start),
      kind: ExprKind::PathQualifier { base: Box::new(base), segment, type_args },
    })
  }

  fn parse_index_expr(&mut self, object: Expr) -> ParseResult<Expr> {
    let start = object.span.clone();
    self.expect(TokenType::LeftBracket, "in array index")?;
    let index = self.parse_expr()?;
    self.expect(TokenType::RightBracket, "after array index")?;

    // Represent as a call to an index operator
    Ok(Expr {
      id: NodeId::new(),
      span: self.span_from(start),
      kind: ExprKind::Call {
        callee: Box::new(Expr {
          id: NodeId::new(),
          span: object.span.clone(),
          kind: ExprKind::Field {
            object: Box::new(object),
            field: Ident { name: "[]".to_string(), span: start.clone() },
          },
        }),
        args: vec![Argument { id: NodeId::new(), name: None, value: index, span: start }],
      },
    })
  }

  fn parse_if_expr(&mut self) -> ParseResult<IfExpr> {
    let start = self.expect(TokenType::If, "at start of if expression")?.span;
    let condition = Box::new(self.parse_expr()?);
    let then_block = self.parse_block()?;

    let else_branch = if self.eat(TokenType::Else) {
      if self.check(&TokenType::If) {
        Some(ElseBranch::If(Box::new(self.parse_if_expr()?)))
      } else {
        Some(ElseBranch::Block(self.parse_block()?))
      }
    } else {
      None
    };

    Ok(IfExpr { id: NodeId::new(), condition, then_block, else_branch, span: self.span_from(start) })
  }

  fn parse_match_expr(&mut self) -> ParseResult<MatchExpr> {
    let start = self.expect(TokenType::Match, "at start of match")?.span;
    let scrutinee = Box::new(self.parse_expr()?);

    self.expect(TokenType::LeftBrace, "after match scrutinee")?;

    let mut arms = Vec::new();
    while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
      arms.push(self.parse_match_arm()?);

      // Allow optional comma after arm
      self.eat(TokenType::Comma);
    }

    self.expect(TokenType::RightBrace, "after match arms")?;

    Ok(MatchExpr { id: NodeId::new(), scrutinee, arms, span: self.span_from(start) })
  }

  fn parse_match_arm(&mut self) -> ParseResult<MatchArm> {
    let start = self.current_span();
    let pattern = self.parse_pattern()?;
    self.expect(TokenType::Arrow, "after match pattern")?;
    let body = self.parse_expr()?;

    Ok(MatchArm { id: NodeId::new(), pattern, body, span: self.span_from(start) })
  }

  pub fn parse_pattern(&mut self) -> ParseResult<Pattern> {
    let start = self.current_span();

    // Wildcard pattern: _
    if self.eat(TokenType::Underscore) {
      return Ok(Pattern::Wildcard(WildcardPat { id: NodeId::new(), span: self.span_from(start) }));
    }

    // Literal pattern
    if let Some(lit) = self.try_parse_literal()? {
      return Ok(Pattern::Literal(lit));
    }

    // Identifier or path pattern
    if let TokenType::Identifier(_) = self.peek().token_type {
      // Check if this is a struct pattern or enum pattern
      return if self
        .peek_ahead(1)
        .map_or(false, |t| matches!(t.token_type, TokenType::LeftBrace | TokenType::LeftParen))
      {
        let path = self.parse_path()?;
        let type_path = crate::ast::TypePath {
          id: NodeId::new(),
          path: path.clone(),
          args: None,
          span: path.span.clone(),
        };

        if self.check(&TokenType::LeftBrace) {
          // Struct pattern
          self.parse_struct_pattern(type_path)
        } else {
          // Enum pattern with tuple
          self.parse_enum_pattern(type_path)
        }
      } else {
        let ident = self.parse_identifier()?;
        Ok(Pattern::Ident(ident))
      };
    }

    // Tuple pattern
    if self.eat(TokenType::LeftParen) {
      return self.parse_tuple_pattern(start);
    }

    Err(ParserError {
      kind: ParserErrorKind::ExpectedPattern,
      span: self.current_span(),
      message: format!("Expected pattern, found {}", self.peek().token_type),
    })
  }

  fn parse_struct_pattern(&mut self, path: crate::ast::TypePath) -> ParseResult<Pattern> {
    let start = path.span.clone();
    self.expect(TokenType::LeftBrace, "in struct pattern")?;

    let mut fields = Vec::new();

    if !self.check(&TokenType::RightBrace) {
      loop {
        let field_name = self.parse_identifier()?;

        let field = if self.eat(TokenType::Colon) {
          let pattern = self.parse_pattern()?;
          StructPatternField::Full { name: field_name, pattern }
        } else {
          StructPatternField::Shorthand(field_name)
        };

        fields.push(field);

        if !self.eat(TokenType::Comma) {
          break;
        }
        if self.check(&TokenType::RightBrace) {
          break;
        }
      }
    }

    self.expect(TokenType::RightBrace, "after struct pattern fields")?;

    Ok(Pattern::Struct(StructPattern { id: NodeId::new(), path, fields, span: self.span_from(start) }))
  }

  fn parse_enum_pattern(&mut self, path: crate::ast::TypePath) -> ParseResult<Pattern> {
    let start = path.span.clone();
    self.expect(TokenType::LeftParen, "in enum pattern")?;

    let mut patterns = Vec::new();

    if !self.check(&TokenType::RightParen) {
      loop {
        patterns.push(self.parse_pattern()?);
        if !self.eat(TokenType::Comma) {
          break;
        }
        if self.check(&TokenType::RightParen) {
          break;
        }
      }
    }

    self.expect(TokenType::RightParen, "after enum pattern")?;

    Ok(Pattern::Enum(EnumPattern { id: NodeId::new(), path, patterns, span: self.span_from(start) }))
  }

  fn parse_tuple_pattern(&mut self, start: Span) -> ParseResult<Pattern> {
    let mut patterns = Vec::new();

    if !self.check(&TokenType::RightParen) {
      loop {
        patterns.push(self.parse_pattern()?);
        if !self.eat(TokenType::Comma) {
          break;
        }
        if self.check(&TokenType::RightParen) {
          break;
        }
      }
    }

    self.expect(TokenType::RightParen, "after tuple pattern")?;

    Ok(Pattern::Tuple(TuplePattern { id: NodeId::new(), patterns, span: self.span_from(start) }))
  }

  fn try_parse_literal(&mut self) -> ParseResult<Option<Literal>> {
    let token = self.peek().clone();

    let lit = match &token.token_type {
      TokenType::IntegerLiteral(base) => {
        self.advance();
        let (value, ast_base) = match base {
          LexIntBase::Decimal(v) => (v.clone(), IntBase::Decimal),
          LexIntBase::Binary(v) => (v.clone(), IntBase::Binary),
          LexIntBase::Octal(v) => (v.clone(), IntBase::Octal),
          LexIntBase::Hexadecimal(v) => (v.clone(), IntBase::Hexadecimal),
        };
        let suffix = self.parse_int_suffix(&token.lexeme, &value)?;
        Literal::Int(IntLit { id: NodeId::new(), value, base: ast_base, suffix, span: token.span })
      }
      TokenType::FloatLiteral(value) => {
        self.advance();
        let suffix = self.parse_float_suffix(&token.lexeme, value)?;
        Literal::Float(FloatLit { id: NodeId::new(), value: value.clone(), suffix, span: token.span })
      }
      TokenType::StringLiteral(value) => {
        self.advance();
        Literal::String(crate::ast::StringLit {
          id: NodeId::new(),
          value: value.clone(),
          raw: token.lexeme.clone(),
          span: token.span,
        })
      }
      TokenType::CharLiteral(ch) => {
        self.advance();
        Literal::Char(CharLit { id: NodeId::new(), value: *ch, raw: token.lexeme.clone(), span: token.span })
      }
      TokenType::ByteLiteral(b) => {
        self.advance();
        Literal::Byte(ByteLit { id: NodeId::new(), value: *b, raw: token.lexeme.clone(), span: token.span })
      }
      TokenType::True => {
        self.advance();
        Literal::Bool(BoolLit { id: NodeId::new(), value: true, span: token.span })
      }
      TokenType::False => {
        self.advance();
        Literal::Bool(BoolLit { id: NodeId::new(), value: false, span: token.span })
      }
      _ => return Ok(None),
    };

    Ok(Some(lit))
  }

  fn try_parse_binary_op(&self) -> Option<BinaryOp> {
    match &self.peek().token_type {
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

  fn try_parse_assign_op(&self) -> Option<AssignOp> {
    match &self.peek().token_type {
      TokenType::Assign => Some(AssignOp::Assign),
      TokenType::PlusAssign => Some(AssignOp::AddAssign),
      TokenType::MinusAssign => Some(AssignOp::SubAssign),
      TokenType::StarAssign => Some(AssignOp::MulAssign),
      TokenType::SlashAssign => Some(AssignOp::DivAssign),
      TokenType::PercentAssign => Some(AssignOp::ModAssign),
      TokenType::AndAssign => Some(AssignOp::AndAssign),
      TokenType::OrAssign => Some(AssignOp::OrAssign),
      TokenType::AmpAssign => Some(AssignOp::BitAndAssign),
      TokenType::PipeAssign => Some(AssignOp::BitOrAssign),
      TokenType::CaretAssign => Some(AssignOp::BitXorAssign),
      TokenType::ShlAssign => Some(AssignOp::ShlAssign),
      TokenType::ShrAssign => Some(AssignOp::ShrAssign),
      _ => None,
    }
  }

  fn try_parse_unary_op(&self) -> Option<UnaryOp> {
    match &self.peek().token_type {
      TokenType::Not => Some(UnaryOp::Not),
      TokenType::Tilde => Some(UnaryOp::BitNot),
      TokenType::Minus => Some(UnaryOp::Neg),
      TokenType::Plus => Some(UnaryOp::Plus),
      TokenType::Star => Some(UnaryOp::Deref),
      _ => None,
    }
  }

  fn infix_binding_power(&self, op: BinaryOp) -> (u8, u8) {
    let prec = op.precedence();
    if op.is_left_associative() { (prec, prec + 1) } else { (prec, prec) }
  }

  fn prefix_binding_power(&self, _op: UnaryOp) -> ((), u8) {
    ((), 2)
  }

  fn parse_int_suffix(
    &self,
    lexeme: &str,
    value: &str,
  ) -> ParseResult<Option<crate::ast::IntSuffix>> {
    let suffix_start = lexeme.find(value).unwrap_or(0) + value.len();
    let suffix = &lexeme[suffix_start..];

    if suffix.is_empty() {
      return Ok(None);
    }

    match suffix {
      "i8" => Ok(Some(IntSuffix::I8)),
      "i16" => Ok(Some(IntSuffix::I16)),
      "i32" => Ok(Some(IntSuffix::I32)),
      "i64" => Ok(Some(IntSuffix::I64)),
      "i128" => Ok(Some(IntSuffix::I128)),
      "isize" => Ok(Some(IntSuffix::ISize)),
      "u8" => Ok(Some(IntSuffix::U8)),
      "u16" => Ok(Some(IntSuffix::U16)),
      "u32" => Ok(Some(IntSuffix::U32)),
      "u64" => Ok(Some(IntSuffix::U64)),
      "u128" => Ok(Some(IntSuffix::U128)),
      "usize" => Ok(Some(IntSuffix::USize)),
      _ => Err(ParserError {
        kind: ParserErrorKind::InvalidLiteral,
        span: self.peek().span,
        message: format!(
          "Invalid integer suffix: '{}', expected one of: i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize",
          suffix
        ),
      }),
    }
  }

  fn parse_float_suffix(
    &self,
    lexeme: &str,
    value: &str,
  ) -> ParseResult<Option<crate::ast::FloatSuffix>> {
    let suffix_start = lexeme.find(value).unwrap_or(0) + value.len();
    let suffix = &lexeme[suffix_start..];

    if suffix.is_empty() {
      return Ok(None);
    }

    match suffix {
      "f32" => Ok(Some(FloatSuffix::F32)),
      "f64" => Ok(Some(FloatSuffix::F64)),
      _ => Err(ParserError {
        kind: ParserErrorKind::InvalidLiteral,
        span: self.peek().span,
        message: format!("Invalid float suffix: '{}', expected one of: f32, f64", suffix),
      }),
    }
  }
}
