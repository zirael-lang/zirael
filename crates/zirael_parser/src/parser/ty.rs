use crate::expressions::Expr;
use crate::parser::Parser;
use crate::parser::errors::{ConstAloneInType, ExpectedType};
use crate::{
  ArrayType, FunctionType, GenericParam, GenericParams, Mutability, NodeId,
  OptionalType, PointerType, PrimitiveKind, PrimitiveType, TokenType,
  TupleType, Type, TypePath, UnitType,
};
use zirael_source::span::Span;

impl Parser<'_> {
  pub fn parse_type(&mut self) -> Type {
    let start = self.current_span();
    let mut ty = self.parse_type_inner(start);

    while self.eat(TokenType::Question) {
      let span = self.span_from(start);
      ty = Type::Optional(OptionalType {
        id: NodeId::new(),
        inner: Box::new(ty),
        span,
      });
    }

    ty
  }

  fn parse_type_inner(&mut self, start: Span) -> Type {
    if self.eat(TokenType::Star) {
      return self.parse_pointer_type(start);
    }

    if self.eat(TokenType::Const) {
      let const_span = self.previous().span;

      return match self.peek().kind {
        TokenType::Func => {
          self.advance();
          self.parse_function_type(true, start)
        }
        _ => {
          self.emit(ConstAloneInType { span: const_span });
          Type::Invalid
        }
      };
    }

    if self.eat(TokenType::Func) {
      return self.parse_function_type(false, start);
    }

    match self.peek().kind.clone() {
      TokenType::LeftParen => {
        self.advance();
        self.parse_parenthesized_type(start)
      }
      TokenType::LeftBracket => {
        self.advance();
        self.parse_array_type(start)
      }
      TokenType::Identifier(name) => self.parse_identifier_type(start, name),
      TokenType::Package | TokenType::SelfValue | TokenType::Super => {
        self.parse_path_type(start)
      }
      _ => {
        self.emit(ExpectedType {
          span: self.current_span(),
        });
        Type::Invalid
      }
    }
  }

  fn parse_pointer_type(&mut self, start: Span) -> Type {
    let mutability = self.parse_mutability();
    let inner = Box::new(self.parse_type());

    Type::Pointer(PointerType {
      id: NodeId::new(),
      mutability,
      inner,
      span: self.span_from(start),
    })
  }

  fn parse_parenthesized_type(&mut self, start: Span) -> Type {
    if self.eat(TokenType::RightParen) {
      return Type::Unit(UnitType {
        id: NodeId::new(),
        span: self.span_from(start),
      });
    }

    let first = self.parse_type();

    if self.eat(TokenType::Comma) {
      let mut elements = vec![first];

      while !self.check(&TokenType::RightParen) && !self.is_at_end() {
        elements.push(self.parse_type());

        if !self.eat(TokenType::Comma) {
          break;
        }
      }

      self.expect(TokenType::RightParen, "to close tuple type");

      return Type::Tuple(TupleType {
        id: NodeId::new(),
        elements,
        span: self.span_from(start),
      });
    }

    self.expect(TokenType::RightParen, "to close type");
    first
  }

  fn parse_array_type(&mut self, start: Span) -> Type {
    let element = Box::new(self.parse_type());

    if self
      .expect(TokenType::Semicolon, "after array element type")
      .is_none()
    {
      self.advance_until_one_of(&[TokenType::RightBracket]);
    }

    let size = self.parse_array_size_expr();

    self.expect(TokenType::RightBracket, "to close array type");

    Type::Array(ArrayType {
      id: NodeId::new(),
      element,
      size,
      span: self.span_from(start),
    })
  }

  fn parse_array_size_expr(&mut self) -> Expr {
    if self.check(&TokenType::RightBracket) || self.is_at_end() {
      return Expr::dummy();
    }

    while !self.check(&TokenType::RightBracket) && !self.is_at_end() {
      self.advance();
    }

    Expr::dummy()
  }

  fn parse_identifier_type(&mut self, start: Span, name: String) -> Type {
    if let Some(kind) = Self::primitive_kind(&name) {
      let next = self.peek_ahead(1).map(|t| t.kind.clone());

      if !matches!(next, Some(TokenType::ColonColon) | Some(TokenType::Lt)) {
        let span = self.advance().span;

        return Type::Primitive(PrimitiveType {
          id: NodeId::new(),
          kind,
          span,
        });
      }
    }

    self.parse_path_type(start)
  }

  fn parse_path_type(&mut self, start: Span) -> Type {
    let path = self.parse_path();
    let args = self.parse_type_arguments();

    Type::Path(TypePath {
      id: NodeId::new(),
      path,
      args,
      span: self.span_from(start),
    })
  }

  fn parse_type_arguments(&mut self) -> Option<Vec<Type>> {
    if !self.eat(TokenType::Lt) {
      return None;
    }

    let mut args = Vec::new();

    if self.check(&TokenType::Gt) {
      self.advance();
      return Some(args);
    }

    loop {
      args.push(self.parse_type());

      if self.eat(TokenType::Comma) {
        if self.check(&TokenType::Gt) {
          self.advance();
          break;
        }

        continue;
      }

      self.expect(TokenType::Gt, "to close type arguments");
      break;
    }

    Some(args)
  }

  fn parse_function_type(&mut self, is_const: bool, span: Span) -> Type {
    let mut params = Vec::new();

    if self
      .expect(TokenType::LeftParen, "after func in function type")
      .is_some()
    {
      if !self.check(&TokenType::RightParen) {
        loop {
          params.push(self.parse_type());

          if self.eat(TokenType::Comma) {
            continue;
          }

          break;
        }
      }

      self.expect(TokenType::RightParen, "to close function type parameters");
    }

    let return_type = if self.eat(TokenType::Arrow) {
      Box::new(self.parse_type())
    } else {
      Box::new(Type::Unit(UnitType {
        id: NodeId::new(),
        span: self.span_from(span),
      }))
    };

    Type::Function(FunctionType {
      id: NodeId::new(),
      is_const,
      params,
      return_type,
      span: self.span_from(span),
    })
  }

  fn parse_mutability(&mut self) -> Mutability {
    if self.eat(TokenType::Const) {
      Mutability::Const
    } else if self.eat(TokenType::Mut) {
      Mutability::Mut
    } else {
      Mutability::Const
    }
  }

  fn primitive_kind(name: &str) -> Option<PrimitiveKind> {
    match name {
      "i8" => Some(PrimitiveKind::I8),
      "i16" => Some(PrimitiveKind::I16),
      "i32" => Some(PrimitiveKind::I32),
      "i64" => Some(PrimitiveKind::I64),
      "i128" => Some(PrimitiveKind::I128),
      "isize" => Some(PrimitiveKind::ISize),
      "u8" => Some(PrimitiveKind::U8),
      "u16" => Some(PrimitiveKind::U16),
      "u32" => Some(PrimitiveKind::U32),
      "u64" => Some(PrimitiveKind::U64),
      "u128" => Some(PrimitiveKind::U128),
      "usize" => Some(PrimitiveKind::USize),
      "f32" => Some(PrimitiveKind::F32),
      "f64" => Some(PrimitiveKind::F64),
      "bool" => Some(PrimitiveKind::Bool),
      "char" => Some(PrimitiveKind::Char),
      _ => None,
    }
  }
}
