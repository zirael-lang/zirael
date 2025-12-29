use crate::ast::{
  ArrayType, FunctionType, PrimitiveKind, PrimitiveType, ReferenceType, TupleType, Type, TypePath,
  NodeId, UnitType,
};
use crate::lexer::TokenType;
use crate::parser::{ParseResult, Parser};
use zirael_utils::prelude::Span;

impl Parser {
  /// Parse a type expression
  pub fn parse_type(&mut self) -> ParseResult<Type> {
    self.parse_type_inner()
  }

  fn parse_type_inner(&mut self) -> ParseResult<Type> {
    let start = self.current_span();

    // Reference type: &T or &mut T
    if self.eat(TokenType::Amp) {
      let is_mut = self.eat(TokenType::Mut);
      let inner = Box::new(self.parse_type_inner()?);
      return Ok(Type::Reference(ReferenceType { id: NodeId::new(), is_mut, inner, span: self.span_from(start) }));
    }

    // Function type: func(T, U) -> R
    if self.eat(TokenType::Func) {
      return self.parse_function_type(start);
    }

    // Tuple or grouped type
    if self.eat(TokenType::LeftParen) {
      return self.parse_tuple_or_unit_type(start);
    }

    // Array type: [T; N]
    if self.eat(TokenType::LeftBracket) {
      let element = Box::new(self.parse_type()?);
      self.expect(TokenType::Semicolon, "in array type")?;
      let size = self.parse_expr()?;
      self.expect(TokenType::RightBracket, "after array type")?;
      return Ok(Type::Array(ArrayType { id: NodeId::new(), element, size, span: self.span_from(start) }));
    }

    // Primitive or path type
    if let Some(primitive) = self.try_parse_primitive_type() {
      return Ok(Type::Primitive(primitive));
    }

    // Path type (e.g., Vec<T>, module::Type)
    let path = self.parse_path()?;

    // Type arguments: Type<T, U>
    let args = if self.eat(TokenType::Lt) { Some(self.parse_type_args()?) } else { None };

    Ok(Type::Path(TypePath { id: NodeId::new(), path, args, span: self.span_from(start) }))
  }

  fn parse_function_type(&mut self, start: Span) -> ParseResult<Type> {
    self.expect(TokenType::LeftParen, "in function type")?;

    let mut params = Vec::new();
    if !self.check(&TokenType::RightParen) {
      loop {
        params.push(self.parse_type()?);
        if !self.eat(TokenType::Comma) {
          break;
        }
        if self.check(&TokenType::RightParen) {
          break;
        }
      }
    }

    self.expect(TokenType::RightParen, "after function parameters")?;
    self.expect(TokenType::Arrow, "in function type")?;
    let return_type = Box::new(self.parse_type()?);

    Ok(Type::Function(FunctionType { id: NodeId::new(), params, return_type, span: self.span_from(start) }))
  }

  fn parse_tuple_or_unit_type(&mut self, start: Span) -> ParseResult<Type> {
    if self.eat(TokenType::RightParen) {
      return Ok(Type::Unit(UnitType { id: NodeId::new(), span: self.span_from(start) }));
    }

    let first = self.parse_type()?;

    if self.eat(TokenType::RightParen) {
      return Ok(first);
    }

    self.expect(TokenType::Comma, "in tuple type")?;

    let mut elements = vec![first];

    if !self.check(&TokenType::RightParen) {
      loop {
        elements.push(self.parse_type()?);
        if !self.eat(TokenType::Comma) {
          break;
        }
        if self.check(&TokenType::RightParen) {
          break;
        }
      }
    }

    self.expect(TokenType::RightParen, "after tuple type")?;

    Ok(Type::Tuple(TupleType { id: NodeId::new(), elements, span: self.span_from(start) }))
  }

  pub(crate) fn parse_type_args(&mut self) -> ParseResult<Vec<Type>> {
    let mut args = Vec::new();

    if !self.check(&TokenType::Gt) {
      loop {
        args.push(self.parse_type()?);
        if !self.eat(TokenType::Comma) {
          break;
        }
        if self.check(&TokenType::Gt) {
          break;
        }
      }
    }

    self.expect(TokenType::Gt, "after type arguments")?;
    Ok(args)
  }

  fn try_parse_primitive_type(&mut self) -> Option<PrimitiveType> {
    let token = self.peek();
    let kind = match &token.token_type {
      TokenType::Identifier(name) => match name.as_str() {
        "i8" => PrimitiveKind::I8,
        "i16" => PrimitiveKind::I16,
        "i32" => PrimitiveKind::I32,
        "i64" => PrimitiveKind::I64,
        "i128" => PrimitiveKind::I128,
        "isize" => PrimitiveKind::ISize,
        "u8" => PrimitiveKind::U8,
        "u16" => PrimitiveKind::U16,
        "u32" => PrimitiveKind::U32,
        "u64" => PrimitiveKind::U64,
        "u128" => PrimitiveKind::U128,
        "usize" => PrimitiveKind::USize,
        "f32" => PrimitiveKind::F32,
        "f64" => PrimitiveKind::F64,
        "bool" => PrimitiveKind::Bool,
        "char" => PrimitiveKind::Char,
        _ => return None,
      },
      _ => return None,
    };

    let span = token.span.clone();
    self.advance();
    Some(PrimitiveType { id: NodeId::new(), kind, span })
  }
}
