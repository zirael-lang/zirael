use crate::lexer::{Token, TokenType};
use std::fmt;
use zirael_utils::prelude::Span;

pub type ParseResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone)]
pub struct ParserError {
  pub kind: ParserErrorKind,
  pub span: Span,
  pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserErrorKind {
  UnexpectedToken,
  UnexpectedEof,
  InvalidSyntax,
  ExpectedExpression,
  ExpectedStatement,
  ExpectedType,
  ExpectedIdentifier,
  ExpectedPattern,
  InvalidLiteral,
}

impl fmt::Display for ParserError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:?}: {} at {:?}", self.kind, self.message, self.span)
  }
}

impl std::error::Error for ParserError {}

pub struct Parser {
  tokens: Vec<Token>,
  pos: usize,
  /// Errors collected during parsing for error recovery
  pub errors: Vec<ParserError>,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    let tokens: Vec<Token> =
      tokens.into_iter().filter(|t| !matches!(t.token_type, TokenType::Whitespace)).collect();

    Self { tokens, pos: 0, errors: Vec::new() }
  }

  /// Get the current token without consuming
  #[inline]
  pub fn peek(&self) -> &Token {
    self
      .tokens
      .get(self.pos)
      .unwrap_or_else(|| self.tokens.last().expect("Token stream should never be empty"))
  }

  /// Get token at offset from the current position
  #[inline]
  pub fn peek_ahead(&self, offset: usize) -> Option<&Token> {
    self.tokens.get(self.pos + offset)
  }

  /// Check if the current token matches a type
  #[inline]
  pub fn check(&self, token_type: &TokenType) -> bool {
    &self.peek().token_type == token_type
  }

  /// Check if any of the token types match
  #[inline]
  pub fn check_any(&self, token_types: &[TokenType]) -> bool {
    token_types.iter().any(|tt| self.check(tt))
  }

  /// Consume and return the current token
  #[inline]
  pub fn advance(&mut self) -> Token {
    let token = self.peek().clone();
    if !matches!(token.token_type, TokenType::Eof) {
      self.pos += 1;
    }
    token
  }

  /// Check if the current token matches and consume it
  pub fn eat(&mut self, token_type: TokenType) -> bool {
    if self.check(&token_type) {
      self.advance();
      true
    } else {
      false
    }
  }

  /// Expect a specific token and consume it or report an error
  pub fn expect(&mut self, token_type: TokenType, context: &str) -> ParseResult<Token> {
    if self.check(&token_type) {
      Ok(self.advance())
    } else {
      let current = self.peek();
      Err(ParserError {
        kind: ParserErrorKind::UnexpectedToken,
        span: current.span.clone(),
        message: format!("Expected {} {}, found {}", token_type, context, current.token_type),
      })
    }
  }

  pub fn report_error(&mut self, error: ParserError) {
    self.errors.push(error);
  }

  /// Synchronize the parser state after an error by advancing to a recovery point
  pub fn synchronize(&mut self) {
    self.advance();

    while !matches!(self.peek().token_type, TokenType::Eof) {
      if matches!(
        self.tokens.get(self.pos.saturating_sub(1)).map(|t| &t.token_type),
        Some(TokenType::Semicolon)
      ) {
        return;
      }

      match self.peek().token_type {
        TokenType::Func
        | TokenType::Struct
        | TokenType::Import
        | TokenType::Pub
        | TokenType::If
        | TokenType::While
        | TokenType::For
        | TokenType::Loop
        | TokenType::Return
        | TokenType::LeftBrace => return,
        _ => {}
      }

      self.advance();
    }
  }

  #[inline]
  pub fn is_at_end(&self) -> bool {
    matches!(self.peek().token_type, TokenType::Eof)
  }

  pub fn span_from(&self, start: Span) -> Span {
    let end = if self.pos > 0 { &self.tokens[self.pos - 1].span } else { &start };
    Span::new(start.start, end.end, start.file_id)
  }

  pub fn current_span(&self) -> Span {
    self.peek().span.clone()
  }
}
