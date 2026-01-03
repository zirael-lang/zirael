use crate::identifier::Ident;
use crate::lexer::{Token, TokenType};
use crate::parser::errors::{
  ExpectedIdentifier, ExpectedTokens, UnexpectedToken,
};
use zirael_diagnostics::DiagnosticCtx;
use zirael_diagnostics::ToDiagnostic;
use zirael_utils::prelude::Span;

pub const ITEM_TOKENS: &[TokenType] =
  &[TokenType::Mod, TokenType::Const, TokenType::Func];

pub struct Parser<'dcx> {
  tokens: Vec<Token>,
  pos: usize,

  /// doc comments on current item
  pub doc_comment: Option<Vec<String>>,

  dcx: &'dcx DiagnosticCtx,
}

impl<'dcx> Parser<'dcx> {
  pub fn new(tokens: Vec<Token>, dcx: &'dcx DiagnosticCtx) -> Self {
    let tokens: Vec<Token> = tokens
      .into_iter()
      .filter(|t| !matches!(t.kind, TokenType::Whitespace))
      .collect();

    Self {
      tokens,
      pos: 0,
      dcx,
      doc_comment: None,
    }
  }

  pub fn push_comment(&mut self, comment: String) {
    if let Some(comments) = &mut self.doc_comment {
      comments.push(comment);
    } else {
      self.doc_comment = Some(vec![comment]);
    }
  }

  pub fn has_doc_comment(&self) -> bool {
    self.doc_comment.is_some()
  }

  pub fn emit(&self, error: impl ToDiagnostic) {
    self.dcx.emit(error);
  }

  /// Get the current token without consuming
  #[inline]
  pub fn peek(&self) -> &Token {
    self.tokens.get(self.pos).unwrap_or_else(|| {
      self
        .tokens
        .last()
        .expect("Token stream should never be empty")
    })
  }

  /// Get the previous token
  #[inline]
  pub fn previous(&self) -> &Token {
    &self.tokens[self.pos - 1]
  }

  /// Get token at offset from the current position
  #[inline]
  pub fn peek_ahead(&self, offset: usize) -> Option<&Token> {
    self.tokens.get(self.pos + offset)
  }

  /// Check if the current token matches a type
  #[inline]
  pub fn check(&self, token_type: &TokenType) -> bool {
    &self.peek().kind == token_type
  }

  #[inline]
  pub fn check_if(&self, predicate: fn(&TokenType) -> bool) -> bool {
    (predicate)(&self.peek().kind)
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
    if !matches!(token.kind, TokenType::Eof) {
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
  pub fn expect(
    &mut self,
    token_type: TokenType,
    context: &str,
  ) -> Option<Token> {
    if self.check(&token_type) {
      Some(self.advance())
    } else {
      let current = self.peek();
      self.emit(UnexpectedToken {
        span: current.span,
        expected: ExpectedTokens::one(token_type),
        found: current.kind.clone(),
        context: context.to_owned(),
      });
      None
    }
  }

  pub fn expect_any(
    &mut self,
    types: &[TokenType],
    context: &str,
  ) -> Option<Token> {
    let current = self.peek();

    if types.contains(&current.kind) {
      Some(self.advance())
    } else {
      self.emit(UnexpectedToken {
        span: current.span,
        expected: ExpectedTokens::many(types.to_vec()),
        found: current.kind.clone(),
        context: context.to_owned(),
      });
      None
    }
  }

  pub fn advance_until_one_of(&mut self, types: &[TokenType]) {
    loop {
      if types.contains(&self.peek().kind) || self.is_at_end() {
        break;
      }

      self.advance();
    }
  }

  pub fn is(&mut self, token_type: TokenType) -> Option<Token> {
    if self.check(&token_type) {
      Some(self.advance())
    } else {
      None
    }
  }

  #[inline]
  pub fn is_at_end(&self) -> bool {
    matches!(self.peek().kind, TokenType::Eof) || self.pos >= self.tokens.len()
  }

  pub fn span_from(&self, start: Span) -> Span {
    let end = if self.pos > 0 {
      &self.tokens[self.pos - 1].span
    } else {
      &start
    };
    Span::new(start.start, end.end, start.file_id)
  }

  pub fn current_span(&self) -> Span {
    self.peek().span
  }

  pub fn parse_identifier(&mut self) -> Ident {
    let token = self.advance();
    if let TokenType::Identifier(name) = &token.kind {
      Ident::new(name.as_str(), token.span)
    } else {
      self.emit(ExpectedIdentifier {
        span: token.span,
        found: token.kind,
      });
      Ident::dummy()
    }
  }

  pub fn eat_all(&mut self, token: TokenType) {
    while self.peek().kind == token {
      self.advance();
    }
  }

  pub fn eat_semis(&mut self) {
    self.eat_all(TokenType::Semicolon);
  }
}
