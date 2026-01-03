use crate::lexer::lexer_errors::{LexError, LexErrorKind, LexResult};
use crate::lexer::nfc::is_xid_start;
use crate::lexer::tokens::{Token, TokenType};
use zirael_diagnostics::DiagnosticCtx;
use zirael_source::prelude::SourceFile;
use zirael_utils::prelude::{SourceFileId, Span};

#[derive(Debug)]
pub struct Lexer<'ctx> {
  pub source: String,
  chars: Vec<char>,
  file_id: SourceFileId,

  pos: usize,
  pub offset: usize,
  current: Option<char>,

  pub dcx: &'ctx DiagnosticCtx,
}

impl<'ctx> Lexer<'ctx> {
  pub fn new(sf: &SourceFile, dcx: &'ctx DiagnosticCtx) -> Self {
    let source = Self::strip_bom(sf.content().to_string());
    let source = Self::strip_shebang(source);

    let chars: Vec<char> = source.chars().collect();
    let current = chars.get(0).copied();

    Lexer {
      source,
      chars,
      file_id: sf.file_id,
      pos: 0,
      offset: 0,
      current,
      dcx,
    }
  }

  pub(crate) fn current_position(&self) -> usize {
    self.pos
  }

  pub fn dcx(&self) -> &DiagnosticCtx {
    self.dcx
  }

  fn strip_bom(source: String) -> String {
    if source.starts_with('\u{FEFF}') {
      source[3..].to_string() // BOM is 3 bytes in UTF-8
    } else {
      source
    }
  }

  fn strip_shebang(source: String) -> String {
    if source.starts_with("#!") {
      // Find the first newline and skip to after it
      if let Some(newline_pos) = source.find('\n') {
        source[newline_pos + 1..].to_string()
      } else {
        String::new()
      }
    } else {
      source
    }
  }

  pub(crate) fn make_span(&self, start_offset: usize) -> Span {
    Span::new(start_offset, self.offset, self.file_id)
  }

  pub(crate) fn make_char_span(&self) -> Span {
    Span::new(
      self.offset,
      self.offset + self.current.map_or(0, |c| c.len_utf8()),
      self.file_id,
    )
  }

  pub(crate) fn peek(&self) -> Option<char> {
    self.current
  }

  pub(crate) fn peek_ahead(&self, n: usize) -> Option<char> {
    self.chars.get(self.pos + n).copied()
  }

  pub(crate) fn advance(&mut self) -> Option<char> {
    let ch = self.current?;

    self.offset += ch.len_utf8();
    self.pos += 1;

    self.current = self.chars.get(self.pos).copied();
    Some(ch)
  }

  pub(crate) fn is_eof(&self) -> bool {
    self.current.is_none()
  }

  /// Skip the current character and record an error
  pub(crate) fn skip_with_error(&mut self, kind: LexErrorKind) {
    let span = self.make_char_span();
    self.dcx.emit(LexError::new(kind, span));
    self.advance();
  }

  fn next_token(&mut self) -> LexResult<Token> {
    self.skip_whitespace();

    if self.is_eof() {
      let pos = self.offset;
      let span = Span::new(pos, pos, self.file_id);
      return Ok(Token::new(TokenType::Eof, span, String::new()));
    }

    match self.peek() {
      Some('/') => match self.peek_ahead(1) {
        Some('/') => {
          if self.peek_ahead(2) == Some('/') {
            Ok(self.lex_doc_comment())
          } else {
            self.skip_line_comment();
            self.next_token()
          }
        }
        Some('*') => {
          self.skip_block_comment()?;
          self.next_token()
        }
        _ => self.lex_operator_or_punctuation(),
      },

      // String literal
      Some('"') => self.lex_string(),

      // Byte literal (b'...')
      Some('b') if self.peek_ahead(1) == Some('\'') => self.lex_byte(),

      // Character literal
      Some('\'') => self.lex_char(),

      // Numbers
      Some(ch) if ch.is_ascii_digit() => self.lex_number(),

      // Float starting with dot (.5)
      Some('.') if self.peek_ahead(1).map_or(false, |c| c.is_ascii_digit()) => {
        self.lex_decimal_or_float()
      }

      // Identifiers and keywords
      Some(ch) if ch == '_' || is_xid_start(ch) => self.lex_identifier(),

      // Operators and punctuation
      Some(_) => self.lex_operator_or_punctuation(),

      None => {
        let pos = self.offset;
        let span = Span::new(pos, pos, self.file_id);
        Ok(Token::new(TokenType::Eof, span, String::new()))
      }
    }
  }

  /// Tokenize an entire source into a vector of tokens
  pub fn tokenize(&mut self) -> Option<Vec<Token>> {
    let mut tokens = Vec::new();

    loop {
      let offset_before = self.offset;
      match self.next_token() {
        Ok(token) => {
          let is_eof = token.kind == TokenType::Eof;
          tokens.push(token);
          if is_eof {
            break;
          }
        }
        Err(err) => {
          self.dcx.emit(err);

          if self.is_eof() {
            let pos = self.offset;
            let span = Span::new(pos, pos, self.file_id);
            tokens.push(Token::new(TokenType::Eof, span, String::new()));
            break;
          }

          self.advance();

          if self.offset == offset_before {
            return None;
          }
        }
      }
    }

    Some(tokens)
  }
}
