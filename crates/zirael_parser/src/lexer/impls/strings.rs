use crate::lexer::lexer::Lexer;
use crate::lexer::lexer_errors::{LexError, LexErrorKind, LexResult};
use crate::lexer::tokens::{Token, TokenType};

impl Lexer<'_> {
  pub(crate) fn lex_string(&mut self) -> LexResult<Token> {
    let start_offset = self.offset;
    let mut value = String::new();
    let mut lexeme = String::from("\"");

    self.advance();

    loop {
      match self.peek() {
        None | Some('\n' | '\r') => {
          let span = self.make_span(start_offset);
          return Err(LexError::new(LexErrorKind::UnterminatedString, span));
        }
        Some('"') => {
          lexeme.push('"');
          self.advance();
          break;
        }
        Some('\\') => {
          lexeme.push('\\');
          self.advance();

          match self.parse_escape_sequence(true) {
            Ok(escaped) => {
              value.push_str(&escaped);
              lexeme.push_str(&escaped);
            }
            Err(err) => {
              self.dcx.emit(err);
              value.push('?');
            }
          }
        }
        Some(ch) => {
          value.push(ch);
          lexeme.push(ch);
          self.advance();
        }
      }
    }

    let span = self.make_span(start_offset);

    Ok(Token::new(TokenType::StringLiteral(value), span, lexeme))
  }

  fn parse_escape_sequence(
    &mut self,
    allow_unicode: bool,
  ) -> LexResult<String> {
    let start_offset = self.offset;

    match self.peek() {
      Some('\\') => {
        self.advance();
        Ok("\\".to_owned())
      }
      Some('"') => {
        self.advance();
        Ok("\"".to_owned())
      }
      Some('\'') => {
        self.advance();
        Ok("'".to_owned())
      }
      Some('n') => {
        self.advance();
        Ok("\n".to_owned())
      }
      Some('r') => {
        self.advance();
        Ok("\r".to_owned())
      }
      Some('t') => {
        self.advance();
        Ok("\t".to_owned())
      }
      Some('b') => {
        self.advance();
        Ok("\u{0008}".to_owned())
      }
      Some('f') => {
        self.advance();
        Ok("\u{000C}".to_owned())
      }
      Some('x') => {
        self.advance();
        let hex = self.read_hex_digits(2)?;
        let value = u32::from_str_radix(&hex, 16).unwrap();
        Ok(char::from_u32(value).unwrap().to_string())
      }
      Some('u') if allow_unicode => {
        self.advance();
        let hex = self.read_hex_digits(4)?;
        let value = u32::from_str_radix(&hex, 16).unwrap();
        if let Some(ch) = char::from_u32(value) { Ok(ch.to_string()) } else {
          let span = self.make_span(start_offset);
          Err(LexError::new(
            LexErrorKind::InvalidEscape {
              escape: format!("\\u{hex}"),
            },
            span,
          ))
        }
      }
      Some('U') if allow_unicode => {
        self.advance();
        let hex = self.read_hex_digits(8)?;
        let value = u32::from_str_radix(&hex, 16).unwrap();
        if let Some(ch) = char::from_u32(value) { Ok(ch.to_string()) } else {
          let span = self.make_span(start_offset);
          Err(LexError::new(
            LexErrorKind::InvalidEscape {
              escape: format!("\\U{hex}"),
            },
            span,
          ))
        }
      }
      Some(ch) => {
        let escape = format!("\\{ch}");
        let span = self.make_span(start_offset);
        Err(LexError::new(LexErrorKind::InvalidEscape { escape }, span))
      }
      None => {
        let span = self.make_span(start_offset);
        Err(LexError::new(
          LexErrorKind::InvalidEscape {
            escape: "\\".to_owned(),
          },
          span,
        ))
      }
    }
  }

  fn read_hex_digits(&mut self, n: usize) -> LexResult<String> {
    let start_offset = self.offset;
    let mut digits = String::new();

    for _ in 0..n {
      match self.peek() {
        Some(ch) if ch.is_ascii_hexdigit() => {
          digits.push(ch);
          self.advance();
        }
        _ => {
          let span = self.make_span(start_offset);
          return Err(LexError::new(
            LexErrorKind::InvalidEscape {
              escape: format!("\\x{digits}"),
            },
            span,
          ));
        }
      }
    }

    Ok(digits)
  }

  pub(crate) fn lex_char(&mut self) -> LexResult<Token> {
    let start_offset = self.offset;
    let mut lexeme = String::from("'");

    self.advance();

    let value = match self.peek() {
      None | Some('\n' | '\r') => {
        let span = self.make_span(start_offset);
        return Err(LexError::new(LexErrorKind::UnterminatedChar, span));
      }
      Some('\'') => {
        let span = self.make_span(start_offset);
        return Err(LexError::new(LexErrorKind::EmptyCharLiteral, span));
      }
      Some('\\') => {
        lexeme.push('\\');
        self.advance();
        match self.parse_escape_sequence(true) {
          Ok(escaped) => {
            lexeme.push_str(&escaped);
            escaped.chars().next().unwrap()
          }
          Err(err) => {
            self.dcx.emit(err);
            '?'
          }
        }
      }
      Some(ch) => {
        lexeme.push(ch);
        self.advance();
        ch
      }
    };

    if self.peek() == Some('\'') {
      lexeme.push('\'');
      self.advance();
    } else {
      let span = self.make_span(start_offset);
      return Err(LexError::new(LexErrorKind::UnterminatedChar, span));
    }

    let span = self.make_span(start_offset);

    Ok(Token::new(TokenType::CharLiteral(value), span, lexeme))
  }

  pub(crate) fn lex_byte(&mut self) -> LexResult<Token> {
    let start_offset = self.offset;
    let mut lexeme = String::from("b'");

    // Consume 'b' and opening quote
    self.advance();
    self.advance();

    let value = match self.peek() {
      None | Some('\n' | '\r') => {
        let span = self.make_span(start_offset);
        return Err(LexError::new(LexErrorKind::UnterminatedChar, span));
      }
      Some('\'') => {
        let span = self.make_span(start_offset);
        return Err(LexError::new(LexErrorKind::EmptyCharLiteral, span));
      }
      Some('\\') => {
        lexeme.push('\\');
        self.advance();
        match self.parse_escape_sequence(false) {
          // No \u or \U
          Ok(escaped) => {
            lexeme.push_str(&escaped);
            let ch = escaped.chars().next().unwrap();
            ch as u32
          }
          Err(err) => {
            self.dcx.emit(err);
            0
          }
        }
      }
      Some(ch) => {
        lexeme.push(ch);
        self.advance();
        ch as u32
      }
    };

    if value > 255 {
      let span = self.make_span(start_offset);
      return Err(LexError::new(
        LexErrorKind::InvalidByteValue { value },
        span,
      ));
    }

    if self.peek() == Some('\'') {
      lexeme.push('\'');
      self.advance();
    } else {
      let span = self.make_span(start_offset);
      return Err(LexError::new(LexErrorKind::UnterminatedChar, span));
    }

    let span = self.make_span(start_offset);

    Ok(Token::new(
      TokenType::ByteLiteral(value as u8),
      span,
      lexeme,
    ))
  }
}
