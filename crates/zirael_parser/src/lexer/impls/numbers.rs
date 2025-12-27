use crate::lexer::lexer::Lexer;
use crate::lexer::lexer_errors::{LexError, LexErrorKind, LexResult};
use crate::lexer::tokens::{IntBase, Token, TokenType};

impl Lexer {
    pub(crate) fn lex_number(&mut self) -> LexResult<Token> {
        if self.peek() == Some('0') {
            match self.peek_ahead(1) {
                Some('b') | Some('B') => return self.lex_binary(),
                Some('o') | Some('O') => return self.lex_octal(),
                Some('x') | Some('X') => return self.lex_hexadecimal(),
                _ => {} // Continue as decimal or float
            }
        }

        self.lex_decimal_or_float()
    }

    fn lex_binary(&mut self) -> LexResult<Token> {
        let start_offset = self.offset;
        let mut lexeme = String::new();

        // Consume '0b' or '0B'
        lexeme.push(self.advance().unwrap());
        lexeme.push(self.advance().unwrap());

        let mut digits = String::new();

        while let Some(ch) = self.peek() {
            match ch {
                '0' | '1' => {
                    digits.push(ch);
                    lexeme.push(ch);
                    self.advance();
                }
                _ if ch.is_ascii_alphanumeric() => {
                    let span = self.make_char_span();
                    return Err(LexError::new(
                        LexErrorKind::InvalidDigitForBase {
                            digit: ch,
                            base: "binary".to_string(),
                        },
                        span,
                    ));
                }
                _ => break,
            }
        }

        if digits.is_empty() {
            let span = self.make_span(start_offset);
            return Err(LexError::new(
                LexErrorKind::MissingDigitsAfterBase {
                    base: "0b".to_string(),
                },
                span,
            ));
        }

        let span = self.make_span(start_offset);

        Ok(Token::new(
            TokenType::IntegerLiteral(IntBase::Binary(digits)),
            span,
            lexeme,
        ))
    }

    /// Lex octal integer (0o...)
    fn lex_octal(&mut self) -> LexResult<Token> {
        let start_offset = self.offset;
        let mut lexeme = String::new();

        // Consume '0o' or '0O'
        lexeme.push(self.advance().unwrap());
        lexeme.push(self.advance().unwrap());

        let mut digits = String::new();

        // Read octal digits
        while let Some(ch) = self.peek() {
            match ch {
                '0'..='7' => {
                    digits.push(ch);
                    lexeme.push(ch);
                    self.advance();
                }
                _ if ch.is_ascii_alphanumeric() => {
                    let span = self.make_char_span();
                    return Err(LexError::new(
                        LexErrorKind::InvalidDigitForBase {
                            digit: ch,
                            base: "octal".to_string(),
                        },
                        span,
                    ));
                }
                _ => break,
            }
        }

        if digits.is_empty() {
            let span = self.make_span(start_offset);
            return Err(LexError::new(
                LexErrorKind::MissingDigitsAfterBase {
                    base: "0o".to_string(),
                },
                span,
            ));
        }

        let span = self.make_span(start_offset);

        Ok(Token::new(
            TokenType::IntegerLiteral(IntBase::Octal(digits)),
            span,
            lexeme,
        ))
    }

    /// Lex hexadecimal integer (0x...)
    fn lex_hexadecimal(&mut self) -> LexResult<Token> {
        let start_offset = self.offset;
        let mut lexeme = String::new();

        // Consume '0x' or '0X'
        lexeme.push(self.advance().unwrap());
        lexeme.push(self.advance().unwrap());

        let mut digits = String::new();

        while let Some(ch) = self.peek() {
            if ch.is_ascii_hexdigit() {
                digits.push(ch);
                lexeme.push(ch);
                self.advance();
            } else if ch.is_ascii_alphanumeric() {
                let span = self.make_char_span();
                return Err(LexError::new(
                    LexErrorKind::InvalidDigitForBase {
                        digit: ch,
                        base: "hexadecimal".to_string(),
                    },
                    span,
                ));
            } else {
                break;
            }
        }

        if digits.is_empty() {
            let span = self.make_span(start_offset);
            return Err(LexError::new(
                LexErrorKind::MissingDigitsAfterBase {
                    base: "0x".to_string(),
                },
                span,
            ));
        }

        let span = self.make_span(start_offset);

        Ok(Token::new(
            TokenType::IntegerLiteral(IntBase::Hexadecimal(digits)),
            span,
            lexeme,
        ))
    }

    pub(crate) fn lex_decimal_or_float(&mut self) -> LexResult<Token> {
        let start_offset = self.offset;
        let mut lexeme = String::new();
        let mut has_dot = false;
        let mut has_exponent = false;

        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                lexeme.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if self.peek() == Some('.') && self.peek_ahead(1).map_or(false, |c| c.is_ascii_digit()) {
            has_dot = true;
            lexeme.push('.');
            self.advance();

            // Read the fractional part
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    lexeme.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if matches!(self.peek(), Some('e') | Some('E')) {
            has_exponent = true;
            lexeme.push(self.advance().unwrap());

            if matches!(self.peek(), Some('+') | Some('-')) {
                lexeme.push(self.advance().unwrap());
            }

            let exp_start = lexeme.len();
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    lexeme.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }

            if lexeme.len() == exp_start {
                let span = self.make_span(start_offset);
                return Err(LexError::new(
                    LexErrorKind::MalformedExponent,
                    span,
                ));
            }
        }

        let span = self.make_span(start_offset);

        let token_type = if has_dot || has_exponent {
            TokenType::FloatLiteral(lexeme.clone())
        } else {
            TokenType::IntegerLiteral(IntBase::Decimal(lexeme.clone()))
        };

        Ok(Token::new(token_type, span, lexeme))
    }
}