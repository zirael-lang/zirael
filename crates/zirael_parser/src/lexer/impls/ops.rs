use crate::lexer::lexer::Lexer;
use crate::lexer::lexer_errors::{LexError, LexErrorKind, LexResult};
use crate::lexer::tokens::{Token, TokenType};
use crate::lexer::nfc::is_xid_continue;

impl Lexer {
    pub(crate) fn lex_operator_or_punctuation(&mut self) -> LexResult<Token> {
        let start_offset = self.offset;

        let token_type = match self.peek() {
            Some('+') => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenType::PlusAssign
                } else {
                    TokenType::Plus
                }
            }
            Some('-') => {
                self.advance();
                match self.peek() {
                    Some('=') => {
                        self.advance();
                        TokenType::MinusAssign
                    }
                    Some('>') => {
                        self.advance();
                        TokenType::Arrow
                    }
                    _ => TokenType::Minus,
                }
            }
            Some('*') => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenType::StarAssign
                } else {
                    TokenType::Star
                }
            }
            Some('/') => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenType::SlashAssign
                } else {
                    TokenType::Slash
                }
            }
            Some('%') => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenType::PercentAssign
                } else {
                    TokenType::Percent
                }
            }
            Some('=') => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenType::EqEq
                } else {
                    TokenType::Assign
                }
            }
            Some('!') => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenType::NotEq
                } else {
                    TokenType::Not
                }
            }
            Some('<') => {
                self.advance();
                match self.peek() {
                    Some('<') => {
                        self.advance();
                        if self.peek() == Some('=') {
                            self.advance();
                            TokenType::ShlAssign
                        } else {
                            TokenType::Shl
                        }
                    }
                    Some('=') => {
                        self.advance();
                        TokenType::LtEq
                    }
                    _ => TokenType::Lt,
                }
            }
            Some('>') => {
                self.advance();
                match self.peek() {
                    Some('>') => {
                        self.advance();
                        if self.peek() == Some('=') {
                            self.advance();
                            TokenType::ShrAssign
                        } else {
                            TokenType::Shr
                        }
                    }
                    Some('=') => {
                        self.advance();
                        TokenType::GtEq
                    }
                    _ => TokenType::Gt,
                }
            }
            Some('&') => {
                self.advance();
                match self.peek() {
                    Some('&') => {
                        self.advance();
                        if self.peek() == Some('=') {
                            self.advance();
                            TokenType::AndAssign
                        } else {
                            TokenType::AndAnd
                        }
                    }
                    Some('=') => {
                        self.advance();
                        TokenType::AmpAssign
                    }
                    _ => TokenType::Amp,
                }
            }
            Some('|') => {
                self.advance();
                match self.peek() {
                    Some('|') => {
                        self.advance();
                        if self.peek() == Some('=') {
                            self.advance();
                            TokenType::OrAssign
                        } else {
                            TokenType::OrOr
                        }
                    }
                    Some('=') => {
                        self.advance();
                        TokenType::PipeAssign
                    }
                    _ => TokenType::Pipe,
                }
            }
            Some('^') => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenType::CaretAssign
                } else {
                    TokenType::Caret
                }
            }
            Some('~') => {
                self.advance();
                TokenType::Tilde
            }
            Some(':') => {
                self.advance();
                if self.peek() == Some(':') {
                    self.advance();
                    TokenType::ColonColon
                } else {
                    TokenType::Colon
                }
            }
            Some('?') => {
                self.advance();
                TokenType::Question
            }
            Some('(') => {
                self.advance();
                TokenType::LeftParen
            }
            Some(')') => {
                self.advance();
                TokenType::RightParen
            }
            Some('{') => {
                self.advance();
                TokenType::LeftBrace
            }
            Some('}') => {
                self.advance();
                TokenType::RightBrace
            }
            Some('[') => {
                self.advance();
                TokenType::LeftBracket
            }
            Some(']') => {
                self.advance();
                TokenType::RightBracket
            }
            Some(',') => {
                self.advance();
                TokenType::Comma
            }
            Some(';') => {
                self.advance();
                TokenType::Semicolon
            }
            Some('.') => {
                self.advance();
                // Check for ... (ellipsis)
                if self.peek() == Some('.') && self.peek_ahead(1) == Some('.') {
                    self.advance();
                    self.advance();
                    TokenType::DotDotDot
                } else {
                    TokenType::Dot
                }
            }
            Some('#') => {
                self.advance();
                TokenType::Hash
            }
            Some('@') => {
                self.advance();
                TokenType::At
            }
            Some('_') => {
                self.advance();
                // Check if this is just a standalone underscore (not an identifier)
                if self.peek().map_or(true, |ch| !is_xid_continue(ch)) {
                    TokenType::Underscore
                } else {
                    // It's an identifier starting with underscore
                    // Backtrack and let the identifier lexer handle it
                    self.offset = start_offset;
                    return self.lex_identifier();
                }
            }
            Some(ch) => {
                let span = self.make_char_span();
                return Err(LexError::new(
                    LexErrorKind::UnexpectedCharacter { char: ch },
                    span
                ));
            }
            None => {
                let span = self.make_char_span();
                return Err(LexError::new(
                    LexErrorKind::UnexpectedCharacter { char: '\0' },
                    span
                ));
            }
        };

        let span = self.make_span(start_offset);
        let lexeme = self.source[start_offset..self.offset].to_string();

        Ok(Token::new(token_type, span, lexeme))
    }
}
