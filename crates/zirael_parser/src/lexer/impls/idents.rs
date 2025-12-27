use crate::lexer::lexer::Lexer;
use crate::lexer::lexer_errors::{LexError, LexErrorKind, LexResult};
use crate::lexer::nfc::{is_xid_continue, is_xid_start, normalize_nfc};
use crate::lexer::tokens::{Token, TokenType};

impl Lexer {
    pub(crate) fn lex_identifier(&mut self) -> LexResult<Token> {
        let start_offset = self.offset;
        let mut lexeme = String::new();

        // First character: _ or XID_Start
        match self.peek() {
            Some('_') => {
                lexeme.push('_');
                self.advance();
            }
            Some(ch) if is_xid_start(ch) => {
                lexeme.push(ch);
                self.advance();
            }
            Some(ch) => {
                let span = self.make_char_span();
                return Err(LexError::new(
                    LexErrorKind::InvalidIdentifierStart { char: ch },
                    span,
                ));
            }
            None => {
                let span = self.make_char_span();
                return Err(LexError::new(
                    LexErrorKind::UnexpectedCharacter { char: '\0' },
                    span,
                ));
            }
        }

        while let Some(ch) = self.peek() {
            if is_xid_continue(ch) {
                lexeme.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let normalized = normalize_nfc(&lexeme);

        let token_type = match normalized.as_str() {
            "true" => TokenType::True,
            "false" => TokenType::False,
            "func" => TokenType::Func,
            "struct" => TokenType::Struct,
            "enum" => TokenType::Enum,
            "interface" => TokenType::Interface,
            "impl" => TokenType::Impl,
            "type" => TokenType::Type,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "while" => TokenType::While,
            "for" => TokenType::For,
            "in" => TokenType::In,
            "return" => TokenType::Return,
            "loop" => TokenType::Loop,
            "match" => TokenType::Match,
            "import" => TokenType::Import,
            "as" => TokenType::As,
            "pub" => TokenType::Pub,
            "var" => TokenType::Var,
            "const" => TokenType::Const,
            "mut" => TokenType::Mut,
            "package" => TokenType::Package,
            "self" => TokenType::SelfValue,
            "super" => TokenType::Super,
            "await" => TokenType::Await,
            "async" => TokenType::Async,
            _ => TokenType::Identifier(normalized.clone()),
        };

        let span = self.make_span(start_offset);

        Ok(Token::new(token_type, span, lexeme))
    }
}
