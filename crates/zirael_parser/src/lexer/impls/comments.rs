use crate::lexer::lexer::Lexer;
use crate::lexer::lexer_errors::{LexError, LexErrorKind};
use crate::lexer::tokens::{Token, TokenType};

impl Lexer {
    pub(crate) fn skip_line_comment(&mut self) {
        self.advance();
        self.advance();

        while let Some(ch) = self.peek() {
            if ch == '\n' || ch == '\r' {
                break;
            }
            self.advance();
        }
    }

    pub(crate) fn skip_block_comment(&mut self) -> Result<(), LexError> {
        let start_offset = self.offset;

        self.advance();
        self.advance();

        loop {
            match self.peek() {
                None => {
                    let span = self.make_span(start_offset);
                    return Err(LexError::new(LexErrorKind::UnterminatedBlockComment, span));
                }
                Some('*') if self.peek_ahead(1) == Some('/') => {
                    self.advance();
                    self.advance();
                    return Ok(());
                }
                Some(_) => {
                    self.advance();
                }
            }
        }
    }

    pub(crate) fn lex_doc_comment(&mut self) -> Token {
        let start_offset = self.offset;

        self.advance();
        self.advance();
        self.advance();

        if self.peek() == Some(' ') {
            self.advance();
        }

        let mut content = String::new();
        while let Some(ch) = self.peek() {
            if ch == '\n' || ch == '\r' {
                break;
            }
            content.push(ch);
            self.advance();
        }

        let span = self.make_span(start_offset);

        Token::new(
            TokenType::DocComment(content.clone()),
            span,
            format!("///{}", content),
        )
    }
}