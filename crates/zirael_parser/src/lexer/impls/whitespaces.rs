use crate::lexer::lexer::Lexer;
use crate::lexer::lexer_errors::LexErrorKind;

impl Lexer {
    pub(crate) fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            match ch {
                ' ' | '\t' | '\n' | '\r' | '\x0B' | '\x0C' => {
                    self.advance();
                }
                '\u{FEFF}' => {
                    // BOM after start is an error - skip and continue
                    self.skip_with_error(LexErrorKind::UnexpectedBom);
                }
                '\u{0000}' => {
                    // NUL is disallowed - skip and continue
                    self.skip_with_error(LexErrorKind::DisallowedCodePoint { code_point: ch });
                }
                ch if ch.is_whitespace() && !ch.is_ascii() => {
                    // Non-ASCII whitespace is an error - skip and continue
                    self.skip_with_error(LexErrorKind::NonAsciiWhitespace { code_point: ch });
                }
                _ => break,
            }
        }
    }

    pub(crate) fn is_ascii_whitespace(ch: char) -> bool {
        matches!(ch, ' ' | '\t' | '\n' | '\r' | '\x0B' | '\x0C')
    }
}