mod attrs;
mod expr;
mod generics;
mod item;
mod stmt;
mod ty;

use crate::{LexedModule, Token, TokenKind, ast::{Ast, keyword::Keyword}, get_tokens, ModuleId};
use ariadne::{ReportKind, Span as _};
use std::{ops::Range, path::PathBuf};
use zirael_utils::prelude::{Identifier, ReportBuilder, SourceFile, SourceFileId, get_or_intern};

pub type ParseResult<'report, T> = Result<T, ReportBuilder<'report>>;

pub struct ParserState {
    position: usize,
    error_count: usize,
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    position: usize,
    pub errors: Vec<ReportBuilder<'a>>,
    source: SourceFile,
    sync_tokens: Vec<TokenKind>,
    pub discover_queue: Vec<(PathBuf, Range<usize>)>,
}

impl<'a> Parser<'a> {
    pub fn new(input: SourceFile) -> Self {
        let tokens = get_tokens(input.content());
        Self {
            tokens,
            position: 0,
            errors: Vec::new(),
            source: input,
            sync_tokens: vec![TokenKind::BraceClose],
            discover_queue: Vec::new(),
        }
    }

    pub fn with_sync_tokens(mut self, sync_tokens: Vec<TokenKind>) -> Self {
        self.sync_tokens = sync_tokens;
        self
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    pub fn peek_span(&self) -> Range<usize> {
        self.peek().map(|token| token.span.clone()).unwrap_or_default()
    }

    pub fn prev(&self) -> Option<&Token> {
        if self.position == 0 {
            return self.tokens.first();
        }

        self.tokens.get(self.position - 1)
    }

    pub fn prev_span(&self) -> Range<usize> {
        self.prev().map(|token| token.span.clone()).unwrap_or_default()
    }

    pub fn peek_ahead(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset)
    }

    pub fn peek_back(&self) -> Option<&Token> {
        if self.position > 0 { self.tokens.get(self.position - 1) } else { None }
    }

    pub fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn remaining(&self) -> usize {
        self.tokens.len().saturating_sub(self.position)
    }

    pub fn advance(&mut self) -> Option<Token> {
        if !self.is_at_end() {
            let token = self.tokens[self.position].clone();
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    pub fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(&kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn check_keyword(&self, keyword: Keyword) -> bool {
        if let Some(token) = self.peek()
            && let TokenKind::Keyword(k) = &token.kind
            && *k == keyword
        {
            true
        } else {
            false
        }
    }

    pub fn match_keyword(&mut self, keyword: Keyword) -> bool {
        if self.check_keyword(keyword) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn match_any(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        if self.check_any(kinds) { self.advance() } else { None }
    }

    pub fn expect(&mut self, expected: TokenKind) -> Option<Token> {
        self.expect_impl(expected, None)
    }

    pub fn expect_message(&mut self, expected: TokenKind, message: &str) -> Option<Token> {
        self.expect_impl(expected, Some(message))
    }

    pub fn add_error(&mut self, error: ReportBuilder<'a>) {
        self.errors.push(error);
    }

    pub fn error_at(&mut self, message: impl Into<String>, span: Range<usize>) {
        let message = message.into();
        self.add_error(
            ReportBuilder::builder(message.clone(), ReportKind::Error)
                .label(message.as_str(), span),
        );
    }

    pub fn error_at_current(&mut self, message: impl Into<String>) {
        self.error_at(message, self.prev_span());
    }

    pub fn error_at_peek(&mut self, message: impl Into<String>) {
        self.error_at(message, self.peek_span());
    }

    fn eof_span(&self) -> Range<usize> {
        let end = self.source.content().len();
        end..end
    }

    pub fn save_state(&self) -> ParserState {
        ParserState { position: self.position, error_count: self.errors.len() }
    }

    pub fn restore_state(&mut self, state: ParserState) {
        self.position = state.position;
        self.errors.truncate(state.error_count);
    }

    pub fn try_parse<T, F>(&mut self, parse_fn: F) -> Option<T>
    where
        F: FnOnce(&mut Self) -> ParseResult<T>,
    {
        let state = self.save_state();
        if let Ok(result) = parse_fn(self) {
            Some(result)
        } else {
            self.restore_state(state);
            None
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn clear_errors(&mut self) {
        self.errors.clear();
    }

    pub fn check(&self, kind: &TokenKind) -> bool {
        match self.peek() {
            Some(token) => &token.kind == kind,
            None => false,
        }
    }

    pub fn check_any(&self, kinds: &[TokenKind]) -> bool {
        match self.peek() {
            Some(token) => kinds.contains(&token.kind),
            None => false,
        }
    }

    fn expect_impl(&mut self, expected: TokenKind, expected_msg: Option<&str>) -> Option<Token> {
        match self.peek() {
            Some(token) if token.kind == expected => Some(self.advance().unwrap()),
            Some(token) => {
                let error = ReportBuilder::builder(
                    if let Some(msg) = expected_msg {
                        msg.to_owned()
                    } else {
                        format!("Expected {:?}, found {:?}", expected, token.kind)
                    },
                    ReportKind::Error,
                )
                .label("here", token.span.clone());
                self.add_error(error);
                None
            }
            None => {
                let span = self.eof_span();
                self.add_error(
                    ReportBuilder::builder(
                        format!("Expected {expected:?}, found end of file"),
                        ReportKind::Error,
                    )
                    .label("here", span),
                );
                None
            }
        }
    }

    pub fn expect_any(&mut self, expected: &[TokenKind]) -> Option<Token> {
        match self.peek() {
            // Changed from current() to peek()
            Some(token) if expected.contains(&token.kind) => Some(self.advance().unwrap()),
            Some(token) => {
                self.add_error(
                    ReportBuilder::builder(
                        format!("Expected one of {:?}, found {:?}", expected, token.kind),
                        ReportKind::Error,
                    )
                    .label("here", token.span.clone()),
                );
                None
            }
            None => {
                let span = self.eof_span();
                self.add_error(
                    ReportBuilder::builder(
                        format!("Expected one of {expected:?}, found end of file"),
                        ReportKind::Error,
                    )
                    .label("here", span),
                );
                None
            }
        }
    }

    pub fn synchronize(&mut self, tokens: &[TokenKind]) {
        let tokens = if tokens.is_empty() { self.sync_tokens.clone() } else { tokens.to_vec() };

        while !self.is_at_end() {
            if let Some(token) = self.peek() {
                if tokens.contains(&token.kind) {
                    break;
                }
            }
            self.advance();
        }
    }

    pub fn skip_while<F>(&mut self, mut condition: F) -> usize
    where
        F: FnMut(&TokenKind) -> bool,
    {
        let start_pos = self.position;

        while let Some(token) = self.peek() {
            if condition(&token.kind) {
                self.advance();
            } else {
                break;
            }
        }

        self.position - start_pos
    }

    pub fn skip_until<F>(&mut self, mut condition: F) -> usize
    where
        F: FnMut(&TokenKind) -> bool,
    {
        let start_pos = self.position;

        while let Some(token) = self.peek() {
            if condition(&token.kind) {
                break;
            } else {
                self.advance();
            }
        }

        self.position - start_pos
    }

    pub fn expect_keyword(&mut self, expected: Keyword) -> bool {
        if let Some(token) = self.peek()
            && let TokenKind::Keyword(keyword) = &token.kind
            && *keyword == expected
        {
            self.advance();
            return true;
        }

        self.error_at_peek(format!("Expected keyword '{}'", expected.as_str()));
        false
    }

    pub fn expect_identifier(&mut self) -> Option<Identifier> {
        if let Some(token) = self.peek()
            && let TokenKind::Identifier(ident) = &token.kind
        {
            if Keyword::is_valid(ident) {
                self.error_at_peek(format!("Expected an identifier, found keyword {ident:?}"));
                return None;
            }

            let ident = ident.clone();
            self.advance();
            return Some(get_or_intern(&ident));
        }

        self.error_at_peek(format!("Expected an identifier, found {:?}", self.peek()));
        None
    }

    pub fn expect_string(&mut self) -> Option<String> {
        if let Some(token) = self.peek() {
            if let TokenKind::String(string) = &token.kind {
                let result = string.clone();
                self.advance();
                return Some(result);
            }
        }

        self.error_at_current("Expected a string literal");
        None
    }

    pub fn match_triple_dot(&mut self) -> bool {
        let mut dots = 0;
        while let Some(token) = self.peek_ahead(dots) {
            if token.kind == TokenKind::Dot {
                dots += 1;
            } else {
                break;
            }
        }

        if dots == 3 {
            self.position += 3;
            true
        } else {
            false
        }
    }

    pub fn parse(&mut self, entrypoint: SourceFileId) -> LexedModule {
        let mut items = Vec::new();

        while !self.is_at_end() {
            if let Some(item) = self.parse_item() {
                items.push(item);
            }
        }

        LexedModule::new(ModuleId::File(entrypoint), Ast::new(items))
    }
}
