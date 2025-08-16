mod attrs;
mod expr;
mod generics;
mod item;
mod stmt;
mod ty;

use crate::{
    AstId, Expr, ExprKind, LexedModule, ModuleId, Token, TokenKind,
    ast::{Ast, Keyword},
    get_tokens,
};
use id_arena::Arena;
use std::{ops::Range, path::PathBuf};
use zirael_utils::prelude::*;

pub type ParseResult<'report, T> = Result<T, ReportBuilder<'report>>;

pub struct ParserState {
    position: usize,
    error_count: usize,
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    position: usize,
    pub reports: Vec<ReportBuilder<'a>>,
    source: SourceFile,
    sync_tokens: Vec<TokenKind>,
    pub discover_queue: Vec<(PathBuf, Range<usize>)>,
    ast_id_arena: Arena<()>,
}

impl<'a> Parser<'a> {
    pub fn new(input: SourceFile) -> Self {
        let tokens = get_tokens(input.content());
        Self {
            tokens,
            position: 0,
            reports: Vec::new(),
            source: input,
            sync_tokens: vec![TokenKind::BraceClose],
            discover_queue: Vec::new(),
            ast_id_arena: Arena::new(),
        }
    }

    pub fn fresh_id(&mut self) -> AstId {
        self.ast_id_arena.alloc(())
    }

    pub fn new_expr(&mut self, kind: ExprKind, span: Span) -> Expr {
        Expr::new(kind, span, self.fresh_id())
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

    pub fn add_report(&mut self, error: ReportBuilder<'a>) {
        self.reports.push(error);
    }

    pub fn error_at(&mut self, message: impl Into<String>, span: Range<usize>) {
        let message = message.into();
        self.add_report(
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
        ParserState { position: self.position, error_count: self.reports.len() }
    }

    pub fn restore_state(&mut self, state: ParserState) {
        self.position = state.position;
        self.reports.truncate(state.error_count);
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
                self.add_report(error);
                None
            }
            None => {
                let span = self.eof_span();
                self.add_report(
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
            Some(token) if expected.contains(&token.kind) => Some(self.advance().unwrap()),
            Some(token) => {
                self.add_report(
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
                self.add_report(
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

    pub fn expect_keyword(&mut self, expected: Keyword) -> bool {
        if let Some(token) = self.peek()
            && let TokenKind::Keyword(keyword) = &token.kind
            && *keyword == expected
        {
            self.advance();
            return true;
        }

        self.error_at_peek(format!("expected keyword '{}'", expected.as_str()));
        false
    }

    pub fn expect_identifier(&mut self) -> Option<Identifier> {
        if let Some(token) = self.peek()
            && let TokenKind::Identifier(ident) = &token.kind
        {
            if Keyword::is_valid(ident) {
                self.error_at_peek(format!("expected an identifier, found keyword {ident:?}"));
                return None;
            }

            let ident = ident.clone();
            self.advance();
            return Some(get_or_intern(&ident));
        }

        self.error_at_peek(format!("expected an identifier, found {:?}", self.peek()));
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

        self.error_at_current("expected a string literal");
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
