use crate::{
    TokenKind,
    ast::{GenericArg, GenericParameter, TraitBound},
    parser::Parser,
};
use ariadne::ReportKind;
use zirael_utils::prelude::ReportBuilder;

impl<'a> Parser<'a> {
    pub fn parse_generics(&mut self) -> Vec<GenericParameter> {
        if !self.match_token(TokenKind::LessThan) {
            return vec![];
        }

        let mut generics = vec![];

        if self.match_token(TokenKind::GreaterThan) {
            return generics;
        }

        loop {
            match self.parse_generic_parameter() {
                Some(param) => generics.push(param),
                None => {
                    self.synchronize(&[TokenKind::Comma, TokenKind::GreaterThan]);
                }
            }

            if self.match_token(TokenKind::Comma) {
                if self.check(&TokenKind::GreaterThan) {
                    break;
                }
            } else if self.match_token(TokenKind::GreaterThan) {
                break;
            } else {
                self.error_at_current("Expected ',' or '>' in generic parameter list");

                self.synchronize(&[TokenKind::Comma, TokenKind::GreaterThan]);

                if self.match_token(TokenKind::GreaterThan) {
                    break;
                } else {
                    self.error_at_current("Unable to recover from generic parsing error");
                    break;
                }
            }
        }

        generics
    }

    fn parse_generic_parameter(&mut self) -> Option<GenericParameter> {
        let identifier = self.expect_identifier()?;

        let mut constraints = vec![];
        let mut default_type = None;

        if self.match_token(TokenKind::Colon) {
            loop {
                if let Some(constraint) = self.parse_trait_bound() {
                    constraints.push(constraint);
                } else {
                    self.error_at_current("Expected trait bound after ':'");
                    self.synchronize(&[
                        TokenKind::Plus,
                        TokenKind::Comma,
                        TokenKind::GreaterThan,
                        TokenKind::Equals,
                    ]);
                    break;
                }

                if self.match_token(TokenKind::Plus) {
                    if self.check_any(&[
                        TokenKind::Comma,
                        TokenKind::GreaterThan,
                        TokenKind::Equals,
                    ]) {
                        self.error_at_current("Expected trait bound after '+'");
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        // optional default
        if self.match_token(TokenKind::Equals) {
            match self.parse_type() {
                Some(ty) => default_type = Some(ty),
                None => {
                    self.error_at_current("Expected type after '=' in generic parameter default");
                }
            }
        }

        Some(GenericParameter { name: identifier, constraints, default_type })
    }

    fn parse_trait_bound(&mut self) -> Option<TraitBound> {
        let trait_name = self.expect_identifier()?;

        let mut generic_args = vec![];

        if self.match_token(TokenKind::LessThan) {
            if self.match_token(TokenKind::GreaterThan) {
                return Some(TraitBound { name: trait_name, generic_args });
            }

            loop {
                let is_named_arg = self.try_parse(|parser| {
                    let name_span = parser.peek_span();
                    let name = parser.expect_identifier().ok_or_else(|| {
                        ReportBuilder::builder(
                            "Expected identifier in named generic argument",
                            ReportKind::Error,
                        )
                            .label("here", name_span)
                    })?;

                    parser.expect(TokenKind::Equals);

                    let type_span = parser.peek_span();
                    let ty = parser.parse_type().ok_or_else(|| {
                        ReportBuilder::builder(
                            "Expected type after '=' in named generic argument",
                            ReportKind::Error,
                        )
                            .label("here", type_span)
                    })?;

                    Ok(GenericArg::Named { name, ty })
                });

                if let Some(named_arg) = is_named_arg {
                    generic_args.push(named_arg);
                } else {
                    if let Some(ty) = self.parse_type() {
                        generic_args.push(GenericArg::Type(ty));
                    } else {
                        self.error_at_current(
                            "Expected type or named argument in generic parameter list",
                        );
                        self.synchronize(&[TokenKind::Comma, TokenKind::GreaterThan]);
                        if self.check(&TokenKind::GreaterThan) {
                            break;
                        }
                        if !self.check(&TokenKind::Comma) {
                            break;
                        }
                    }
                }

                if self.match_token(TokenKind::Comma) {
                    if self.check(&TokenKind::GreaterThan) {
                        break;
                    }
                } else if self.match_token(TokenKind::GreaterThan) {
                    break;
                } else {
                    self.error_at_current("Expected ',' or '>' in trait generic arguments");

                    self.synchronize(&[TokenKind::Comma, TokenKind::GreaterThan]);
                    if self.match_token(TokenKind::Comma) {
                    } else if self.match_token(TokenKind::GreaterThan) {
                        break;
                    } else {
                        return None;
                    }
                }
            }
        }

        Some(TraitBound { name: trait_name, generic_args })
    }
}