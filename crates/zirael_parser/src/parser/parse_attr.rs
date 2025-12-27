use crate::ast::{AttrArg, AttrPath, Attribute};
use crate::identifier::Ident;
use crate::lexer::TokenType;
use crate::parser::{ParseResult, Parser, ParserError, ParserErrorKind};

impl Parser {
    /// Parse attributes: `#[attr]` or `#[attr(args)]`
    pub fn parse_attributes(&mut self) -> ParseResult<Vec<Attribute>> {
        let mut attributes = Vec::new();

        while self.check(&TokenType::Hash) {
            match self.parse_attribute() {
                Ok(attr) => attributes.push(attr),
                Err(e) => {
                    self.report_error(e);
                    self.synchronize();
                }
            }
        }

        Ok(attributes)
    }

    fn parse_attribute(&mut self) -> ParseResult<Attribute> {
        let start = self.expect(TokenType::Hash, "before attribute")?.span;
        self.expect(TokenType::LeftBracket, "after #")?;

        let path = self.parse_attr_path()?;

        let args = if self.eat(TokenType::LeftParen) {
            Some(self.parse_attr_args()?)
        } else {
            None
        };

        self.expect(TokenType::RightBracket, "to close attribute")?;

        Ok(Attribute {
            path,
            args,
            span: self.span_from(start),
        })
    }

    fn parse_attr_path(&mut self) -> ParseResult<AttrPath> {
        let start = self.current_span();
        let mut segments = Vec::new();

        segments.push(self.parse_identifier()?);

        while self.eat(TokenType::ColonColon) {
            segments.push(self.parse_identifier()?);
        }

        Ok(AttrPath {
            segments,
            span: self.span_from(start),
        })
    }

    fn parse_attr_args(&mut self) -> ParseResult<Vec<AttrArg>> {
        let mut args = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                args.push(self.parse_attr_arg()?);

                if !self.eat(TokenType::Comma) {
                    break;
                }

                // Allow trailing comma
                if self.check(&TokenType::RightParen) {
                    break;
                }
            }
        }

        self.expect(TokenType::RightParen, "after attribute arguments")?;

        Ok(args)
    }

    fn parse_attr_arg(&mut self) -> ParseResult<AttrArg> {
        // Try named argument: name = value
        if let TokenType::Identifier(_) = &self.peek().token_type {
            if let Some(next) = self.peek_ahead(1) {
                if matches!(next.token_type, TokenType::Assign) {
                    let name = self.parse_identifier()?;
                    self.expect(TokenType::Assign, "in named attribute argument")?;
                    let value = self.parse_expr()?;
                    return Ok(AttrArg::Named { name, value });
                }
            }
        }

        // Positional argument
        let value = self.parse_expr()?;
        Ok(AttrArg::Positional(value))
    }

    pub fn parse_identifier(&mut self) -> ParseResult<Ident> {
        let token = self.advance();
        match &token.token_type {
            TokenType::Identifier(name) => Ok(Ident {
                name: name.clone(),
                span: token.span,
            }),
            TokenType::SelfValue => Err(ParserError {
                kind: ParserErrorKind::ExpectedIdentifier,
                span: token.span,
                message: "Expected identifier, found keyword `self`".to_string(),
            }),
            _ => Err(ParserError {
                kind: ParserErrorKind::ExpectedIdentifier,
                span: token.span,
                message: format!("Expected identifier, found {}", token.token_type),
            }),
        }
    }
}
