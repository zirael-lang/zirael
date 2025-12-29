use crate::ast::{
    ImportName, ImportSpec, ImportTail, Path, PathRoot, NodeId,
};
use crate::import::ImportDecl;
use crate::lexer::TokenType;
use crate::parser::{ParseResult, Parser};

impl Parser {
    /// Parse import declaration: `import path;` or `import path::{items};`
    pub fn parse_import(&mut self) -> ParseResult<ImportDecl> {
        let start = self.expect(TokenType::Import, "at start of import")?.span;

        let path = self.parse_path()?;

        let tail = if self.eat(TokenType::ColonColon) {
            self.expect(TokenType::LeftBrace, "after :: in import")?;

            if self.eat(TokenType::Star) {
                self.expect(TokenType::RightBrace, "after * in import")?;
                Some(ImportTail::Wildcard)
            } else {
                let items = self.parse_import_items()?;
                self.expect(TokenType::RightBrace, "after import items")?;
                Some(ImportTail::Items(items))
            }
        } else {
            None
        };

        self.expect(TokenType::Semicolon, "after import declaration")?;

        Ok(ImportDecl {
            id: NodeId::new(),
            path,
            tail,
            span: self.span_from(start),
        })
    }

    fn parse_import_items(&mut self) -> ParseResult<Vec<ImportSpec>> {
        let mut items = Vec::new();

        if !self.check(&TokenType::RightBrace) {
            loop {
                items.push(self.parse_import_spec()?);

                if !self.eat(TokenType::Comma) {
                    break;
                }

                if self.check(&TokenType::RightBrace) {
                    break;
                }
            }
        }

        Ok(items)
    }

    fn parse_import_spec(&mut self) -> ParseResult<ImportSpec> {
        let start = self.current_span();

        let name = if self.eat(TokenType::SelfValue) {
            ImportName::SelfValue
        } else {
            ImportName::Ident(self.parse_identifier()?)
        };

        let alias = if self.eat(TokenType::As) {
            Some(self.parse_identifier()?)
        } else {
            None
        };

        Ok(ImportSpec {
            id: NodeId::new(),
            name,
            alias,
            span: self.span_from(start),
        })
    }

    /// Parse a path like `package::module::item` or `self::item` or `super::item`
    pub fn parse_path(&mut self) -> ParseResult<Path> {
        let start = self.current_span();

        let root = if self.eat(TokenType::Package) {
            self.expect(TokenType::ColonColon, "after package")?;
            Some(PathRoot::Package)
        } else if self.eat(TokenType::SelfValue) {
            self.expect(TokenType::ColonColon, "after self")?;
            Some(PathRoot::SelfMod)
        } else if self.eat(TokenType::Super) {
            self.expect(TokenType::ColonColon, "after super")?;
            Some(PathRoot::Super)
        } else {
            None
        };

        let mut segments = Vec::new();
        segments.push(self.parse_identifier()?);

        while self.eat(TokenType::ColonColon) {
            // Check if this is the start of turbofish or import items
            if self.check(&TokenType::Lt) || self.check(&TokenType::LeftBrace) {
                break;
            }
            segments.push(self.parse_identifier()?);
        }

        Ok(Path {
            id: NodeId::new(),
            root,
            segments,
            span: self.span_from(start),
        })
    }
}
