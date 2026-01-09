use crate::identifier::Ident;
use crate::parser::Parser;
use crate::parser::errors::{ExpectedSuperOrIdentPath, SelfAndPackageRootOnly};
use crate::{NodeId, Path, PathRoot, TokenType};

impl Parser<'_> {
  pub fn parse_path(&mut self) -> Path {
    let start = self.current_span();
    let mut segments = Vec::new();

    let root = if self.eat(TokenType::Package) {
      Some(PathRoot::Package)
    } else if self.eat(TokenType::SelfValue) {
      Some(PathRoot::SelfMod)
    } else if self.eat(TokenType::Super) {
      Some(PathRoot::Super)
    } else if self.is_identifier() {
      segments.push(self.parse_identifier());
      None
    } else {
      None
    };

    while self.eat(TokenType::ColonColon) {
      if self.check(&TokenType::Lt) || self.check(&TokenType::LeftBrace) {
        break;
      }

      match self.peek().kind {
        TokenType::Super => {
          segments.push(Ident::new("super", self.peek().span));
          self.advance();
        }
        TokenType::Package | TokenType::SelfValue => {
          self.emit(SelfAndPackageRootOnly {
            span: self.peek().span,
          });
          self.advance();
        }
        TokenType::Identifier(_) => segments.push(self.parse_identifier()),
        _ => {
          self.emit(ExpectedSuperOrIdentPath {
            span: self.peek().span,
            found: self.peek().kind.clone(),
          });
          self.advance();
        }
      }
    }

    Path {
      id: NodeId::new(),
      root,
      segments,
      span: self.span_from(start),
    }
  }
}
