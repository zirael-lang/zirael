use crate::parser::Parser;
use crate::parser::errors::{
  ExpectedSuperOrIdentPath, GenericsFirstSegment, GenericsInSuper,
  SelfAndPackageRootOnly,
};
use crate::{NodeId, Path, PathRoot, PathSegment, TokenType};
use zirael_utils::prelude::Identifier;

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
      let identifier = self.parse_identifier();
      let generics = self.parse_generic_parameters();

      if let Some(generics) = generics {
        self.emit(GenericsFirstSegment {
          span: generics.span,
        })
      };
      segments.push(PathSegment {
        identifier,
        args: vec![],
      });

      None
    } else {
      None
    };

    while self.eat(TokenType::ColonColon) {
      if self.check(TokenType::Lt) || self.check(TokenType::LeftBrace) {
        break;
      }

      match self.peek().kind {
        TokenType::Super => {
          let identifier = Identifier::new("super", self.peek().span);
          let generics = self.parse_generic_parameters();

          if let Some(gens) = generics {
            self.emit(GenericsInSuper { span: gens.span });
          }

          segments.push(PathSegment {
            identifier,
            args: vec![],
          });
          self.advance();
        }
        TokenType::Package | TokenType::SelfValue => {
          self.emit(SelfAndPackageRootOnly {
            span: self.peek().span,
          });
          self.advance();
        }
        TokenType::Identifier(_) => {
          let identifier = self.parse_identifier();
          let generics = self.parse_generic_arguments();

          segments.push(PathSegment {
            identifier,
            args: generics,
          });
        }
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
