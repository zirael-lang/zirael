use crate::parser::Parser;
use crate::{NodeId, Path, PathRoot, TokenType};

impl<'dcx> Parser<'dcx> {
  pub fn parse_path(&mut self) -> Option<Path> {
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
    segments.push(self.parse_identifier());

    while self.eat(TokenType::ColonColon) {
      // Check if this is the start of turbofish or import items
      if self.check(&TokenType::Lt) || self.check(&TokenType::LeftBrace) {
        break;
      }
      segments.push(self.parse_identifier());
    }

    Some(Path {
      id: NodeId::new(),
      root,
      segments,
      span: self.span_from(start),
    })
  }
}
