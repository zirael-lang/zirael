use crate::{TokenKind, ast::Attribute, parser::Parser};
use zirael_utils::ident_table::get_or_intern;

impl<'a> Parser<'a> {
  pub fn parse_attrs(&mut self) -> Vec<Attribute> {
    let mut attrs = Vec::new();

    loop {
      if !self.check(&TokenKind::At) {
        break;
      }

      self.advance();

      let name = if let Some(token) = self.advance() {
        if let TokenKind::Identifier(name_str) = token.kind {
          get_or_intern(&name_str, Some(token.span))
        } else {
          self.error_at("expected attribute name after '@'", token.span);
          continue;
        }
      } else {
        self.error_at_peek("expected attribute name after '@'");
        break;
      };

      let args = if self.check(&TokenKind::ParenOpen) {
        self.advance();
        let mut arguments = Vec::new();

        if !self.check(&TokenKind::ParenClose) {
          loop {
            arguments.push(self.parse_expr());

            if self.match_token(TokenKind::Comma) {
              continue;
            }

            break;
          }
        }

        if !self.match_token(TokenKind::ParenClose) {
          self.error_at_current("expected ')' after attribute arguments");
        }

        Some(arguments)
      } else {
        None
      };

      attrs.push(Attribute { name, args });
    }

    attrs
  }
}
