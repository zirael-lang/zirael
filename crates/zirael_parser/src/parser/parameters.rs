use crate::parser::Parser;
use crate::parser::errors::{ExpectedIdentOrDots, VariadicNoDefault};
use crate::{
  NodeId, Param, RegularParam, SelfKind, SelfParam, TokenType, Type,
  VariadicParam,
};

impl Parser<'_> {
  pub fn parse_function_parameters(&mut self) -> Vec<Param> {
    let mut params = vec![];
    if self.peek().kind == TokenType::RightParen {
      return params;
    }

    loop {
      match self.peek().kind {
        TokenType::Identifier(_) => {
          let span = self.peek().span;

          let name = self.parse_identifier();
          let colon = self.expect(
            TokenType::Colon,
            "after the identifier (types are required for parameters)",
          );
          let ty = if colon.is_some() {
            self.parse_type()
          } else {
            Type::Invalid
          };

          let default = if self.check(TokenType::Assign) {
            self.advance();
            Some(self.parse_const_expr())
          } else {
            None
          };

          params.push(Param::Regular(RegularParam {
            id: NodeId::new(),
            default,
            ty,
            name,
            span: self.span_from(span),
          }));
        }
        TokenType::SelfValue => {
          self.advance();
          params.push(Param::SelfParam(SelfParam {
            id: NodeId::new(),
            span: self.previous().span,
            kind: SelfKind::Value,
          }));
        }
        TokenType::Mut => {
          let span = self.peek().span;
          self.advance();

          if self.peek().kind == TokenType::SelfValue {
            self.advance();
            params.push(Param::SelfParam(SelfParam {
              id: NodeId::new(),
              span: self.span_from(span),
              kind: SelfKind::Mut,
            }));
          } else {
            self.expect(
              TokenType::SelfValue,
              "after mut in a function parameter",
            );
            self
              .advance_until_one_of(&[TokenType::Comma, TokenType::RightParen]);
          }
        }
        TokenType::Star => {
          let span = self.peek().span;
          self.advance();
          let kind = match &self.peek().kind {
            TokenType::Mut => {
              self.advance();
              self.expect(TokenType::SelfValue, "after mut");

              SelfKind::PtrMut
            }
            TokenType::Const => {
              self.advance();
              self.expect(TokenType::SelfValue, "after const");

              SelfKind::Ptr
            }
            TokenType::SelfValue => {
              self.advance();
              SelfKind::Ptr
            }
            _ => todo!(),
          };

          params.push(Param::SelfParam(SelfParam {
            kind,
            id: NodeId::new(),
            span: self.span_from(span),
          }));
        }
        TokenType::DotDotDot => {
          self.advance();
          let span = self.previous().span;
          let name = self.parse_identifier();
          self.expect(
            TokenType::Colon,
            "after the identifier. types are required for parameters.",
          );
          let ty = self.parse_type();

          if self.check(TokenType::Assign) {
            self.advance();
            self.emit(VariadicNoDefault {
              span: self.previous().span,
            });

            let _ = self.parse_expr();
          }

          params.push(Param::Variadic(VariadicParam {
            id: NodeId::new(),
            name,
            ty,
            span: self.span_from(span),
          }));
        }
        _ => {
          self.emit(ExpectedIdentOrDots {
            span: self.peek().span,
            found: self.peek().kind.clone(),
          });
          self.advance_until_one_of(&[TokenType::Comma, TokenType::RightParen]);
        }
      }

      if self.check(TokenType::Comma) {
        self.advance();
        if self.check(TokenType::RightParen) {
          break;
        }
      } else if self.check(TokenType::RightParen) {
        break;
      }
    }

    params
  }
}
