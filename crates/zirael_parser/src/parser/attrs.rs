use crate::{ExpectedAttribute, Expr, ExprKind, GenericArg, Literal, TokenKind, ast::Attribute, parser::Parser, Attributes};
use zirael_utils::ident_table::get_or_intern;
use zirael_utils::prelude::Identifier;

impl<'a> Parser<'a> {
  pub fn parse_attrs(&mut self) -> Attributes {
    let mut attrs = Vec::new();

    while self.check(&TokenKind::At) {
      if let Some(attr) = self.parse_single_attr() {
        attrs.push(attr);
      }
    }

    Attributes::new(attrs)
  }

  fn get_builtin_attr(name: &Identifier) -> Option<BuiltinAttr> {
    for attr in BUILTIN_ATTRS {
      if &get_or_intern(attr.name, None) == name {
        return Some(attr.clone());
      }
    }
    None
  }

  fn parse_single_attr(&mut self) -> Option<Attribute> {
    self.advance();

    let name = self.parse_attr_name()?;
    let args = self.parse_attr_args();

    if let Some(attr) = Self::get_builtin_attr(&name) {
      if let Some(ref args) = args {
        if args.len() != attr.expected_arguments.len() {
          self.error_at_current(&format!(
            "attribute '{}' expects {} argument(s), but got {}",
            attr.name,
            attr.expected_arguments.len(),
            args.len()
          ));
        } else {
          self.validate_attr_args(&attr, args);
        }
      } else if !attr.expected_arguments.is_empty() {
        self.error_at_current(&format!(
          "attribute '{}' expects {} argument(s), but got none",
          attr.name,
          attr.expected_arguments.len()
        ));
      }
    }

    Some(Attribute { name, args })
  }

  fn validate_attr_args(&mut self, attr: &BuiltinAttr, args: &[Expr]) {
    for (i, expected) in attr.expected_arguments.iter().enumerate() {
      let arg = &args[i];
      match (expected, &arg.kind) {
        (ExpectedAttribute::String, ExprKind::Literal(Literal::String(_))) => {}
        (ExpectedAttribute::Int, ExprKind::Literal(Literal::Integer(_))) => {}
        (ExpectedAttribute::Float, ExprKind::Literal(Literal::Float(_))) => {}
        _ => {
          self.error_at_current(&format!(
            "attribute '{}' argument {} expects a {}, but got {}",
            attr.name,
            i + 1,
            expected,
            arg.kind.name()
          ));
        }
      }
    }
  }

  fn parse_attr_name(&mut self) -> Option<Identifier> {
    let token = self.advance()?;

    if let TokenKind::Identifier(name_str) = token.kind {
      Some(get_or_intern(&name_str, Some(token.span)))
    } else {
      self.error_at("expected attribute name after '@'", token.span);
      None
    }
  }

  fn parse_attr_args(&mut self) -> Option<Vec<crate::ast::Expr>> {
    if !self.check(&TokenKind::ParenOpen) {
      return None;
    }

    self.advance();

    let arguments = self.parse_argument_list();

    if !self.match_token(TokenKind::ParenClose) {
      self.error_at_current("expected ')' after attribute arguments");
    }

    Some(arguments)
  }

  fn parse_argument_list(&mut self) -> Vec<crate::ast::Expr> {
    let mut arguments = Vec::new();

    if self.check(&TokenKind::ParenClose) {
      return arguments;
    }

    loop {
      arguments.push(self.parse_expr());

      if !self.match_token(TokenKind::Comma) {
        break;
      }
    }

    arguments
  }
}

#[derive(Clone)]
pub struct BuiltinAttr {
  pub name: &'static str,
  pub expected_arguments: &'static [ExpectedAttribute],
}

pub const BUILTIN_ATTRS: &[BuiltinAttr] =
  &[BuiltinAttr { name: "mode", expected_arguments: &[ExpectedAttribute::String] }];
