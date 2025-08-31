use crate::{ast::Attribute, parser::Parser};

impl<'a> Parser<'a> {
  pub fn parse_attrs(&mut self) -> Vec<Attribute> {
    vec![]
  }
}
