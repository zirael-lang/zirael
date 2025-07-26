use crate::{ast::Attribute, parser::Parser};
use log::warn;

impl<'a> Parser<'a> {
    pub fn parse_attrs(&mut self) -> Vec<Attribute> {
        vec![]
    }
}
