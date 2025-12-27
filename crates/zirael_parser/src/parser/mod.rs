mod parse_attr;
mod parse_expr;
mod parse_import;
mod parse_item;
mod parse_stmt;
mod parse_type;
mod parser;

pub use parser::{ParseResult, Parser, ParserError, ParserErrorKind};

use crate::ast::ProgramNode;
use crate::lexer::Token;

pub fn parse(tokens: Vec<Token>) -> ParseResult<ProgramNode> {
  let mut parser = Parser::new(tokens);
  parser.parse_program()
}
