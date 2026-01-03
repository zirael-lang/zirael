mod errors;
mod parser;
mod path;
mod program;

pub use parser::Parser;

use crate::ast::ProgramNode;
use crate::lexer::Token;
use zirael_diagnostics::DiagnosticCtx;

pub fn parse(tokens: Vec<Token>, dcx: &DiagnosticCtx) -> Option<ProgramNode> {
  let mut parser = Parser::new(tokens, dcx);
  parser.parse_program()
}

#[macro_export]
macro_rules! log_parse_failure {
  ($value:expr, $item_type:expr) => {{
    let result = $value;
    if result.is_none() {
      debug!(
        "Failed to parse {} at {}:{}:{}",
        $item_type,
        file!(),
        line!(),
        column!()
      );
    }
    result
  }};
}
