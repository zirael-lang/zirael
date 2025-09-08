pub mod ast;
mod discover;
mod lexer;
mod module;
mod parser;
pub mod symbols;
mod walker;

pub use ast::*;
pub use discover::*;
pub use lexer::*;
pub use module::*;
pub use symbols::*;
pub use walker::*;
pub use zirael_utils::dependency::*;

#[derive(Debug)]
pub enum MainFunction {
  Symbol(SymbolId),
  Mangled(String),
}
