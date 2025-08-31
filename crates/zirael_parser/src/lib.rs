pub mod ast;
mod dependency;
mod discover;
mod lexer;
mod module;
mod parser;
mod span;
pub mod symbols;
mod walker;

pub use ast::*;
pub use dependency::*;
pub use discover::*;
pub use lexer::*;
pub use module::*;
pub use symbols::*;
pub use walker::*;

#[derive(Debug)]
pub enum MainFunction {
  Symbol(SymbolId),
  Mangled(String),
}
