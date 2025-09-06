mod c_functions;
mod generator;
mod match_expr;
mod resolver;
mod writer;

pub use generator::*;

use anyhow::Result;
use std::{
  fs::{File, create_dir_all},
  io::{BufWriter, Write as _},
  path::PathBuf,
};

#[derive(Debug)]
pub struct Codegen {
  content: BufWriter<Vec<u8>>,
  indent_level: usize,
  indent_size: usize,
  pub top_level: bool,
}

impl Default for Codegen {
  fn default() -> Self {
    Self::new()
  }
}

impl Codegen {
  pub fn new() -> Self {
    Self { content: BufWriter::new(vec![]), indent_level: 0, indent_size: 2, top_level: true }
  }

  pub fn write_to_file(&self, path: PathBuf) -> Result<()> {
    if let Some(parent) = path.parent() {
      create_dir_all(parent)?;
    }

    let mut file = File::create(path)?;
    file.write_all(self.content.buffer())?;

    Ok(())
  }

  pub fn write(&mut self, text: &str) {
    write!(self.content, "{text}").unwrap();
  }

  pub fn writeln(&mut self, text: &str) {
    let indent = " ".repeat(self.indent_level * self.indent_size);
    writeln!(self.content, "{indent}{text}").unwrap();
  }

  pub fn write_indented(&mut self, text: &str) {
    let indent = " ".repeat(self.indent_level * self.indent_size);
    write!(self.content, "{indent}{text}").unwrap();
  }

  pub fn block<F>(&mut self, f: F) -> &mut Self
  where
    F: FnOnce(&mut Self),
  {
    self.writeln("{");
    self.indent();
    f(self);
    self.dedent();
    self.writeln("}");
    self
  }

  pub fn newline(&mut self) -> &mut Self {
    writeln!(self.content).unwrap();
    self
  }

  pub fn indent(&mut self) -> &mut Self {
    self.indent_level += 1;
    self
  }

  pub fn dedent(&mut self) -> &mut Self {
    if self.indent_level > 0 {
      self.indent_level -= 1;
    }
    self
  }
}

pub trait Gen {
  fn generate_header(&self, _cg: &mut Codegen) {}
  fn generate(&self, cg: &mut Codegen);
}
