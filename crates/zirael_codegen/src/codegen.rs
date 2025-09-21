use std::fmt::Write;

/// A code generation helper that manages indentation and provides convenient
/// methods for writing formatted code.
#[derive(Debug, Clone)]
pub struct Codegen {
  buffer: String,
  indent_level: usize,
  indent_str: String,
}

impl Codegen {
  pub fn new() -> Self {
    Self::new_with_indent("    ")
  }

  pub fn new_with_indent(indent_str: &str) -> Self {
    Self { buffer: String::new(), indent_level: 0, indent_str: indent_str.to_string() }
  }

  pub fn with_tabs() -> Self {
    Self::new_with_indent("\t")
  }

  pub fn write(&mut self, text: &str) -> &mut Self {
    self.buffer.push_str(text);
    self
  }

  pub fn write_indented(&mut self, text: &str) -> &mut Self {
    self.add_current_indent();
    self.buffer.push_str(text);
    self
  }

  pub fn writeln(&mut self, text: &str) -> &mut Self {
    self.buffer.push_str(text);
    self.buffer.push('\n');
    self
  }

  pub fn writeln_indented(&mut self, text: &str) -> &mut Self {
    self.add_current_indent();
    self.buffer.push_str(text);
    self.buffer.push('\n');
    self
  }

  pub fn line(&mut self, text: &str) -> &mut Self {
    self.writeln_indented(text)
  }

  pub fn empty_line(&mut self) -> &mut Self {
    self.buffer.push('\n');
    self
  }

  pub fn empty_lines(&mut self, count: usize) -> &mut Self {
    for _ in 0..count {
      self.buffer.push('\n');
    }
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

  pub fn set_indent(&mut self, level: usize) -> &mut Self {
    self.indent_level = level;
    self
  }

  pub fn get_indent(&self) -> usize {
    self.indent_level
  }

  pub fn with_indent<F>(&mut self, f: F) -> &mut Self
  where
    F: FnOnce(&mut Self),
  {
    let old_level = self.indent_level;
    self.indent_level += 1;
    f(self);
    self.indent_level = old_level;
    self
  }

  pub fn block<F>(&mut self, opening: &str, closing: &str, f: F) -> &mut Self
  where
    F: FnOnce(&mut Self),
  {
    self.line(opening);
    self.with_indent(f);
    self.line(closing)
  }

  pub fn brace_block<F>(&mut self, header: &str, f: F) -> &mut Self
  where
    F: FnOnce(&mut Self),
  {
    self.line(&format!("{} {{", header));
    self.with_indent(f);
    self.line("}")
  }

  pub fn writef(&mut self, args: std::fmt::Arguments) -> &mut Self {
    write!(self.buffer, "{}", args).expect("Writing to string should not fail");
    self
  }

  pub fn writef_indented(&mut self, args: std::fmt::Arguments) -> &mut Self {
    self.add_current_indent();
    write!(self.buffer, "{}", args).expect("Writing to string should not fail");
    self
  }

  pub fn linef(&mut self, args: std::fmt::Arguments) -> &mut Self {
    self.add_current_indent();
    write!(self.buffer, "{}", args).expect("Writing to string should not fail");
    self.buffer.push('\n');
    self
  }

  pub fn comment(&mut self, text: &str) -> &mut Self {
    self.line(&format!("// {}", text))
  }

  pub fn comment_block(&mut self, lines: &[&str]) -> &mut Self {
    self.line("/*");
    for line in lines {
      self.line(&format!(" * {}", line));
    }
    self.line(" */")
  }

  pub fn to_string(&self) -> String {
    self.buffer.clone()
  }

  pub fn as_str(&self) -> &str {
    &self.buffer
  }

  pub fn clear(&mut self) -> &mut Self {
    self.buffer.clear();
    self.indent_level = 0;
    self
  }

  pub fn len(&self) -> usize {
    self.buffer.len()
  }

  pub fn is_empty(&self) -> bool {
    self.buffer.is_empty()
  }

  fn add_current_indent(&mut self) {
    for _ in 0..self.indent_level {
      self.buffer.push_str(&self.indent_str);
    }
  }
}

impl Default for Codegen {
  fn default() -> Self {
    Self::new()
  }
}

impl std::fmt::Display for Codegen {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.buffer)
  }
}

#[macro_export]
macro_rules! cwrite {
    ($codegen:expr, $($arg:tt)*) => {
        $codegen.writef(format_args!($($arg)*))
    };
}

#[macro_export]
macro_rules! cline {
    ($codegen:expr, $($arg:tt)*) => {
        $codegen.linef(format_args!($($arg)*))
    };
}
