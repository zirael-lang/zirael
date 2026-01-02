use std::fmt::{Debug, Formatter};
use std::io::Write;

pub struct Writer<T: Write>(T);

impl<T: Write> Write for Writer<T> {
  fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
    self.0.write(buf)
  }

  fn flush(&mut self) -> std::io::Result<()> {
    self.0.flush()
  }
}

impl<T: Write> Debug for Writer<T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "Writer")
  }
}
