use core::fmt;

/// A stable 4-digit diagnostic code in the range 0001..9999.
///
/// Formatting is always zero-padded to 4 digits.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DiagnosticCode(u16);

impl DiagnosticCode {
  pub const fn new(code: u16) -> Self {
    assert!(code != 0, "diagnostic code 0000 is reserved");
    assert!(code <= 9999, "diagnostic code must be <= 9999");
    Self(code)
  }

  pub const fn get(self) -> u16 {
    self.0
  }
}

impl fmt::Display for DiagnosticCode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:04}", self.0)
  }
}
