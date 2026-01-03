use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum DiagnosticOutputType {
  HumanReadable,
  JSON,
}

impl Display for DiagnosticOutputType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{:?}",
      match self {
        Self::JSON => "json",
        Self::HumanReadable => "human readable",
      }
    )
  }
}
