use std::fmt::Display;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum Mode {
  #[default]
  Debug,
  Release,
}

impl Mode {
  pub fn from_str(mode: &str) -> Option<Self> {
    match mode.to_lowercase().as_str() {
      "debug" => Some(Self::Debug),
      "release" => Some(Self::Release),
      _ => None,
    }
  }
}

impl Display for Mode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Debug => "debug",
        Self::Release => "release",
      }
    )
  }
}
