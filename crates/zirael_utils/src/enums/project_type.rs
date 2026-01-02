use serde::{Deserialize, Serialize};
use std::fmt::Display;
use std::str::FromStr;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PackageType {
  #[serde(alias = "bin")]
  Binary,
  #[serde(alias = "lib")]
  Library,
}

impl Display for PackageType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Binary => "binary",
        Self::Library => "library",
      }
    )
  }
}

impl FromStr for PackageType {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s.to_ascii_lowercase().as_str() {
      "binary" | "bin" => Ok(Self::Binary),
      "library" | "lib" => Ok(Self::Library),
      _ => Err(()),
    }
  }
}
