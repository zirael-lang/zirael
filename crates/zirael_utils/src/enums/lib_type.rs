use clap::ValueEnum;
use std::fmt::Display;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum LibType {
  Static,
  Dynamic,
}

impl Display for LibType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{:?}",
      match self {
        Self::Static => "static",
        Self::Dynamic => "dynamic",
      }
    )
  }
}
