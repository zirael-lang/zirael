mod diagnostics;

pub use diagnostics::*;

pub mod prelude {
  pub use crate::diagnostics::*;
  pub use anyhow::Result;
  pub use ariadne::Color;
}
