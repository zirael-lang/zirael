mod check;
mod context;
mod unit;

pub mod vars {
  pub const FILE_EXTENSION: &str = "zr";
}

pub mod prelude {
  pub use crate::{check::*, context::*, unit::*, vars::*};
  pub use zirael_parser::*;
  pub use zirael_utils::{prelude::*, *};
}
