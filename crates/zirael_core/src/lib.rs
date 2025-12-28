mod check;
mod unit;

pub mod vars {
  pub const FILE_EXTENSION: &str = "zr";
}

pub mod prelude {
  pub use crate::{check::*, unit::*, vars::*};
  pub use zirael_parser::*;
  pub use zirael_utils::context::*;
  pub use zirael_utils::{prelude::*, *};
}
