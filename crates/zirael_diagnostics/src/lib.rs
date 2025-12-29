// The HumanReadableEmitter is actually modified version of ariadne's diagnostic emitter.

mod code;
pub mod codes;
mod ctx;
mod diagnostics;
mod emitter;
mod fmt;
mod output_type;
mod show;

pub use code::*;
pub use ctx::*;
pub use diagnostics::*;

pub mod prelude {
  pub use crate::code::*;
  pub use crate::codes::*;
  pub use crate::diagnostics::*;
  pub use crate::output_type::*;
  pub use anyhow::Result;
}
