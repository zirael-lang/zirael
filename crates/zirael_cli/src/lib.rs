mod clap_styles;
mod cli;
pub mod logger;

pub use clap_styles::CLAP_STYLING;
pub use cli::{Cli, CliLibType, CliMode, CliPackageType};

pub mod prelude {
  pub use crate::cli::*;
  pub use crate::logger::*;
}
