mod arena;
mod article;
mod cli;
pub mod context;
pub mod dependency;
mod diagnostics;
mod enums;
pub mod ident_table;
mod path;
mod project_config;
mod session;
mod span;

pub mod prelude {
  pub use crate::cli::*;
  pub use crate::{article::*, ident_table::*, path::*, project_config::*, session::*, span::*};
  pub use anyhow::{Result, anyhow, bail};
  pub use ariadne::Color;
  pub use colored::Colorize;
  pub use fs_err as fs;
  pub use log::{debug, error, info, warn};
  pub use parking_lot::*;
  pub use rayon::prelude::*;

  pub use crate::arena::sources::*;
  pub use crate::diagnostics::*;
  pub use crate::enums::lib_type::*;
  pub use crate::enums::mode::*;
  pub use crate::enums::project_type::*;
  pub use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
  };
}
