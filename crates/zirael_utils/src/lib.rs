mod article;
pub mod context;
pub mod dependency;
pub mod enums;
pub mod ident_table;
mod path;
pub mod project_config;
mod session;
pub mod term_style;

pub mod prelude {
  pub use crate::{article::*, ident_table::*, path::*, project_config::*, session::*};
  pub use anyhow::{Result, anyhow, bail};
  pub use colored::Colorize;
  pub use fs_err as fs;
  pub use log::{debug, error, info, warn};
  pub use parking_lot::*;
  pub use rayon::prelude::*;

  pub use crate::term_style::*;

  pub use zirael_diagnostics::prelude::*;
  pub use crate::enums::lib_type::*;
  pub use crate::enums::mode::*;
  pub use crate::enums::project_type::*;
  pub use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
  };
  pub use zirael_source::prelude::*;
}
