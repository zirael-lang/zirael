mod article;
mod clap;
mod cli;
mod comp_info;
pub mod dependency;
pub mod ident_table;
mod lib_type;
pub mod logger;
mod mode;
mod path;
mod project_config;
mod project_type;
mod reports;
pub mod sources;
pub mod style;

pub mod prelude {
  pub use crate::{
    article::*, cli::*, comp_info::*, ident_table::*, lib_type::*, logger::*, mode::*, path::*,
    project_type::*, reports::*, sources::*, style::*, project_config::*
  };
  pub use anyhow::{Result, anyhow, bail};
  pub use ariadne::{Color, Label, Report, ReportKind, Source};
  pub use colored::Colorize;
  pub use fs_err as fs;
  pub use log::{debug, error, info, warn};
  pub use parking_lot::*;
  pub use rayon::prelude::*;
  use std::ops::Range;
  pub use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
  };

  pub type Span = Range<usize>;
}
