mod article;
mod comp_info;
pub mod ident_table;
mod lib_type;
pub mod logger;
mod mode;
mod path;
mod project_type;
mod reports;
pub mod sources;
pub mod style;

pub mod prelude {
    pub use crate::{
        article::*, comp_info::*, ident_table::*, lib_type::*, logger::*, mode::*, path::*,
        project_type::*, reports::*, sources::*, style::*,
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
