mod article;
pub mod ident_table;
mod mode;
mod path;
mod reports;
pub mod sources;
pub mod style;

pub mod prelude {
    pub use crate::{article::*, ident_table::*, mode::*, reports::*, sources::*, style::*, path::*};
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
