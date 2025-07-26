mod context;
mod unit;

pub mod vars {
    pub const FILE_EXTENSION: &'static str = "zr";
}

#[allow(unused_imports)]
pub mod prelude {
    pub use crate::{context::*, unit::*, vars::*};
    pub use anyhow::{Result, anyhow, bail};
    pub use colored::Colorize;
    pub use fs_err as fs;
    pub use log::{error, info, warn};
    pub use parking_lot::RwLock;
    pub use std::path::PathBuf;
    pub use zirael_parser::*;
    pub use zirael_utils::{prelude::*, *};
}
