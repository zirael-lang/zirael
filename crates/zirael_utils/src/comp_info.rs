use crate::{mode::Mode, prelude::PackageType};
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CompilationInfo {
  pub mode: Mode,
  pub root: PathBuf,
  pub name: String,
  pub write_to: PathBuf,
  pub ty: PackageType,
  pub keep_dead_code: bool,
}
