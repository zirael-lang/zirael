use crate::dependency::Dependency;
use crate::prelude::{LibType, Mode, PackageType};
use std::path::PathBuf;
use zirael_diagnostics::prelude::DiagnosticOutputType;

#[derive(Debug, Clone)]
pub struct ProjectConfig {
  pub entrypoint: PathBuf,
  pub project_type: PackageType,
  pub packages: Vec<Dependency>,
  pub mode: Mode,
  pub name: String,
  pub lib_type: LibType,
  pub output: PathBuf,
  pub root: PathBuf,
  pub diagnostic_output_type: DiagnosticOutputType,
  pub color: bool,
}

impl ProjectConfig {}
