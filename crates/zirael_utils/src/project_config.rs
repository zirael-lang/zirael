use crate::dependency::Dependency;
use crate::prelude::{LibType, Mode, PackageType};
use std::path::PathBuf;

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
}

impl ProjectConfig {}

#[derive(Debug, Clone)]
pub struct CheckConfig {
  pub entrypoint: PathBuf,
  pub project_type: PackageType,
  pub packages: Vec<Dependency>,
  pub mode: Mode,
  pub name: String,
  pub root: PathBuf,
}

impl CheckConfig {}
