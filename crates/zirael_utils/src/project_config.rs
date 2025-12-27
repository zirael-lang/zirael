use crate::cli::Cli;
use crate::dependency::Dependency;
use crate::prelude::{LibType, Mode, PackageType};
use std::env::current_dir;
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

impl ProjectConfig {
  pub fn from_cli(cli: Cli) -> anyhow::Result<Self> {
    let root = current_dir()?;

    Ok(Self {
      entrypoint: cli.entrypoint,
      project_type: cli.ty,
      packages: cli.packages,
      mode: cli.mode,
      name: cli.name,
      lib_type: cli.lib_type,
      output: cli.output,
      root,
    })
  }
}

#[derive(Debug, Clone)]
pub struct CheckConfig {
  pub entrypoint: PathBuf,
  pub project_type: PackageType,
  pub packages: Vec<Dependency>,
  pub mode: Mode,
  pub name: String,
  pub root: PathBuf,
}

impl CheckConfig {
  pub fn from_cli(cli: Cli) -> anyhow::Result<Self> {
    let root = current_dir()?;

    Ok(Self {
      entrypoint: cli.entrypoint,
      project_type: cli.ty,
      packages: cli.packages,
      mode: cli.mode,
      name: cli.name,
      root,
    })
  }
}
