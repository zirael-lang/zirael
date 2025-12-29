use crate::CLAP_STYLING;
use clap::{Parser, ValueEnum};
use std::env::current_dir;
use std::path::PathBuf;
use zirael_utils::dependency::Dependency;
use zirael_utils::project_config::ProjectConfig;
use zirael_utils::{enums, term_style};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug, Default)]
pub enum CliMode {
  #[default]
  Debug,
  Release,
}

impl From<CliMode> for enums::mode::Mode {
  fn from(value: CliMode) -> Self {
    match value {
      CliMode::Debug => Self::Debug,
      CliMode::Release => Self::Release,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum CliPackageType {
  #[value(alias("bin"))]
  Binary,
  #[value(alias("lib"))]
  Library,
}

impl From<CliPackageType> for enums::project_type::PackageType {
  fn from(value: CliPackageType) -> Self {
    match value {
      CliPackageType::Binary => Self::Binary,
      CliPackageType::Library => Self::Library,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum CliLibType {
  Static,
  Dynamic,
}

impl From<CliLibType> for enums::lib_type::LibType {
  fn from(value: CliLibType) -> Self {
    match value {
      CliLibType::Static => Self::Static,
      CliLibType::Dynamic => Self::Dynamic,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum DiagOutputType {
  JSON,
  HumanReadable,
}

impl From<DiagOutputType> for DiagnosticOutputType {
  fn from(value: DiagOutputType) -> Self {
    match value {
      DiagOutputType::JSON => Self::JSON,
      DiagOutputType::HumanReadable => Self::HumanReadable,
    }
  }
}

#[derive(Parser)]
#[command(name = "zirael")]
#[command(bin_name = "zirael")]
#[command(styles = CLAP_STYLING)]
pub struct Cli {
  #[arg(value_name = "entrypoint", help = "Entrypoint of the project")]
  pub entrypoint: PathBuf,

  #[arg(
    value_name = "type",
    short = 't',
    long = "type",
    help = "Type of the project",
    default_value = "library"
  )]
  pub ty: CliPackageType,

  #[arg(
    value_name = "verbose",
    short = 'v',
    long = "verbose",
    help = "Enable verbose logging: debug and trace"
  )]
  pub verbose: bool,

  #[arg(
    value_name = "packages",
    short = 'd',
    long = "packages",
    help = "Add packages that will be resolved by the compiler. \
        Format: name:write_to=entrypoint \
        Example: -d std:./std=./std/lib.zr \
        Order is important, because if one dependency depends on another, but it isn't compiled yet, the compiler will fail."
  )]
  pub packages: Vec<Dependency>,

  #[arg(
    value_name = "mode",
    short = 'm',
    long = "mode",
    help = "Compilation mode: either 'debug' or 'release'",
    default_value = "debug"
  )]
  pub mode: CliMode,

  #[arg(value_name = "name", long = "name", help = "Name of the project")]
  pub name: String,

  #[arg(
    value_name = "lib-type",
    long = "lib",
    help = "Type of the library to generate",
    default_value = "static"
  )]
  pub lib_type: CliLibType,

  #[arg(
    value_name = "diagnostic-output",
    long = "diagnostic-output",
    help = "Emitter for diagnostics",
    default_value = "human-readable"
  )]
  pub diag_output_type: DiagOutputType,

  #[arg(
    value_name = "output",
    help = "Path where the codegen should be saved to",
    long = "output",
    short = 'o'
  )]
  pub output: PathBuf,

  #[arg(
    value_name = "check-only",
    help = "Do not output codegen or compile the code, only check for errors",
    long = "check-only"
  )]
  pub check_only: bool,

  #[arg(value_name = "no-color", help = "No color in the output", long = "no-color")]
  pub no_color: bool,
}

impl TryFrom<Cli> for ProjectConfig {
  type Error = anyhow::Error;

  fn try_from(cli: Cli) -> Result<Self, Self::Error> {
    let root = current_dir()?;

    Ok(Self {
      entrypoint: cli.entrypoint,
      project_type: cli.ty.into(),
      packages: cli.packages,
      mode: cli.mode.into(),
      name: cli.name,
      lib_type: cli.lib_type.into(),
      output: cli.output,
      root,
      diagnostic_output_type: cli.diag_output_type.into(),
      color: !cli.no_color,
    })
  }
}

pub use term_style::{
  ERROR, GOOD, HEADER, INVALID, LITERAL, NOP, NOTE, PLACEHOLDER, USAGE, VALID, WARN,
};
use zirael_diagnostics::prelude::DiagnosticOutputType;
