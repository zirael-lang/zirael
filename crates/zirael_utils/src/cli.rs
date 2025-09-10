use crate::clap::CLAP_STYLING;
use crate::dependency::Dependency;
use crate::lib_type::LibType;
use crate::mode::Mode;
use crate::prelude::PackageType;
use clap::Parser;
use std::path::PathBuf;

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
  pub ty: PackageType,

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
  pub mode: Mode,

  #[arg(value_name = "name", long = "name", help = "Name of the project")]
  pub name: String,

  #[arg(
    value_name = "lib-type",
    long = "lib",
    help = "Type of the library to generate",
    default_value = "static"
  )]
  pub lib_type: LibType,

  #[arg(
    value_name = "output",
    help = "Path where the codegen should be saved to",
    long = "output",
    short = 'o'
  )]
  pub output: PathBuf,

}
