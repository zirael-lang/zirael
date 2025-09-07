use clap::{Parser, builder::Styles};
use std::env::current_dir;
use zirael_c_compiler::CBuild;
use zirael_core::prelude::*;

pub fn try_cli() -> Result<()> {
  let cli = Cli::parse();
  setup_logger(cli.verbose, false);

  let config = ProjectConfig::from_cli(cli)?;
  compile_project(&config)
}