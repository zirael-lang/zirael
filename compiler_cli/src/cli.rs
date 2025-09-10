use clap::Parser as _;
use zirael_core::prelude::*;

pub fn try_cli() -> Result<()> {
  let cli = Cli::parse();
  setup_logger(cli.verbose, false);

  let config = ProjectConfig::from_cli(cli)?;
  compile_project(&config)
}