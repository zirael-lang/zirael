use clap::Parser as _;
use zirael_cli::Cli;
use zirael_cli::logger::setup_logger;
use zirael_core::prelude::*;

pub fn try_cli() -> Result<()> {
  let cli = Cli::parse();
  setup_logger(cli.verbose, false);

  let check_config = CheckConfig::try_from(cli)?;
  check_project(&check_config)
}
