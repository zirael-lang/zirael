use clap::Parser as _;
use zirael_core::prelude::*;

pub fn try_cli() -> Result<()> {
  let cli = Cli::parse();
  setup_logger(cli.verbose, false);

  let check_config = CheckConfig::from_cli(cli)?;
  check_project(&check_config)
}
