use clap::Parser as _;
use std::io::{Cursor, Write as _, stderr};
use std::sync::Arc;
use std::time::Instant;
use zirael_cli::Cli;
use zirael_cli::logger::setup_logger;
use zirael_core::prelude::*;

pub fn try_cli() -> Result<()> {
  let instant = Instant::now();
  let cli = Cli::parse();
  setup_logger(cli.verbose, cli.no_color);

  let check_config = ProjectConfig::try_from(cli)?;

  let output = Arc::new(Mutex::new(Cursor::new(vec![])));
  check_project(&check_config, output.clone(), false)?;
  stderr().write_all(output.lock().get_ref())?;

  info!("finished in {:.2?}", instant.elapsed());
  Ok(())
}
