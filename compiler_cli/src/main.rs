use crate::{cli::try_cli, logger::setup_logger};

mod cli;
mod logger;

fn main() -> anyhow::Result<()> {
    setup_logger();
    try_cli()
}
