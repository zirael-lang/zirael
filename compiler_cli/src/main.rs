use crate::{cli::try_cli, logger::setup_logger};
use std::process::exit;
use zirael_core::prelude::*;

mod cli;
mod logger;

fn main() {
    setup_logger();

    if let Err(e) = try_cli() {
        error!("{e:?}");
        exit(1);
    }
}
