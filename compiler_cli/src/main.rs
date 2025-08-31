use crate::cli::try_cli;
use std::process::exit;
use zirael_core::prelude::*;

mod cli;

fn main() {
  if let Err(e) = try_cli() {
    error!("{e:?}");
    exit(1);
  }
}
