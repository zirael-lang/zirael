use clap::{Parser, builder::Styles};
use zirael::prelude::*;

#[derive(Parser)]
#[command(name = "zirael")]
#[command(bin_name = "zr")]
#[command(styles = CLAP_STYLING)]
struct Cli {
    #[arg(value_name = "input")]
    files: Vec<PathBuf>,
}

pub const CLAP_STYLING: Styles = Styles::styled()
    .header(style::HEADER)
    .usage(style::USAGE)
    .literal(style::LITERAL)
    .placeholder(style::PLACEHOLDER)
    .error(style::ERROR)
    .valid(style::VALID)
    .invalid(style::INVALID);

pub fn try_cli() -> Result<()> {
    let cli = Cli::parse();
    let context = Context::new();

    // Handle file inputs
    for file in &cli.files {
        println!("Processing file: {}", file.display());
    }

    Ok(())
}
