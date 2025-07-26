use clap::{Parser, builder::Styles};
use std::env::current_dir;
use zirael_core::prelude::*;

#[derive(Parser)]
#[command(name = "zirael")]
#[command(bin_name = "zr")]
#[command(styles = CLAP_STYLING)]
struct Cli {
    #[arg(value_name = "input", help = "Each input file is compiled as its own project.")]
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
    if cli.files.is_empty() {
        bail!("No input files provided.")
    }

    let context = Context::new();

    // Handle file inputs
    for file in &cli.files {
        info!("processing entrypoint: {}", file.display());

        if let Some(ext) = file.extension() {
            if ext != FILE_EXTENSION {
                bail!(
                    "Found an entry point with invalid extension: {}. It must be {}",
                    file.display().to_string().dimmed(),
                    FILE_EXTENSION.dimmed()
                );
            }
        }

        let file = current_dir()?.join(file);
        let contents = fs::read_to_string(file.clone())?;

        let file = context.sources().add_owned(contents, Some(file));
        let unit = CompilationUnit::new(file, context.clone());
        unit.compile();
    }

    Ok(())
}
