use crate::logger::setup_logger;
use clap::{Parser, builder::Styles};
use std::env::current_dir;
use zirael_core::prelude::*;

#[derive(Parser)]
#[command(name = "zirael")]
#[command(bin_name = "zr")]
#[command(styles = CLAP_STYLING)]
pub struct Cli {
    #[arg(value_name = "input", help = "Each input file is compiled as its own project.")]
    files: Vec<PathBuf>,
    #[arg(
        value_name = "verbose",
        short = 'v',
        long = "verbose",
        help = "Enable verbose logging: debug and trace"
    )]
    verbose: bool,
    #[arg(
        value_name = "dependencies",
        short = 'd',
        long = "dependencies",
        help = "Add dependencies that will be resolved by the compiler. \
    Each dependency should be a path to its entry point, with all imports \
    resolved relative to this path. Example: -d std=./std/lib.zr allows \
    the compiler to resolve 'import \"std/io\"'.\
    Order is important, because if one dependency depends on another, but it isn't compiled yes, the compiler will fail."
    )]
    dependencies: Vec<String>,
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
    setup_logger(cli.verbose);

    if cli.files.is_empty() {
        bail!("No input files provided.")
    }
    let mut context = Context::new();

    for dependency in &cli.dependencies {
        let text = dependency.split('=').collect::<Vec<&str>>();
        if text.len() != 2 {
            error!(
                "Invalid dependency: {}. The correct format is: {{name}}={{entrypoint of dependency}}",
                dependency
            );
            continue;
        }
        let dep = Dependency::new(text[0].to_string(), PathBuf::from(text[1]));

        if context.dependencies().contains(&dep) {
            error!("Found multiple dependencies with the same name: {}", text[0]);
            continue;
        }
        context.add_dependency(dep);
    }
    info!(
        "linked dependencies: {}",
        context.dependencies().iter().map(|dep| dep.name()).collect::<Vec<&str>>().join(", ")
    );

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

        let file = context.sources().add_owned(contents, file);
        let mut unit = CompilationUnit::new(file, context.clone());
        unit.compile();
    }

    Ok(())
}
