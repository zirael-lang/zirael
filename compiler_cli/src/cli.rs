use clap::{Parser, builder::Styles};
use std::env::current_dir;
use zirael_core::prelude::*;

#[derive(Parser)]
#[command(name = "zirael")]
#[command(bin_name = "zr")]
#[command(styles = CLAP_STYLING)]
pub struct Cli {
    #[arg(value_name = "entrypoint", help = "Entrypoint of the project")]
    entrypoint: PathBuf,

    #[arg(value_name = "type", help = "Type of the project", default_value = "library")]
    ty: ProjectType,

    #[arg(
        value_name = "verbose",
        short = 'v',
        long = "verbose",
        help = "Enable verbose logging: debug and trace"
    )]
    verbose: bool,

    #[arg(
        value_name = "packages",
        short = 'd',
        long = "packages",
        help = "Add packages that will be resolved by the compiler. \
    Each dependency should be a path to its entry point, with all imports \
    resolved relative to this path. Example: -d std=./std/lib.zr allows \
    the compiler to resolve 'import \"std/io\"'. \
    Order is important, because if one dependency depends on another, but it isn't compiled yet, the compiler will fail."
    )]
    packages: Vec<String>,

    #[arg(
        value_name = "mode",
        short = 'm',
        long = "mode",
        help = "Compilation mode: either 'debug' or 'release'",
        default_value = "debug"
    )]
    mode: Mode,

    #[arg(value_name = "name", long = "name", help = "Name of the project")]
    name: String,

    #[arg(
        value_name = "lib-type",
        long = "lib",
        help = "Type of the library to generate",
        default_value = "static"
    )]
    lib_type: LibType,
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
    setup_logger(cli.verbose, false);

    let mut context = Context::new();

    for package in &cli.packages {
        let text = package.split('=').collect::<Vec<&str>>();
        if text.len() != 2 {
            error!(
                "Invalid package: {}. The correct format is: {{name}}={{entrypoint of package}}",
                package
            );
            continue;
        }
        let dep = Dependency::new(text[0].to_string(), PathBuf::from(text[1]));

        if context.packages().contains(&dep) {
            error!("Found multiple packages with the same name: {}", text[0]);
            continue;
        }
        context.add_package(dep);
    }
    info!(
        "linked packages: {}",
        context.packages().iter().map(|dep| dep.name()).collect::<Vec<&str>>().join(", ")
    );

    let file = cli.entrypoint;
    info!("processing entrypoint: {} with {:?} mode", file.display(), cli.mode);

    if let Some(ext) = file.extension() {
        if ext != FILE_EXTENSION {
            bail!(
                "Found an entry point with invalid extension: {}. It must be {}",
                file.display().to_string().dimmed(),
                FILE_EXTENSION.dimmed()
            );
        }
    }

    let root = current_dir()?;
    let file = root.join(file);
    let contents = fs::read_to_string(file.clone())?;

    let file = context.sources().add_owned(contents, file);
    let mut unit = CompilationUnit::new(
        file,
        context.clone(),
        CompilationInfo { mode: cli.mode, name: cli.name.clone(), root },
    );
    unit.compile()?;

    Ok(())
}
