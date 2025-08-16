use log::LevelFilter;

pub fn setup_logger(verbose: bool, test_logger: bool) {
    let level = if verbose { LevelFilter::Debug } else { LevelFilter::Info };

    let mut builder = env_logger::Builder::from_default_env();
    builder.filter_level(level);

    if test_logger {
        builder.format(|buf, record| {
            use std::io::Write as _;
            writeln!(buf, "{} {}", record.level().to_string().to_lowercase(), record.args())
        });
    } else {
        builder.format(|buf, record| {
            use std::io::Write as _;

            let level_color = match record.level() {
                log::Level::Error => "\x1b[31m",
                log::Level::Warn => "\x1b[33m",
                log::Level::Info => "\x1b[36m",
                log::Level::Debug => "\x1b[37m",
                log::Level::Trace => "\x1b[90m",
            };

            writeln!(
                buf,
                "{}{}\x1b[0m \x1b[90m{}\x1b[0m {}",
                level_color,
                record.level().to_string().to_lowercase(),
                record.module_path().unwrap_or("unknown"),
                record.args()
            )
        });
    }

    builder.init();
}
