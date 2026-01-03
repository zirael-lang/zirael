use crate::prelude::{Colorize as _, CompilationUnit, FILE_EXTENSION, error};
use anyhow::Result;
use anyhow::bail;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use zirael_diagnostics::DiagnosticWriter;
use zirael_source::prelude::Sources;
use zirael_source::source_file::SourceFile;
use zirael_utils::context::Context;
use zirael_utils::prelude::{PackageType, ProjectConfig, Session, info};

pub fn check_project(
  config: &ProjectConfig,
  writer: DiagnosticWriter,
) -> Result<Session> {
  let file = &config.entrypoint;
  info!(
    "checking entrypoint: {} with {} mode",
    file.display(),
    config.mode
  );

  if let Some(ext) = file.extension() {
    if ext != FILE_EXTENSION {
      bail!(
        "Found an entry point with invalid extension: {}. It must be {}",
        file.display().to_string().dimmed(),
        FILE_EXTENSION.dimmed()
      );
    }
  }
  let sources = Arc::new(Sources::new());

  let sess = Session::new(config.clone(), sources.clone(), writer);
  let context = &mut Context::new(&sess, sources.clone());

  let file = sess.config().root.join(file);
  let contents = fs_err::read_to_string(file.clone())?;

  let file_id = sources.add(contents, file.clone());
  let mut unit = CompilationUnit::new(file_id, &context);

  let _ = unit.check();

  Ok(sess)
}
