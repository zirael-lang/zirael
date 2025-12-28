use crate::prelude::{Colorize as _, CompilationUnit, FILE_EXTENSION, error};
use anyhow::Result;
use anyhow::bail;
use std::path::PathBuf;
use zirael_utils::context::Context;
use zirael_utils::prelude::{CheckConfig, PackageType, Session, SourceFile, info, Diag, DiagnosticLevel};

pub fn check_project(config: &CheckConfig) -> Result<()> {
  let sess = Session::new(config.clone());
  sess.dcx().add(Diag {
    message: "Hello!".to_string(),
    level: DiagnosticLevel::Error,
    labels: vec![],
    helps: vec![],
    notes: vec![]
  });
  let context = &mut Context::new(&sess);

  let file = &config.entrypoint;
  info!("checking entrypoint: {} with {} mode", file.display(), config.mode);

  if let Some(ext) = file.extension() {
    if ext != FILE_EXTENSION {
      bail!(
        "Found an entry point with invalid extension: {}. It must be {}",
        file.display().to_string().dimmed(),
        FILE_EXTENSION.dimmed()
      );
    }
  }

  let file = sess.config().root.join(file);
  let contents = fs_err::read_to_string(file.clone())?;

  let file_id = context.sources().add(SourceFile::new(contents, file.clone()));
  let mut unit = CompilationUnit::new(file_id, &context);

  let _ = unit.check();

  Ok(())
}
