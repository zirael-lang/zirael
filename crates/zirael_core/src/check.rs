use crate::context::Context;
use crate::prelude::{Colorize, CompilationUnit, FILE_EXTENSION, error};
use anyhow::Result;
use anyhow::bail;
use std::path::PathBuf;
use zirael_c_compiler::CBuild;
use zirael_utils::prelude::{
  CheckConfig, CompilationInfo, PackageType, ProjectConfig, canonicalize_with_strip,
  create_dir_all, info,
};

pub fn check_project(config: &CheckConfig) -> Result<()> {
  let context = Context::new();

  // when checking we don't need an output directory
  let write_to = PathBuf::new();

  for dep in &config.packages {
    if context.packages().contains(dep) {
      error!("Found multiple packages with the same name: {}", dep.name);
      continue;
    }
    context.packages().add(dep.clone());

    let entrypoint = fs_err::read_to_string(&dep.entrypoint)?;
    let file = context.sources().add_owned(entrypoint, dep.entrypoint.clone());

    let mut unit = CompilationUnit::new(
      file,
      context.clone(),
      CompilationInfo {
        mode: config.mode,
        name: dep.name.clone(),
        root: dep.root.clone(),
        write_to: write_to.clone(),
        ty: PackageType::Library,
        keep_dead_code: false,
      },
    );

    let (_, _) = unit.check();
  }

  info!(
    "linked packages: {}",
    context.packages().all().iter().map(|dep| dep.name()).collect::<Vec<&str>>().join(", ")
  );

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

  let file = config.root.join(file);
  let contents = fs_err::read_to_string(file.clone())?;

  let file = context.sources().add_owned(contents, file);
  let mut unit = CompilationUnit::new(
    file,
    context.clone(),
    CompilationInfo {
      mode: config.mode,
      name: config.name.clone(),
      root: config.root.clone(),
      write_to: write_to.clone(),
      ty: config.project_type,
      keep_dead_code: false,
    },
  );

  let (_, _) = unit.check();

  Ok(())
}
