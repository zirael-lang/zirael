use crate::context::Context;
use crate::prelude::{Colorize, CompilationUnit, FILE_EXTENSION, error};
use anyhow::Result;
use anyhow::bail;
use zirael_c_compiler::CBuild;
use zirael_utils::prelude::{
  CompilationInfo, PackageType, ProjectConfig, canonicalize_with_strip, create_dir_all, info,
};

pub fn compile_project(config: &ProjectConfig) -> Result<()> {
  let context = Context::new();

  create_dir_all(&config.output)?;
  let write_to = canonicalize_with_strip(config.root.join(&config.output))?;

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
      },
    );
    unit.compile()?;
  }

  info!(
    "linked packages: {}",
    context.packages().all().iter().map(|dep| dep.name()).collect::<Vec<&str>>().join(", ")
  );

  let file = &config.entrypoint;
  info!("processing entrypoint: {} with {} mode", file.display(), config.mode);

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
    },
  );
  let path = unit.compile()?;
  let mut builder =
    CBuild::new(None, config.project_type, write_to.clone(), config.lib_type, config.mode)?;

  builder.add_source(path).compile(&config.name)?;

  Ok(())
}
