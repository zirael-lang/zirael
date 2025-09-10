use crate::{
  compiler::{Compiler, CompilerKind},
  detect::{detect_compiler, resolve_from_kind},
};
use anyhow::{Context as _, Result, bail, ensure};
use indicatif::{ProgressBar, ProgressStyle};
use log::{debug, warn};
use std::{
  collections::HashSet,
  ffi::OsStr,
  path::{Path, PathBuf},
  process::Command,
  sync::Arc,
  time::Duration,
};
use zirael_utils::prelude::{HEADER, LibType, Mode, PackageType};

pub mod compiler;
pub mod detect;

#[derive(Debug)]
pub struct CBuild {
  compiler: Compiler,
  source_files: HashSet<Arc<OsStr>>,
  flags: Vec<Arc<OsStr>>,
  include_dirs: HashSet<Arc<Path>>,
  library_dirs: HashSet<Arc<Path>>,
  libraries: HashSet<Arc<OsStr>>,
  project_type: PackageType,
  build_dir: Arc<Path>,
  mode: Mode,
  defines: HashSet<Arc<OsStr>>,
  lib_type: LibType,
  c_standard: Option<String>,
}

impl CBuild {
  pub fn new(
    compiler: Option<CompilerKind>,
    project_type: PackageType,
    build_dir: impl AsRef<Path>,
    lib_type: LibType,
    mode: Mode,
  ) -> Result<Self> {
    let build_dir = build_dir.as_ref();

    if let Some(parent) = build_dir.parent() {
      fs_err::create_dir_all(parent)?;
    }

    let compiler =
      if let Some(kind) = compiler { resolve_from_kind(kind)? } else { detect_compiler()? };
    Ok(Self {
      compiler,
      source_files: HashSet::new(),
      flags: Vec::new(),
      include_dirs: HashSet::new(),
      library_dirs: HashSet::new(),
      libraries: HashSet::new(),
      project_type,
      build_dir: Arc::from(build_dir),
      mode,
      defines: HashSet::new(),
      lib_type,
      c_standard: None,
    })
  }

  pub fn compiler_kind(&self) -> &CompilerKind {
    self.compiler.kind()
  }

  pub fn add_source<P: AsRef<OsStr>>(&mut self, path: P) -> &mut Self {
    self.source_files.insert(Arc::from(path.as_ref()));
    self
  }

  pub fn add_sources<I, P>(&mut self, paths: I) -> &mut Self
  where
    I: IntoIterator<Item = P>,
    P: AsRef<OsStr>,
  {
    for path in paths {
      self.add_source(path);
    }
    self
  }

  pub fn add_include<P: AsRef<Path>>(&mut self, dir: P) -> &mut Self {
    self.include_dirs.insert(Arc::from(dir.as_ref()));
    self
  }

  pub fn add_library_dir<P: AsRef<Path>>(&mut self, dir: P) -> &mut Self {
    self.library_dirs.insert(Arc::from(dir.as_ref()));
    self
  }

  pub fn add_library<S: AsRef<OsStr>>(&mut self, lib: S) -> &mut Self {
    self.libraries.insert(Arc::from(lib.as_ref()));
    self
  }

  pub fn add_flag<S: AsRef<OsStr>>(&mut self, flag: S) -> &mut Self {
    self.flags.push(Arc::from(flag.as_ref()));
    self
  }

  pub fn set_c_standard<S: Into<String>>(&mut self, standard: S) -> &mut Self {
    self.c_standard = Some(standard.into());
    self
  }

  pub fn define<S: AsRef<OsStr>>(&mut self, define: S) -> &mut Self {
    self.defines.insert(Arc::from(define.as_ref()));
    self
  }

  pub fn compile(&self, output_name: impl AsRef<OsStr>) -> Result<PathBuf> {
    ensure!(!self.source_files.is_empty(), "No source files specified");
    println!();

    let pb = ProgressBar::new_spinner();
    pb.set_style(
      ProgressStyle::default_spinner()
        .tick_chars("⠁⠂⠄⡀⢀⠠⠐⠈ ")
        .template("{spinner:.green} {msg}")
        .unwrap(),
    );
    pb.set_message("Compiling C sources...");
    pb.enable_steady_tick(Duration::from_millis(100));

    match self.project_type {
      PackageType::Library if self.lib_type == LibType::Static => {
        self.compile_static_library(output_name.as_ref(), &pb)
      }
      _ => self.compile_direct(output_name.as_ref(), &pb),
    }
  }

  fn compile_static_library(&self, output_name: &OsStr, pb: &ProgressBar) -> Result<PathBuf> {
    let mut object_files = Vec::new();

    for (i, source_file) in self.source_files.iter().enumerate() {
      pb.set_message(format!(
        "Compiling {} ({}/{})",
        Path::new(source_file).file_name().unwrap_or(source_file).to_string_lossy(),
        i + 1,
        self.source_files.len()
      ));

      let obj_name =
        Path::new(source_file).file_stem().unwrap_or_else(|| OsStr::new("obj")).to_string_lossy();
      let obj_file = format!("{obj_name}.o");

      let mut command = Command::new(self.compiler.path());
      self.configure_compile_command(&mut command, source_file, &obj_file)?;

      debug!("Executing compilation command: {command:?}");

      let output =
        command.current_dir(&*self.build_dir).output().context("Failed to execute compiler")?;

      if !output.stdout.is_empty() {
        debug!("Compilation stdout: {}", String::from_utf8_lossy(&output.stdout));
      }
      if !output.stderr.is_empty() {
        debug!("Compilation stderr: {}", String::from_utf8_lossy(&output.stderr));
      }

      if !output.status.success() {
        pb.finish_with_message("❌ Compilation failed");
        let stderr = String::from_utf8_lossy(&output.stderr);
        if !stderr.is_empty() {
          bail!("Compilation failed with error: {}", stderr);
        } else {
          bail!("Compilation failed with exit code: {}", output.status);
        }
      }

      object_files.push(obj_file);
    }

    pb.set_message("Creating static library...");
    let output_path = self.create_static_library(output_name, &object_files)?;

    for obj_file in &object_files {
      let obj_path = self.build_dir.join(obj_file);
      if obj_path.exists() {
        if let Err(e) = std::fs::remove_file(&obj_path) {
          warn!("Failed to clean up object file {}: {}", obj_path.display(), e);
        }
      }
    }

    pb.finish_with_message(format!(
      "{style}{status:>12}{style:#} C static library",
      style = &HEADER,
      status = "✅ Finished"
    ));

    ensure!(
      output_path.exists(),
      "Library creation succeeded but output file was not created: {}",
      output_path.display()
    );

    Ok(output_path)
  }

  fn compile_direct(&self, output_name: &OsStr, pb: &ProgressBar) -> Result<PathBuf> {
    let mut command = Command::new(self.compiler.path());
    self.configure_command(&mut command, output_name)?;

    debug!("Executing compilation command: {command:?}");

    pb.set_message(format!("Running {} compiler...", self.compiler.kind().name()));

    let output =
      command.current_dir(&*self.build_dir).output().context("Failed to execute compiler")?;

    if !output.stdout.is_empty() {
      debug!("Compilation stdout: {}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
      debug!("Compilation stderr: {}", String::from_utf8_lossy(&output.stderr));
    }

    if !output.status.success() {
      pb.finish_with_message("❌ Compilation failed");

      let stderr = String::from_utf8_lossy(&output.stderr);
      if !stderr.is_empty() {
        bail!("Compilation failed with error: {}", stderr);
      } else {
        bail!("Compilation failed with exit code: {}", output.status);
      }
    }

    let output_path = self.get_output_path(output_name);

    pb.finish_with_message(format!(
      "{style}{status:>12}{style:#} C compilation",
      style = &HEADER,
      status = "✅ Finished"
    ));

    ensure!(
      output_path.exists(),
      "Compilation succeeded but output file was not created: {}",
      output_path.display()
    );

    Ok(output_path)
  }

  fn configure_compile_command(
    &self,
    command: &mut Command,
    source_file: &OsStr,
    obj_file: &str,
  ) -> Result<()> {
    command.arg(source_file);

    for dir in &self.include_dirs {
      command.arg(if self.compiler.kind() == &CompilerKind::Msvc { "/I" } else { "-I" });
      command.arg(dir.as_os_str());
    }

    for define in &self.defines {
      command.arg(if self.compiler.kind() == &CompilerKind::Msvc { "/D" } else { "-D" });
      command.arg(define);
    }

    self.add_optimization_flags(command);
    self.add_c_standard_flag(command, self.c_standard.as_deref())?;

    // Compile only (don't link)
    if self.compiler.kind() == &CompilerKind::Msvc {
      command.arg("/c");
      command.arg(format!("/Fo:{obj_file}"));
    } else {
      command.arg("-c");
      command.arg("-o").arg(obj_file);
    }

    for flag in &self.flags {
      command.arg(flag);
    }

    Ok(())
  }

  fn create_static_library(&self, output_name: &OsStr, object_files: &[String]) -> Result<PathBuf> {
    let output_path = self.get_output_path(output_name);

    let mut command = if self.compiler.kind() == &CompilerKind::Msvc {
      let mut cmd = Command::new("lib");
      cmd.arg(format!("/OUT:{}", output_path.display()));
      cmd
    } else {
      let mut cmd = Command::new("ar");
      cmd.arg("rcs");
      cmd.arg(&output_path);
      cmd
    };

    for obj_file in object_files {
      command.arg(obj_file);
    }

    debug!("Executing library creation command: {command:?}");

    let output = command
      .current_dir(&*self.build_dir)
      .output()
      .context("Failed to execute library archiver")?;

    if !output.stdout.is_empty() {
      debug!("Library creation stdout: {}", String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
      debug!("Library creation stderr: {}", String::from_utf8_lossy(&output.stderr));
    }

    if !output.status.success() {
      let stderr = String::from_utf8_lossy(&output.stderr);
      if !stderr.is_empty() {
        bail!("Library creation failed with error: {}", stderr);
      } else {
        bail!("Library creation failed with exit code: {}", output.status);
      }
    }

    Ok(output_path)
  }

  fn configure_command(&self, command: &mut Command, output_name: &OsStr) -> Result<()> {
    for file in &self.source_files {
      command.arg(file);
    }

    for dir in &self.include_dirs {
      command.arg(if self.compiler.kind() == &CompilerKind::Msvc { "/I" } else { "-I" });
      command.arg(dir.as_os_str());
    }

    for dir in &self.library_dirs {
      command.arg(if self.compiler.kind() == &CompilerKind::Msvc { "/LIBPATH:" } else { "-L" });
      command.arg(dir.as_os_str());
    }

    for lib in &self.libraries {
      command.arg(if self.compiler.kind() == &CompilerKind::Msvc { "/DEFAULTLIB:" } else { "-l" });
      command.arg(lib);
    }

    for define in &self.defines {
      command.arg(if self.compiler.kind() == &CompilerKind::Msvc { "/D" } else { "-D" });
      command.arg(define);
    }

    self.add_optimization_flags(command);
    self.add_c_standard_flag(command, self.c_standard.as_deref())?;
    self.configure_output(command, output_name)?;

    for flag in &self.flags {
      command.arg(flag);
    }

    Ok(())
  }

  fn add_optimization_flags(&self, command: &mut Command) {
    let flags = self.compiler.optimization_flags(self.mode == Mode::Debug);
    for flag in flags {
      command.arg(flag);
    }
  }

  fn add_c_standard_flag(&self, command: &mut Command, standard: Option<&str>) -> Result<()> {
    let std_version = standard.unwrap_or("c17");

    if !self.compiler.supports_c_standard(std_version) {
      warn!(
        "Compiler {} may not support {} standard, using anyway",
        self.compiler.kind().name(),
        std_version
      );
    }

    let flag = if self.compiler.kind() == &CompilerKind::Msvc {
      format!("/std:{std_version}")
    } else {
      format!("-std={std_version}")
    };

    command.arg(flag);
    Ok(())
  }

  fn configure_output(&self, command: &mut Command, output_name: &OsStr) -> Result<()> {
    match self.project_type {
      PackageType::Library => match self.lib_type {
        LibType::Static => {
          if self.compiler.kind() == &CompilerKind::Msvc {
            command.arg("/c");
            command.arg(format!("/Fo:{}", output_name.to_string_lossy()));
          } else {
            command.arg("-c");
            command.arg("-o").arg(output_name);
          }
        }
        LibType::Dynamic => {
          if self.compiler.kind() == &CompilerKind::Msvc {
            command.arg("/LD");
            command.arg(format!("/Fe:{}", output_name.to_string_lossy()));
          } else {
            command.arg("-shared");
            command.arg("-o").arg(if cfg!(windows) {
              PathBuf::from(output_name).with_extension("dll")
            } else {
              output_name.into()
            });
          }
        }
      },
      PackageType::Binary => {
        if self.compiler.kind() == &CompilerKind::Msvc {
          command.arg(format!("/Fe:{}", output_name.to_string_lossy()));
        } else {
          command.arg("-o").arg(if cfg!(windows) {
            PathBuf::from(output_name).with_extension("exe")
          } else {
            output_name.into()
          });
        }
      }
    }
    Ok(())
  }

  pub fn disable_warnings(&mut self) -> &mut Self {
    self.add_flag(if self.compiler.kind() == &CompilerKind::Msvc { "/W0" } else { "-w" });
    self.add_flag("-Waddress-of-temporary");
    self
  }

  fn get_output_path(&self, output_name: &OsStr) -> PathBuf {
    let base_path = self.build_dir.join(output_name);

    match self.project_type {
      PackageType::Library => match self.lib_type {
        LibType::Static => {
          if cfg!(windows) && self.compiler.kind() == &CompilerKind::Msvc {
            base_path.with_extension("lib")
          } else {
            let name = output_name.to_string_lossy();
            if name.starts_with("lib") {
              base_path.with_extension("a")
            } else {
              self.build_dir.join(format!("lib{name}.a"))
            }
          }
        }
        LibType::Dynamic => {
          if cfg!(windows) {
            base_path.with_extension("dll")
          } else if cfg!(target_os = "macos") {
            base_path.with_extension("dylib")
          } else {
            base_path.with_extension("so")
          }
        }
      },
      PackageType::Binary => {
        if cfg!(windows) {
          base_path.with_extension("exe")
        } else {
          base_path
        }
      }
    }
  }
}
