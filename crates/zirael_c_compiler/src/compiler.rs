use log::{debug, warn};
use serde::{Deserialize, Serialize};
use std::{
  fmt::{Display, Formatter},
  path::{Path, PathBuf},
  process::Command,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
  #[error("Compiler not found at path: {0}")]
  NotFound(PathBuf),
  #[error("Unsupported compiler version: {0}")]
  UnsupportedVersion(String),
  #[error("Failed to detect compiler version: {0}")]
  VersionDetectionFailed(String),
  #[error(transparent)]
  IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum CompilerKind {
  Msvc,
  Gcc,
  Clang,
}

impl CompilerKind {
  pub fn default_extension(&self) -> &str {
    match self {
      Self::Msvc => "exe",
      Self::Gcc | Self::Clang => {
        if cfg!(windows) {
          "exe"
        } else {
          ""
        }
      }
    }
  }

  pub fn is_unix_style(&self) -> bool {
    matches!(self, Self::Gcc | Self::Clang)
  }

  pub fn name(&self) -> &str {
    match self {
      Self::Msvc => "MSVC",
      Self::Gcc => "GCC",
      Self::Clang => "Clang",
    }
  }

  pub fn executable_names(&self) -> &[&str] {
    match self {
      Self::Msvc => &["cl.exe", "cl"],
      Self::Gcc => &["gcc.exe", "gcc"],
      Self::Clang => &["clang.exe", "clang"],
    }
  }
}

impl Display for CompilerKind {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name())
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilerVersion {
  pub major: u32,
  pub minor: u32,
  pub patch: Option<u32>,
  pub raw: String,
}

impl Display for CompilerVersion {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Some(patch) = self.patch {
      write!(f, "{}.{}.{}", self.major, self.minor, patch)
    } else {
      write!(f, "{}.{}", self.major, self.minor)
    }
  }
}

#[derive(Debug, Clone)]
pub struct Compiler {
  path: PathBuf,
  kind: CompilerKind,
  version: Option<CompilerVersion>,
}

impl Display for Compiler {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self.version {
      Some(version) => write!(
        f,
        "{} {} at {}",
        self.kind.name(),
        version,
        self.path.display()
      ),
      None => write!(f, "{} at {}", self.kind.name(), self.path.display()),
    }
  }
}

impl Compiler {
  pub fn new(path: PathBuf, kind: CompilerKind) -> Result<Self, CompilerError> {
    if !path.exists() {
      return Err(CompilerError::NotFound(path));
    }

    let mut compiler = Self {
      path,
      kind,
      version: None,
    };

    if let Err(e) = compiler.detect_version() {
      warn!("Failed to detect compiler version: {e}");
    }

    Ok(compiler)
  }

  pub fn path(&self) -> &Path {
    &self.path
  }

  pub fn kind(&self) -> &CompilerKind {
    &self.kind
  }

  pub fn version(&self) -> Option<&CompilerVersion> {
    self.version.as_ref()
  }

  fn detect_version(&mut self) -> Result<(), CompilerError> {
    debug!("Detecting version for compiler: {}", self.path.display());

    let version_arg = match self.kind {
      CompilerKind::Msvc => "/version",
      CompilerKind::Gcc => "--version",
      CompilerKind::Clang => "--version",
    };

    let output = Command::new(&self.path)
      .arg(version_arg)
      .output()
      .map_err(|e| CompilerError::VersionDetectionFailed(e.to_string()))?;

    let output_str = String::from_utf8_lossy(&output.stdout);
    self.version = self.parse_version(&output_str);

    if let Some(ref version) = self.version {
      debug!("Detected compiler version: {version}");
    }

    Ok(())
  }

  fn parse_version(&self, output: &str) -> Option<CompilerVersion> {
    match self.kind {
      CompilerKind::Gcc => {
        for line in output.lines() {
          if line.contains("gcc") {
            if let Some(version_part) = line.split_whitespace().last() {
              return self.parse_version_string(version_part, output);
            }
          }
        }
      }
      CompilerKind::Clang => {
        for line in output.lines() {
          if line.contains("clang version") {
            if let Some(version_part) = line.split("version ").nth(1) {
              let version_only =
                version_part.split_whitespace().next().unwrap_or("");
              return self.parse_version_string(version_only, output);
            }
          }
        }
      }
      CompilerKind::Msvc => {
        return None;
      }
    }
    None
  }

  fn parse_version_string(
    &self,
    version_str: &str,
    raw_output: &str,
  ) -> Option<CompilerVersion> {
    let clean_version = version_str.split('-').next()?.split('+').next()?;
    let parts: Vec<&str> = clean_version.split('.').collect();

    if parts.len() >= 2 {
      let major = parts[0].parse().ok()?;
      let minor = parts[1].parse().ok()?;
      let patch = parts.get(2).and_then(|p| p.parse().ok());

      Some(CompilerVersion {
        major,
        minor,
        patch,
        raw: raw_output.to_owned(),
      })
    } else {
      None
    }
  }

  pub fn create_basic_command(&self) -> Command {
    Command::new(&self.path)
  }

  pub fn create_c_command(&self) -> Command {
    let mut cmd = self.create_basic_command();

    if self.kind == CompilerKind::Msvc {
      cmd.arg("/nologo");
      cmd.arg("/TC");
      cmd.arg("/std:c17");
    } else {
      cmd.arg("-std=c17");
      cmd.arg("-Wall");
      cmd.arg("-Wextra");

      if self.kind == CompilerKind::Gcc {
        cmd.arg("-Wformat=2");
        cmd.arg("-Wformat-security");
        cmd.arg("-Wnull-dereference");
      }
    }

    cmd
  }

  pub fn supports_c_standard(&self, standard: &str) -> bool {
    match self.kind {
      CompilerKind::Msvc => {
        matches!(standard, "c89" | "c90" | "c11" | "c17")
      }
      CompilerKind::Gcc => {
        matches!(
          standard,
          "c89" | "c90" | "c99" | "c11" | "c17" | "c18" | "c23"
        )
      }
      CompilerKind::Clang => {
        matches!(
          standard,
          "c89" | "c90" | "c99" | "c11" | "c17" | "c18" | "c23"
        )
      }
    }
  }

  pub fn optimization_flags(&self, debug: bool) -> Vec<&str> {
    match self.kind {
      CompilerKind::Msvc => {
        if debug {
          vec!["/Od", "/Zi", "/RTC1"]
        } else {
          vec!["/O2", "/DNDEBUG"]
        }
      }
      _ => {
        if debug {
          vec!["-O0", "-g", "-DDEBUG"]
        } else {
          vec!["-O3", "-DNDEBUG"]
        }
      }
    }
  }
}
