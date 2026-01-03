use crate::compiler::{Compiler, CompilerKind};
use anyhow::{Context as _, Result, anyhow};
use log::{debug, warn};
use std::{
  collections::HashMap,
  env,
  path::Path,
  process::{Command, Stdio},
};

pub fn detect_compiler() -> Result<Compiler> {
  debug!("Detecting C compiler, preferring Clang");

  match detect_clang() {
    Ok(compiler) => {
      debug!("Successfully detected Clang compiler");
      return Ok(compiler);
    }
    Err(e) => {
      debug!("Clang detection failed: {e}");
    }
  }

  #[cfg(target_os = "windows")]
  {
    match detect_msvc_compiler() {
      Ok(compiler) => {
        debug!("Successfully detected MSVC compiler as fallback");
        return Ok(compiler);
      }
      Err(e) => {
        warn!("MSVC compiler not available: {}", e);
      }
    }
  }

  match detect_gcc() {
    Ok(compiler) => {
      debug!("Successfully detected GCC compiler as fallback");
      Ok(compiler)
    }
    Err(e) => Err(anyhow!(
      "No suitable C compiler found. Tried Clang, {}, and GCC. Last error: {}",
      if cfg!(windows) {
        "MSVC"
      } else {
        "system compilers"
      },
      e
    )),
  }
}

fn detect_clang() -> Result<Compiler> {
  debug!("Looking for Clang compiler");

  if let Ok(path) = which::which("clang") {
    debug!("Found Clang at: {}", path.display());
    return Compiler::new(path, CompilerKind::Clang)
      .context("Failed to initialize Clang compiler");
  }

  Err(anyhow!("Clang compiler not found in PATH"))
}

fn detect_gcc() -> Result<Compiler> {
  debug!("Looking for GCC compiler");

  if let Ok(path) = which::which("gcc") {
    debug!("Found GCC at: {}", path.display());
    return Compiler::new(path, CompilerKind::Gcc)
      .context("Failed to initialize GCC compiler");
  }

  Err(anyhow!("GCC compiler not found in PATH"))
}

#[cfg(target_os = "windows")]
fn detect_msvc_compiler() -> Result<Compiler> {
  let msvc_paths = vec![
    "2022/Community/VC/Auxiliary/Build/vcvars64.bat",
    "2022/Professional/VC/Auxiliary/Build/vcvars64.bat",
    "2022/Enterprise/VC/Auxiliary/Build/vcvars64.bat",
    "2019/Community/VC/Auxiliary/Build/vcvars64.bat",
    "2019/Professional/VC/Auxiliary/Build/vcvars64.bat",
    "2019/Enterprise/VC/Auxiliary/Build/vcvars64.bat",
    // Fallback to 32-bit
    "2022/Community/VC/Auxiliary/Build/vcvars32.bat",
    "2019/Community/VC/Auxiliary/Build/vcvars32.bat",
  ];

  let program_files = env::var("ProgramFiles")
    .or_else(|_| env::var("ProgramFiles(x86)"))
    .context("Could not find Program Files directory")?;
  let vs_base = Path::new(&program_files).join("Microsoft Visual Studio");

  if !vs_base.exists() {
    return Err(anyhow!(
      "Microsoft Visual Studio installation directory not found at: {}",
      vs_base.display()
    ));
  }

  for path in &msvc_paths {
    let full_path = vs_base.join(path);
    if full_path.exists() {
      debug!("Found MSVC installation at: {}", full_path.display());

      match setup_msvc_environment(&full_path) {
        Ok(env_vars) => {
          apply_env_vars(&env_vars);

          match which::which("cl") {
            Ok(cl_path) => {
              return Compiler::new(cl_path, CompilerKind::Msvc)
                .context("Failed to initialize MSVC compiler");
            }
            Err(e) => {
              return Err(anyhow!(
                "cl.exe not found in PATH after setting up MSVC environment: {}",
                e
              ));
            }
          }
        }
        Err(e) => {
          warn!(
            "Failed to setup MSVC environment for {}: {}",
            full_path.display(),
            e
          );
          continue;
        }
      }
    }
  }

  Err(anyhow!(
    "No working MSVC installation found. Checked paths: {:?}",
    msvc_paths
  ))
}

#[cfg(not(target_os = "windows"))]
fn detect_msvc_compiler() -> Result<Compiler> {
  Err(anyhow!("MSVC compiler not available on this platform"))
}

fn setup_msvc_environment(
  vcvars_path: &Path,
) -> Result<HashMap<String, String>> {
  debug!(
    "Setting up MSVC environment using: {}",
    vcvars_path.display()
  );
  let arch = if vcvars_path.to_string_lossy().contains("vcvars64") {
    "x64"
  } else {
    "x86"
  };

  let cmd_line = format!("\"{}\" {} && set", vcvars_path.display(), arch);
  debug!("Executing MSVC setup command: {cmd_line}");

  let output = Command::new("cmd")
    .args(["/C", &cmd_line])
    .stdout(Stdio::piped())
    .stderr(Stdio::piped())
    .output()
    .context("Failed to execute vcvars script")?;

  debug!(
    "MSVC setup stdout: {}",
    String::from_utf8_lossy(&output.stdout)
  );
  debug!(
    "MSVC setup stderr: {}",
    String::from_utf8_lossy(&output.stderr)
  );

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(anyhow!("vcvars script failed with error: {}", stderr));
  }

  let mut env_vars = HashMap::new();
  let output_str = String::from_utf8_lossy(&output.stdout);

  for line in output_str.lines() {
    if let Some((key, value)) = line.split_once('=') {
      if !value.is_empty() && !key.starts_with("PS1") && !key.starts_with('_') {
        env_vars.insert(key.to_owned(), value.to_owned());
      }
    }
  }

  if env_vars.is_empty() {
    return Err(anyhow!(
      "No environment variables were set by vcvars script"
    ));
  }

  debug!(
    "Successfully captured {} environment variables from MSVC setup",
    env_vars.len()
  );
  Ok(env_vars)
}

fn apply_env_vars(env_vars: &HashMap<String, String>) {
  for (key, value) in env_vars {
    #[expect(unsafe_code)]
    unsafe {
      env::set_var(key, value)
    };
  }
  debug!("Applied {} environment variables", env_vars.len());
}

pub fn resolve_from_kind(kind: CompilerKind) -> Result<Compiler> {
  debug!("Resolving compiler of kind: {kind:?}");

  match kind {
    CompilerKind::Clang => detect_clang(),
    CompilerKind::Gcc => detect_gcc(),
    CompilerKind::Msvc => {
      #[cfg(target_os = "windows")]
      return detect_msvc_compiler();

      #[cfg(not(target_os = "windows"))]
      return Err(anyhow!("MSVC compiler is only available on Windows"));
    }
  }
}

pub fn detect_all_compilers() -> Vec<Compiler> {
  let mut compilers = Vec::new();

  let compiler_kinds = if cfg!(windows) {
    vec![CompilerKind::Clang, CompilerKind::Msvc, CompilerKind::Gcc]
  } else {
    vec![CompilerKind::Clang, CompilerKind::Gcc]
  };

  for kind in compiler_kinds {
    if let Ok(compiler) = resolve_from_kind(kind) {
      compilers.push(compiler);
    }
  }

  debug!("Detected {} compilers total", compilers.len());
  compilers
}

pub fn get_best_compiler() -> Result<Compiler> {
  detect_compiler().context("No suitable C compiler found on this system")
}
