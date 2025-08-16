use anyhow::Result;
use std::path::{Path, PathBuf};

/// strips the same root from a path using `reference_path` as the common base
pub fn strip_same_root(path: PathBuf, reference_path: PathBuf) -> PathBuf {
    let path_components: Vec<_> = path.components().collect();
    let reference_components: Vec<_> = reference_path.components().collect();

    let mut common_length = 0;

    for (path_comp, ref_comp) in path_components.iter().zip(reference_components.iter()) {
        if path_comp == ref_comp {
            common_length += 1;
        } else {
            break;
        }
    }

    let stripped_path: PathBuf = path_components.iter().skip(common_length).collect();

    stripped_path
}

pub fn canonicalize_with_strip<P: AsRef<Path>>(path: P) -> Result<PathBuf> {
    let canonical = fs_err::canonicalize(path)?;
    Ok(strip_windows_long_path_prefix(canonical))
}

/// Strips the Windows long path prefix `\\?\` from a path if present.
///
/// On Windows, paths longer than 260 characters are prefixed with `\\?\` to bypass
/// the `MAX_PATH` limitation. This function removes that prefix to make paths more readable.
fn strip_windows_long_path_prefix(path: PathBuf) -> PathBuf {
    #[cfg(windows)]
    {
        let path_str = path.to_string_lossy();

        if let Some(stripped) = path_str.strip_prefix(r"\\?\") {
            PathBuf::from(stripped)
        } else {
            path
        }
    }

    #[cfg(not(windows))]
    {
        // On non-Windows platforms, just return the path as-is
        path
    }
}
