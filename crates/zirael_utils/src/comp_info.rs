use crate::mode::Mode;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CompilationInfo {
    pub mode: Mode,
    pub root: PathBuf,
    pub name: String,
    pub write_to: PathBuf,
}
