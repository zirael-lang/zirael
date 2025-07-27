use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Dependency {
    name: String,
    path: PathBuf,
}

impl Dependency {
    pub fn new(name: String, path: PathBuf) -> Self {
        Self { name, path }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }
}
