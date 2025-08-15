use anyhow::bail;
use std::{path::PathBuf, str::FromStr};
use std::sync::Arc;
use zirael_utils::prelude::RwLock;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Dependency {
    pub name: String,
    pub root: PathBuf,
    pub entrypoint: PathBuf,
}

impl Dependency {
    pub fn new(name: String, entrypoint: PathBuf, root: PathBuf) -> Self {
        Self { name, entrypoint, root }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn entrypoint(&self) -> &PathBuf {
        &self.entrypoint
    }

    pub fn root(&self) -> &PathBuf {
        &self.root
    }
}

impl FromStr for Dependency {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self, Self::Err> {
        let (name, rest) = match s.split_once(':') {
            Some((n, r)) => (n, r),
            None => bail!("Invalid dependency format. Expected 'name:root=entrypoint', got '{}'", s),
        };

        let (root, entrypoint) = match rest.split_once('=') {
            Some((r, e)) => (r, e),
            None => bail!("Invalid dependency format. Expected 'name:root=entrypoint', got '{}'", s),
        };

        Ok(Dependency {
            name: name.to_string(),
            root: PathBuf::from(root),
            entrypoint: PathBuf::from(entrypoint),
        })
    }
}


#[derive(Debug, Clone, Default)]
pub struct Dependencies(Arc<RwLock<Vec<Dependency>>>);

impl Dependencies {
    pub fn new(dependencies: Vec<Dependency>) -> Self {
        Self(Arc::new(RwLock::new(dependencies)))
    }

    fn read<R>(&self, reader: impl FnOnce(&Vec<Dependency>) -> R) -> R {
        reader(&self.0.read())
    }

    fn write<R>(&self, writer: impl FnOnce(&mut Vec<Dependency>) -> R) -> R {
        writer(&mut self.0.write())
    }

    pub fn get(&self, name: &str) -> Option<Dependency> {
        self.read(|deps| deps.iter().find(|dep| dep.name == name).cloned())
    }

    pub fn add(&self, dependency: Dependency) {
        self.write(|deps| deps.push(dependency))
    }

    pub fn all(&self) -> Vec<Dependency> {
        self.read(|deps| deps.clone())
    }

    pub fn contains(&self, dependency: &Dependency) -> bool {
        self.read(|deps| deps.iter().any(|dep| dep.name == dependency.name))
    }
}
