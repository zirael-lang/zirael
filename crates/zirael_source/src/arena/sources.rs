use ariadne::Source;
use dashmap::DashMap;
use dashmap::iter::Iter;
use dashmap::mapref::one::Ref;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::path::PathBuf;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceFileId(pub usize);

#[derive(Debug, Clone)]
pub struct SourceFile {
  content: Source<String>,
  path: PathBuf,
}

impl SourceFile {
  pub fn new(content: String, path: PathBuf) -> Self {
    Self { content: Source::from(content), path }
  }

  pub fn content(&self) -> &Source {
    &self.content
  }

  pub fn path(&self) -> PathBuf {
    self.path.clone()
  }
}

impl AsRef<str> for SourceFile {
  fn as_ref(&self) -> &str {
    self.content.text()
  }
}

#[derive(Debug, Default)]
pub struct SourcesImpl {
  pub sources: DashMap<SourceFileId, SourceFile>,
  pub path_to_id: DashMap<PathBuf, SourceFileId>,
  pub last_id: AtomicUsize,
}

/// This struct handles all sources (files) used in the compilation process.
#[derive(Debug, Default)]
pub struct Sources {
  inner: SourcesImpl,
}

impl Sources {
  pub fn new() -> Self {
    Default::default()
  }

  pub fn add(&self, source: SourceFile) -> SourceFileId {
    let path = source.path.clone();
    if let Some(existing_id) = self.get_by_path(&source.path) {
      return existing_id.value().clone();
    }

    let id = SourceFileId(self.inner.last_id.fetch_add(1, Ordering::Relaxed) + 1);
    self.inner.sources.insert(id, source);
    self.inner.path_to_id.insert(path, id);
    id
  }

  pub fn get_by_path(&self, path: &PathBuf) -> Option<Ref<'_, PathBuf, SourceFileId>> {
    self.inner.path_to_id.get(path)
  }

  pub fn get(&self, id: SourceFileId) -> Option<Ref<'_, SourceFileId, SourceFile>> {
    self.inner.sources.get(&id)
  }

  pub fn get_unchecked(&self, id: SourceFileId) -> Ref<'_, SourceFileId, SourceFile> {
    self.inner.sources.get(&id).unwrap()
  }

  pub fn all(&self) -> Iter<'_, SourceFileId, SourceFile> {
    self.inner.sources.iter()
  }
}
