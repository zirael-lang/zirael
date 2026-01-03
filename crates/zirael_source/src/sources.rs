use crate::source_file::{SourceFile, SourceFileId};
use dashmap::DashMap;
use dashmap::iter::Iter;
use dashmap::mapref::one::Ref;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

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

  pub fn add(&self, input: String, path: PathBuf) -> SourceFileId {
    if let Some(existing_id) = self.get_by_path(&path) {
      return *existing_id.value();
    }

    let id =
      SourceFileId(self.inner.last_id.fetch_add(1, Ordering::Relaxed) + 1);
    self
      .inner
      .sources
      .insert(id, SourceFile::new(input, path.clone(), id));
    self.inner.path_to_id.insert(path, id);
    id
  }

  pub fn get_by_path(
    &self,
    path: &PathBuf,
  ) -> Option<Ref<'_, PathBuf, SourceFileId>> {
    self.inner.path_to_id.get(path)
  }

  pub fn get(
    &self,
    id: SourceFileId,
  ) -> Option<Ref<'_, SourceFileId, SourceFile>> {
    self.inner.sources.get(&id)
  }

  pub fn get_unchecked(
    &self,
    id: SourceFileId,
  ) -> Ref<'_, SourceFileId, SourceFile> {
    self.inner.sources.get(&id).unwrap()
  }

  pub fn all(&self) -> Iter<'_, SourceFileId, SourceFile> {
    self.inner.sources.iter()
  }

  pub fn display(&self, id: SourceFileId) -> Option<String> {
    self.get(id).map(|s| s.path().display().to_string())
  }
}
