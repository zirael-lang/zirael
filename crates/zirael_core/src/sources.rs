use crate::Context;
use id_arena::{Arena, Id};
use parking_lot::RwLock;
use std::{borrow::Cow, path::PathBuf, sync::Arc};

pub type SourceFileId = Id<SourceFile>;

#[derive(Debug, Clone)]
pub struct SourceFile {
    content: Cow<'static, str>,
    path: Option<PathBuf>,
}

impl SourceFile {
    pub fn new_static(content: &'static str, path: Option<PathBuf>) -> Self {
        Self { content: Cow::Borrowed(content), path }
    }

    pub fn new_owned(content: String, path: Option<PathBuf>) -> Self {
        Self { content: Cow::Owned(content), path }
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }
}

pub type SourcesImpl = Arena<SourceFile>;

/// This struct handles all sources (files) used in the compilation process.
#[derive(Debug, Clone, Default)]
pub struct Sources(Arc<RwLock<SourcesImpl>>);

impl Sources {
    pub fn new() -> Self {
        Default::default()
    }

    fn read<R>(&self, reader: impl FnOnce(&SourcesImpl) -> R) -> R {
        reader(&self.0.read())
    }

    fn write<R>(&self, writer: impl FnOnce(&mut SourcesImpl) -> R) -> R {
        writer(&mut self.0.write())
    }

    pub fn add(&self, source: SourceFile) -> SourceFileId {
        self.write(|arena| arena.alloc(source))
    }

    pub fn get(&self, id: SourceFileId) -> Option<SourceFile> {
        self.read(|arena| arena.get(id).cloned())
    }

    pub fn get_unchecked(&self, id: SourceFileId) -> SourceFile {
        self.read(|arena| arena[id].clone())
    }

    pub fn contains(&self, id: SourceFileId) -> bool {
        self.read(|arena| arena.get(id).is_some())
    }

    pub fn len(&self) -> usize {
        self.read(|arena| arena.len())
    }

    pub fn iter(&self) -> Vec<(SourceFileId, SourceFile)> {
        self.read(|arena| arena.iter().map(|(id, source)| (id, source.clone())).collect())
    }

    pub fn add_static(&self, content: &'static str, path: Option<PathBuf>) -> SourceFileId {
        self.add(SourceFile::new_static(content, path))
    }

    pub fn add_owned(&self, content: String, path: Option<PathBuf>) -> SourceFileId {
        self.add(SourceFile::new_owned(content, path))
    }
}

#[test]
fn test_sources() {
    let sources = Sources::new();
    let id = sources.add_static("hello", None);
    assert_eq!(sources.get_unchecked(id).content(), "hello");
}
