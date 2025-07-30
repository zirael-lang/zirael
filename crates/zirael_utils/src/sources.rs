use id_arena::{Arena, Id};
use parking_lot::RwLock;
use std::{borrow::Cow, collections::HashMap, path::PathBuf, sync::Arc};

pub type SourceFileId = Id<SourceFile>;

#[derive(Debug, Clone)]
pub struct SourceFile {
    content: Cow<'static, str>,
    path: PathBuf,
}

impl SourceFile {
    pub fn new_static(content: &'static str, path: PathBuf) -> Self {
        Self { content: Cow::Borrowed(content), path }
    }

    pub fn new_owned(content: String, path: PathBuf) -> Self {
        Self { content: Cow::Owned(content), path }
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn path(&self) -> PathBuf {
        self.path.clone()
    }
}

#[derive(Debug, Default)]
pub struct SourcesImpl {
    arena: Arena<SourceFile>,
    path_to_id: HashMap<PathBuf, SourceFileId>,
}

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
        self.write(|sources| {
            if let Some(&existing_id) = sources.path_to_id.get(&source.path) {
                return existing_id;
            }

            let id = sources.arena.alloc(source.clone());
            sources.path_to_id.insert(source.path, id);
            id
        })
    }

    pub fn get(&self, id: SourceFileId) -> Option<SourceFile> {
        self.read(|sources| sources.arena.get(id).cloned())
    }

    pub fn get_unchecked(&self, id: SourceFileId) -> SourceFile {
        self.read(|sources| sources.arena[id].clone())
    }

    pub fn contains(&self, id: SourceFileId) -> bool {
        self.read(|sources| sources.arena.get(id).is_some())
    }

    pub fn get_by_path(&self, path: &PathBuf) -> Option<SourceFileId> {
        self.read(|sources| sources.path_to_id.get(path).copied())
    }

    pub fn iter(&self) -> Vec<(SourceFileId, SourceFile)> {
        self.read(|sources| sources.arena.iter().map(|(id, source)| (id, source.clone())).collect())
    }

    pub fn add_static(&self, content: &'static str, path: PathBuf) -> SourceFileId {
        self.add(SourceFile::new_static(content, path))
    }

    pub fn add_owned(&self, content: String, path: PathBuf) -> SourceFileId {
        self.add(SourceFile::new_owned(content, path))
    }
}
