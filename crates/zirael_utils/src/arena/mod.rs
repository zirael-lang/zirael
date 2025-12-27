use generational_arena::Index;
use parking_lot::RwLock;
use std::sync::Arc;

pub mod sources;

pub trait ArenaId {
  fn new(index: Index) -> Self;
  fn index(&self) -> Index;
}

pub trait GenArena<Item> {
  fn arena(&self) -> &generational_arena::Arena<Item>;
}

pub trait ArenaExt<ArenaImpl: GenArena<Item>, Item: Clone, ID: ArenaId> {
  fn lock(&self) -> &Arc<RwLock<ArenaImpl>>;

  fn read<R>(&self, reader: impl FnOnce(&ArenaImpl) -> R) -> R {
    reader(&self.lock().read())
  }

  fn write<R>(&self, writer: impl FnOnce(&mut ArenaImpl) -> R) -> R {
    writer(&mut self.lock().write())
  }

  fn get(&self, id: ID) -> Option<Item> {
    self.read(|sources| sources.arena().get(id.index()).cloned())
  }

  fn get_unchecked(&self, id: ID) -> Item {
    self.read(|sources| sources.arena()[id.index()].clone())
  }

  fn contains(&self, id: ID) -> bool {
    self.read(|sources| sources.arena().get(id.index()).is_some())
  }

  fn all(&self) -> Vec<(Index, Item)> {
    self.read(|sources| {
      sources.arena().iter().map(|(id, source)| (id.clone(), source.clone())).collect()
    })
  }
}
