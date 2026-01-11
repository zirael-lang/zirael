use crate::hir::Hir;
use crate::ids::{HirId, LocalId};
use zirael_parser::NodeId;
use zirael_resolver::{DefId, Resolver};
use zirael_source::prelude::SourceFileId;

pub struct LoweringContext<'a> {
  pub resolver: &'a Resolver,
  /// The HIR being built.
  pub hir: &'a Hir,
  pub source_file: SourceFileId,
  pub current_owner: Option<DefId>,
  next_local_id: LocalId,
  /// Items defined in this module.
  pub module_items: Vec<DefId>,
}

impl<'a> LoweringContext<'a> {
  pub fn new(
    resolver: &'a Resolver,
    hir: &'a Hir,
    source_file: SourceFileId,
  ) -> Self {
    Self {
      resolver,
      hir,
      source_file,
      current_owner: None,
      next_local_id: LocalId::ZERO,
      module_items: Vec::new(),
    }
  }

  /// Generate a new `HirId` within the current owner.
  pub fn next_hir_id(&mut self) -> HirId {
    let owner = self.current_owner.expect("no current owner set");
    let local_id = self.next_local_id;
    self.next_local_id = local_id.next();
    HirId::new(owner, local_id)
  }

  pub fn owner_hir_id(&self, def_id: DefId) -> HirId {
    HirId::make_owner(def_id)
  }

  pub fn with_owner<T>(
    &mut self,
    owner: DefId,
    f: impl FnOnce(&mut Self) -> T,
  ) -> T {
    let prev_owner = self.current_owner;
    let prev_local_id = self.next_local_id;

    self.current_owner = Some(owner);
    self.next_local_id = LocalId(1);

    let result = f(self);

    self.current_owner = prev_owner;
    self.next_local_id = prev_local_id;

    result
  }

  pub fn get_def_id(&self, node_id: NodeId) -> Option<DefId> {
    self.resolver.symbols.get_resolution(node_id)
  }

  pub fn finalize_module(&self) {
    self
      .hir
      .modules
      .insert(self.source_file, self.module_items.clone());
  }
}
