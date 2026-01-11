use zirael_resolver::DefId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirId {
  pub owner: DefId,
  pub local_id: LocalId,
}

impl HirId {
  pub fn new(owner: DefId, local_id: LocalId) -> Self {
    Self { owner, local_id }
  }

  pub fn make_owner(owner: DefId) -> Self {
    Self {
      owner,
      local_id: LocalId::ZERO,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct LocalId(pub u32);

impl LocalId {
  pub const ZERO: Self = Self(0);

  pub fn next(&self) -> Self {
    Self(self.0 + 1)
  }
}
