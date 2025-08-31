use crate::{MonomorphizationId, Type};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonomorphizedSymbol {
  pub id: MonomorphizationId,
  pub display_ty: Box<Type>,
}
