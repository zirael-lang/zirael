use crate::{MonomorphizationId, Type};
use zirael_utils::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonomorphizedSymbol {
    pub id: MonomorphizationId,
    pub display_ty: Box<Type>,
}
