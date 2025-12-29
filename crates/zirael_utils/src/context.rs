use crate::prelude::*;
use zirael_source::arena::sources::Sources;

#[derive(Debug)]
pub struct Context<'ctx> {
  pub session: &'ctx Session,
}

impl<'ctx> Context<'ctx> {
  pub fn new(session: &'ctx Session) -> Self {
    Context { session }
  }
}
