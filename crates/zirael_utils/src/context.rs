use crate::prelude::*;

#[derive(Debug)]
pub struct Context<'ctx> {
  sources: Sources,

  pub session: &'ctx Session,
}

impl<'ctx> Context<'ctx> {
  pub fn new(session: &'ctx Session) -> Self {
    Context { sources: Default::default(), session }
  }

  pub fn sources(&'ctx self) -> &'ctx Sources {
    &self.sources
  }
}
