use crate::prelude::*;
use std::sync::Arc;
use zirael_diagnostics::DiagnosticCtx;
use zirael_source::sources::Sources;

pub struct Context<'ctx> {
  pub session: &'ctx Session,
  pub sources: Arc<Sources>,
}

impl<'ctx> Context<'ctx> {
  pub fn new(session: &'ctx Session, sources: Arc<Sources>) -> Self {
    Context { session, sources }
  }

  pub fn dcx(&self) -> &DiagnosticCtx {
    self.session.dcx()
  }
}
