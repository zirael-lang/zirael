use crate::prelude::DiagnosticCtx;
use crate::project_config::CheckConfig;

#[derive(Debug)]
/// Struct that holds information about current package
pub struct Session {
  config: CheckConfig,
  dcx: DiagnosticCtx,
}

impl Session {
  pub fn new(config: CheckConfig) -> Self {
    Self { config, dcx: DiagnosticCtx::default() }
  }

  pub fn config(&self) -> &CheckConfig {
    &self.config
  }

  pub fn dcx(&self) -> &DiagnosticCtx {
    &self.dcx
  }
}
