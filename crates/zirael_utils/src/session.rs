use crate::project_config::ProjectConfig;
use std::sync::Arc;
use zirael_diagnostics::{DiagnosticCtx, DiagnosticWriter};
use zirael_source::prelude::Sources;

/// Struct that holds information about current package
pub struct Session {
  config: ProjectConfig,
  dcx: DiagnosticCtx,

  is_test: bool,
}

impl Session {
  pub fn new(
    config: ProjectConfig,
    sources: Arc<Sources>,
    w: DiagnosticWriter,
    is_test: bool,
  ) -> Self {
    Self {
      dcx: DiagnosticCtx::new(
        sources.clone(),
        config.color,
        config.diagnostic_output_type.clone(),
        w,
      ),
      config,
      is_test,
    }
  }

  pub fn config(&self) -> &ProjectConfig {
    &self.config
  }

  pub fn dcx(&self) -> &DiagnosticCtx {
    &self.dcx
  }
  
  pub fn is_test(&self) -> bool {
    self.is_test
  }
}
