use crate::project_config::ProjectConfig;
use std::sync::Arc;
use zirael_diagnostics::DiagnosticCtx;
use zirael_source::prelude::Sources;

#[derive(Debug)]
/// Struct that holds information about current package
pub struct Session {
  config: ProjectConfig,
  dcx: DiagnosticCtx,
}

impl Session {
  pub fn new(config: ProjectConfig, sources: Arc<Sources>) -> Self {
    Self {
      dcx: DiagnosticCtx::new(sources, config.color.clone(), config.diagnostic_output_type.clone()),
      config,
    }
  }

  pub fn config(&self) -> &ProjectConfig {
    &self.config
  }

  pub fn dcx(&self) -> &DiagnosticCtx {
    &self.dcx
  }
}
