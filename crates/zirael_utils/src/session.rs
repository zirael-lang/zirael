use crate::project_config::ProjectConfig;
use parking_lot::Mutex;
use std::io::Write;
use std::sync::Arc;
use zirael_diagnostics::{DiagnosticCtx, DiagnosticWriter};
use zirael_source::prelude::Sources;

/// Struct that holds information about current package
pub struct Session {
  config: ProjectConfig,
  dcx: DiagnosticCtx,
}

impl Session {
  pub fn new(
    config: ProjectConfig,
    sources: Arc<Sources>,
    w: DiagnosticWriter,
  ) -> Self {
    Self {
      dcx: DiagnosticCtx::new(
        sources.clone(),
        config.color.clone(),
        config.diagnostic_output_type.clone(),
        w,
      ),
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
