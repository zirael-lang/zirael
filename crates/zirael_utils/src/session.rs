use crate::enums::mode::Mode;
use crate::prelude::PackageType;
use crate::project_config::CheckConfig;
use clap::builder::Str;
use std::path::PathBuf;

#[derive(Debug, Clone)]
/// Struct that holds information about current package
pub struct Session {
  config: CheckConfig,
}

impl Session {
  pub fn new(config: CheckConfig) -> Self {
    Self { config }
  }
  
  pub fn config(&self) -> &CheckConfig {
    &self.config
  }
}
