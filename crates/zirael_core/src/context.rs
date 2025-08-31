use zirael_parser::{Dependencies, SymbolTable};
use zirael_utils::prelude::*;

/// Main compiler struct. It holds data that is read and written throughout the whole compilation process.
#[derive(Clone, Default, Debug)]
pub struct Context<'reports> {
  sources: Sources,
  reports: Reports<'reports>,
  packages: Dependencies,
  symbols: SymbolTable,
}

impl<'reports> Context<'reports> {
  pub fn new() -> Self {
    Default::default()
  }
}

impl<'reports> Context<'reports> {
  pub fn sources(&self) -> &Sources {
    // this is cheap to clone because of using Arc
    &self.sources
  }

  pub fn reports(&self) -> &Reports<'reports> {
    &self.reports
  }

  pub fn packages(&self) -> &Dependencies {
    &self.packages
  }

  pub fn symbols(&self) -> &SymbolTable {
    &self.symbols
  }
}
