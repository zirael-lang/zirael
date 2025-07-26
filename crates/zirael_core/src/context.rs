use parking_lot::RwLock;
use std::{path::PathBuf, sync::Arc};
use zirael_utils::{prelude::Reports, sources::Sources};

/// Main compiler struct. It holds data that is read and written throughout the whole compilation process.
#[derive(Clone, Default, Debug)]
pub struct Context<'reports> {
    sources: Sources,
    reports: Reports<'reports>,
}

impl<'reports> Context<'reports> {
    pub fn new() -> Self {
        Default::default()
    }
}

impl<'reports> Context<'reports> {
    pub fn sources(&self) -> Sources {
        // this is cheap to clone because of using Arc
        self.sources.clone()
    }

    pub fn reports(&self) -> Reports<'reports> {
        self.reports.clone()
    }
}

#[test]
fn test_context() {
    let ctx = Context::new();
    let id = ctx.sources().add_static("test", Some(PathBuf::from("test.rs")));

    assert!(ctx.sources().contains(id));
}
