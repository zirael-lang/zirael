use crate::sources::Sources;
use parking_lot::RwLock;
use std::{path::PathBuf, sync::Arc};

/// Main compiler struct. It holds data that is read and written throughout the whole compilation process.
#[derive(Clone, Default)]
pub struct Context {
    sources: Sources,
}

impl Context {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Context {
    pub fn sources(&self) -> &Sources {
        &self.sources
    }
}

#[test]
fn test_context() {
    let ctx = Context::new();
    let id = ctx.sources().add_static("test", Some(PathBuf::from("test.rs")));

    assert!(ctx.sources().contains(id));
}
