mod context;
mod unit;

pub mod vars {
    pub const FILE_EXTENSION: &str = "zr";
}

#[expect(unused_imports)]
pub mod prelude {
    pub use crate::{context::*, unit::*, vars::*};
    pub use zirael_parser::*;
    pub use zirael_utils::{prelude::*, *};
}
