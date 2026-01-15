mod checking;
mod table;
mod ty;
mod unify;

pub use checking::{TyChecker, typeck_hir};
pub use table::{Constraint, ConstraintKind, InferCtx, TypeEnv, TypeTable};
pub use ty::{
  Expectation, InferTy, TyVar, TyVarKind, TypeScheme, UnifyError, UnifyResult,
};
