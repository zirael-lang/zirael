use zirael_parser::ast::types::{Mutability, PrimitiveKind};
use zirael_resolver::DefId;
use zirael_utils::ident_table::Identifier;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(pub u32);

impl TyVar {
  pub fn new(id: u32) -> Self {
    Self(id)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyVarKind {
  General,
  Integer,
  Float,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferTy {
  Var(TyVar),
  Primitive(PrimitiveKind),
  Adt {
    def_id: DefId,
    args: Vec<InferTy>,
  },
  Ptr {
    mutability: Mutability,
    ty: Box<InferTy>,
  },
  Optional(Box<InferTy>),
  Array {
    ty: Box<InferTy>,
    len: usize,
  },
  Slice(Box<InferTy>),
  Tuple(Vec<InferTy>),
  Fn {
    params: Vec<InferTy>,
    ret: Box<InferTy>,
  },
  Unit,
  Never,
  Param {
    def_id: DefId,
    name: Identifier,
  },
  Err,
}

impl InferTy {
  pub fn has_vars(&self) -> bool {
    match self {
      InferTy::Var(_) => true,
      InferTy::Primitive(_) => false,
      InferTy::Adt { args, .. } => args.iter().any(|t| t.has_vars()),
      InferTy::Ptr { ty, .. } => ty.has_vars(),
      InferTy::Optional(ty) => ty.has_vars(),
      InferTy::Array { ty, .. } => ty.has_vars(),
      InferTy::Slice(ty) => ty.has_vars(),
      InferTy::Tuple(tys) => tys.iter().any(|t| t.has_vars()),
      InferTy::Fn { params, ret } => {
        params.iter().any(|t| t.has_vars()) || ret.has_vars()
      }
      InferTy::Unit | InferTy::Never | InferTy::Param { .. } | InferTy::Err => {
        false
      }
    }
  }

  pub fn is_err(&self) -> bool {
    matches!(self, InferTy::Err)
  }

  pub fn is_var(&self) -> bool {
    matches!(self, InferTy::Var(_))
  }

  pub fn is_numeric(&self) -> bool {
    match self {
      InferTy::Primitive(p) => p.is_numeric(),
      _ => false,
    }
  }

  pub fn is_integer(&self) -> bool {
    match self {
      InferTy::Primitive(p) => p.is_integer(),
      _ => false,
    }
  }

  pub fn is_float(&self) -> bool {
    match self {
      InferTy::Primitive(p) => p.is_float(),
      _ => false,
    }
  }

  pub fn is_bool(&self) -> bool {
    matches!(self, InferTy::Primitive(PrimitiveKind::Bool))
  }

  pub fn is_unit(&self) -> bool {
    matches!(self, InferTy::Unit)
  }

  pub fn is_never(&self) -> bool {
    matches!(self, InferTy::Never)
  }

  pub fn bool() -> Self {
    InferTy::Primitive(PrimitiveKind::Bool)
  }
}

#[derive(Debug, Clone)]
pub struct TypeScheme {
  pub vars: Vec<TyVar>,
  pub ty: InferTy,
}

impl TypeScheme {
  pub fn mono(ty: InferTy) -> Self {
    Self { vars: vec![], ty }
  }

  pub fn is_mono(&self) -> bool {
    self.vars.is_empty()
  }
}

#[derive(Debug, Clone)]
pub enum UnifyResult {
  Ok,
  Err(UnifyError),
}

impl UnifyResult {
  pub fn is_ok(&self) -> bool {
    matches!(self, UnifyResult::Ok)
  }

  pub fn is_err(&self) -> bool {
    matches!(self, UnifyResult::Err(_))
  }
}

#[derive(Debug, Clone)]
pub enum UnifyError {
  /// Two concrete types don't match.
  Mismatch { expected: InferTy, found: InferTy },

  /// Occurs check failed (infinite type).
  OccursCheck { var: TyVar, ty: InferTy },

  /// Arity mismatch (e.g., tuple or function parameter count).
  ArityMismatch { expected: usize, found: usize },

  /// Integer type variable couldn't unify with a non-integer type.
  NotAnInteger { ty: InferTy },

  /// Float type variable couldn't unify with a non-float type.
  NotAFloat { ty: InferTy },

  /// Mutability mismatch for pointers.
  MutabilityMismatch {
    expected: Mutability,
    found: Mutability,
  },

  /// Array length mismatch.
  ArrayLenMismatch { expected: usize, found: usize },

  /// Incompatible type variable kinds (e.g., Integer vs Float).
  IncompatibleKinds { kind1: TyVarKind, kind2: TyVarKind },
}

#[derive(Debug, Clone)]
pub enum Expectation {
  /// Infer freely.
  None,
  /// A specific type is expected.
  ExpectHasType(InferTy),
  /// A coercible type is acceptable.
  Coercion(InferTy),
}

impl Expectation {
  pub fn none() -> Self {
    Expectation::None
  }

  pub fn has_type(ty: InferTy) -> Self {
    Expectation::ExpectHasType(ty)
  }

  pub fn coercion(ty: InferTy) -> Self {
    Expectation::Coercion(ty)
  }

  pub fn to_option(&self) -> Option<&InferTy> {
    match self {
      Expectation::None => None,
      Expectation::ExpectHasType(ty) | Expectation::Coercion(ty) => Some(ty),
    }
  }

  pub fn is_none(&self) -> bool {
    matches!(self, Expectation::None)
  }
}
