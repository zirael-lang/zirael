use std::collections::HashMap;

use crate::ty::{InferTy, TyVar, TyVarKind, TypeScheme};
use dashmap::DashMap;
use parking_lot::RwLock;
use zirael_hir::HirId;
use zirael_resolver::DefId;
use zirael_utils::prelude::Span;

#[derive(Debug)]
pub struct TypeTable {
  /// Maps HIR nodes to their inferred types
  pub(crate) node_types: DashMap<HirId, InferTy>,

  /// Maps definitions to their type schemes
  pub(crate) def_types: DashMap<DefId, TypeScheme>,

  /// Maps struct fields to their types.
  pub(crate) field_types: DashMap<(DefId, String), InferTy>,

  /// Maps method signatures to thier type scheme.
  pub(crate) method_types: DashMap<(DefId, String), TypeScheme>,
}

impl TypeTable {
  pub fn new() -> Self {
    Self {
      node_types: DashMap::new(),
      def_types: DashMap::new(),
      field_types: DashMap::new(),
      method_types: DashMap::new(),
    }
  }

  pub fn record_node_type(&self, hir_id: HirId, ty: InferTy) {
    self.node_types.insert(hir_id, ty);
  }

  pub fn node_type(&self, hir_id: HirId) -> Option<InferTy> {
    self.node_types.get(&hir_id).map(|t| t.clone())
  }

  pub fn record_def_type(&self, def_id: DefId, scheme: TypeScheme) {
    self.def_types.insert(def_id, scheme);
  }

  pub fn def_type(&self, def_id: DefId) -> Option<TypeScheme> {
    self.def_types.get(&def_id).map(|s| s.clone())
  }

  pub fn record_field_type(
    &self,
    struct_id: DefId,
    field_name: String,
    ty: InferTy,
  ) {
    self.field_types.insert((struct_id, field_name), ty);
  }

  pub fn field_type(
    &self,
    struct_id: DefId,
    field_name: &str,
  ) -> Option<InferTy> {
    self
      .field_types
      .get(&(struct_id, field_name.to_string()))
      .map(|t| t.clone())
  }

  pub fn record_method_type(
    &self,
    struct_id: DefId,
    method_name: String,
    scheme: TypeScheme,
  ) {
    self.method_types.insert((struct_id, method_name), scheme);
  }

  pub fn method_type(
    &self,
    struct_id: DefId,
    method_name: &str,
  ) -> Option<TypeScheme> {
    self
      .method_types
      .get(&(struct_id, method_name.to_string()))
      .map(|s| s.clone())
  }
}

impl Default for TypeTable {
  fn default() -> Self {
    Self::new()
  }
}

#[derive(Debug)]
pub struct InferCtx {
  next_var: RwLock<u32>,

  pub(crate) var_kinds: DashMap<TyVar, TyVarKind>,
  /// maps type variables to their resolved types
  pub(crate) substitution: DashMap<TyVar, InferTy>,
  /// Maps type parameter DefIds to their type variables
  type_params: RwLock<HashMap<DefId, TyVar>>,
  /// Constraint set for delayed unification
  constraints: RwLock<Vec<Constraint>>,
}

/// A type constraint to be solved.
#[derive(Debug, Clone)]
pub struct Constraint {
  pub kind: ConstraintKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ConstraintKind {
  /// Two types must be equal: `T1 = T2`
  Eq(InferTy, InferTy),

  /// A type must be a subtype of another (for coercions)
  Subtype { sub: InferTy, sup: InferTy },

  /// A type must have a specific field
  HasField {
    ty: InferTy,
    field: String,
    field_ty: InferTy,
  },

  /// A type must be callable with given arguments
  Callable {
    callee: InferTy,
    args: Vec<InferTy>,
    ret: InferTy,
  },
}

impl InferCtx {
  pub fn new() -> Self {
    Self {
      next_var: RwLock::new(0),
      var_kinds: DashMap::new(),
      substitution: DashMap::new(),
      type_params: RwLock::new(HashMap::new()),
      constraints: RwLock::new(Vec::new()),
    }
  }

  pub fn clear_type_params(&self) {
    self.type_params.write().clear();
  }

  pub fn get_or_create_type_param(&self, def_id: DefId) -> TyVar {
    {
      let type_params = self.type_params.read();
      if let Some(&var) = type_params.get(&def_id) {
        return var;
      }
    }

    let var = self.fresh_var(TyVarKind::General);
    self.type_params.write().insert(def_id, var);
    var
  }

  pub fn lookup_type_param(&self, def_id: DefId) -> Option<TyVar> {
    self.type_params.read().get(&def_id).copied()
  }

  pub fn fresh_var(&self, kind: TyVarKind) -> TyVar {
    let mut next = self.next_var.write();
    let var = TyVar::new(*next);
    *next += 1;
    self.var_kinds.insert(var, kind);
    var
  }

  pub fn fresh(&self) -> InferTy {
    InferTy::Var(self.fresh_var(TyVarKind::General))
  }

  pub fn fresh_int(&self) -> InferTy {
    InferTy::Var(self.fresh_var(TyVarKind::Integer))
  }

  pub fn fresh_float(&self) -> InferTy {
    InferTy::Var(self.fresh_var(TyVarKind::Float))
  }

  pub fn var_kind(&self, var: TyVar) -> TyVarKind {
    self
      .var_kinds
      .get(&var)
      .map(|k| *k)
      .unwrap_or(TyVarKind::General)
  }

  pub fn unify_var(&self, var: TyVar, ty: InferTy) {
    self.substitution.insert(var, ty);
  }

  pub fn probe_var(&self, var: TyVar) -> Option<InferTy> {
    self.substitution.get(&var).map(|t| t.clone())
  }

  pub fn resolve(&self, ty: &InferTy) -> InferTy {
    match ty {
      InferTy::Var(var) => {
        if let Some(resolved) = self.probe_var(*var) {
          self.resolve(&resolved)
        } else {
          ty.clone()
        }
      }
      InferTy::Adt { def_id, args } => InferTy::Adt {
        def_id: *def_id,
        args: args.iter().map(|t| self.resolve(t)).collect(),
      },
      InferTy::Ptr { mutability, ty } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(self.resolve(ty)),
      },
      InferTy::Optional(ty) => InferTy::Optional(Box::new(self.resolve(ty))),
      InferTy::Array { ty, len } => InferTy::Array {
        ty: Box::new(self.resolve(ty)),
        len: *len,
      },
      InferTy::Slice(ty) => InferTy::Slice(Box::new(self.resolve(ty))),
      InferTy::Tuple(tys) => {
        InferTy::Tuple(tys.iter().map(|t| self.resolve(t)).collect())
      }
      InferTy::Fn { params, ret } => InferTy::Fn {
        params: params.iter().map(|t| self.resolve(t)).collect(),
        ret: Box::new(self.resolve(ret)),
      },
      _ => ty.clone(),
    }
  }

  pub fn add_constraint(&self, constraint: Constraint) {
    self.constraints.write().push(constraint);
  }

  pub fn take_constraints(&self) -> Vec<Constraint> {
    std::mem::take(&mut *self.constraints.write())
  }
}

impl Default for InferCtx {
  fn default() -> Self {
    Self::new()
  }
}

/// Tracks the types of local variables during type checking of a function body.
#[derive(Debug, Clone)]
pub struct TypeEnv {
  scopes: Vec<HashMap<DefId, InferTy>>,
}

impl TypeEnv {
  pub fn new() -> Self {
    Self {
      scopes: vec![HashMap::new()],
    }
  }

  pub fn push_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  pub fn pop_scope(&mut self) {
    self.scopes.pop();
  }

  pub fn bind(&mut self, def_id: DefId, ty: InferTy) {
    if let Some(scope) = self.scopes.last_mut() {
      scope.insert(def_id, ty);
    }
  }

  pub fn lookup(&self, def_id: DefId) -> Option<&InferTy> {
    for scope in self.scopes.iter().rev() {
      if let Some(ty) = scope.get(&def_id) {
        return Some(ty);
      }
    }
    None
  }
}

impl Default for TypeEnv {
  fn default() -> Self {
    Self::new()
  }
}
