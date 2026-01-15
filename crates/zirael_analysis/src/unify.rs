use crate::{InferTy, TyChecker, TyVarKind, UnifyError, UnifyResult};
use zirael_utils::prelude::Span;

impl TyChecker<'_> {
  pub fn unify(&self, a: &InferTy, b: &InferTy, span: Span) -> UnifyResult {
    let a = self.infcx.resolve(a);
    let b = self.infcx.resolve(b);

    match (&a, &b) {
      _ if a == b => UnifyResult::Ok,

      (InferTy::Err, _) | (_, InferTy::Err) => UnifyResult::Ok,

      (InferTy::Never, _) | (_, InferTy::Never) => UnifyResult::Ok,

      (InferTy::Var(v1), InferTy::Var(v2)) => {
        let k1 = self.infcx.var_kind(*v1);
        let k2 = self.infcx.var_kind(*v2);

        match (k1, k2) {
          (TyVarKind::General, TyVarKind::Integer)
          | (TyVarKind::General, TyVarKind::Float) => {
            self.infcx.unify_var(*v1, b.clone());
          }
          (TyVarKind::Integer, TyVarKind::General)
          | (TyVarKind::Float, TyVarKind::General) => {
            self.infcx.unify_var(*v2, a.clone());
          }
          (TyVarKind::Integer, TyVarKind::Float)
          | (TyVarKind::Float, TyVarKind::Integer) => {
            return UnifyResult::Err(UnifyError::IncompatibleKinds {
              kind1: k1,
              kind2: k2,
            });
          }
          _ => {
            self.infcx.unify_var(*v1, b.clone());
          }
        }
        UnifyResult::Ok
      }

      (InferTy::Var(v), t) | (t, InferTy::Var(v)) => {
        if t.has_vars() {
          // TODO: Proper occurs check
        }

        let kind = self.infcx.var_kind(*v);
        match kind {
          TyVarKind::Integer => {
            if !t.is_integer() {
              return UnifyResult::Err(UnifyError::NotAnInteger {
                ty: t.clone(),
              });
            }
          }
          TyVarKind::Float => {
            if !t.is_float() {
              return UnifyResult::Err(UnifyError::NotAFloat { ty: t.clone() });
            }
          }
          TyVarKind::General => {}
        }

        self.infcx.unify_var(*v, t.clone());
        UnifyResult::Ok
      }

      (
        InferTy::Ptr {
          mutability: m1,
          ty: t1,
        },
        InferTy::Ptr {
          mutability: m2,
          ty: t2,
        },
      ) => {
        if m1 != m2 {
          return UnifyResult::Err(UnifyError::MutabilityMismatch {
            expected: *m1,
            found: *m2,
          });
        }
        self.unify(t1, t2, span)
      }

      (InferTy::Optional(t1), InferTy::Optional(t2)) => {
        self.unify(t1, t2, span)
      }

      (
        InferTy::Array { ty: t1, len: l1 },
        InferTy::Array { ty: t2, len: l2 },
      ) => {
        if l1 != l2 {
          return UnifyResult::Err(UnifyError::ArrayLenMismatch {
            expected: *l1,
            found: *l2,
          });
        }
        self.unify(t1, t2, span)
      }

      (InferTy::Slice(t1), InferTy::Slice(t2)) => self.unify(t1, t2, span),

      (InferTy::Tuple(ts1), InferTy::Tuple(ts2)) => {
        if ts1.len() != ts2.len() {
          return UnifyResult::Err(UnifyError::ArityMismatch {
            expected: ts1.len(),
            found: ts2.len(),
          });
        }
        for (t1, t2) in ts1.iter().zip(ts2.iter()) {
          let result = self.unify(t1, t2, span);
          if result.is_err() {
            return result;
          }
        }
        UnifyResult::Ok
      }

      (
        InferTy::Fn {
          params: p1,
          ret: r1,
        },
        InferTy::Fn {
          params: p2,
          ret: r2,
        },
      ) => {
        if p1.len() != p2.len() {
          return UnifyResult::Err(UnifyError::ArityMismatch {
            expected: p1.len(),
            found: p2.len(),
          });
        }
        for (t1, t2) in p1.iter().zip(p2.iter()) {
          let result = self.unify(t1, t2, span);
          if result.is_err() {
            return result;
          }
        }
        self.unify(r1, r2, span)
      }

      (
        InferTy::Adt {
          def_id: d1,
          args: a1,
        },
        InferTy::Adt {
          def_id: d2,
          args: a2,
        },
      ) => {
        if d1 != d2 {
          return UnifyResult::Err(UnifyError::Mismatch {
            expected: a.clone(),
            found: b.clone(),
          });
        }
        if a1.len() != a2.len() {
          return UnifyResult::Err(UnifyError::ArityMismatch {
            expected: a1.len(),
            found: a2.len(),
          });
        }
        for (t1, t2) in a1.iter().zip(a2.iter()) {
          let result = self.unify(t1, t2, span);
          if result.is_err() {
            return result;
          }
        }
        UnifyResult::Ok
      }

      _ => UnifyResult::Err(UnifyError::Mismatch {
        expected: a,
        found: b,
      }),
    }
  }
}
