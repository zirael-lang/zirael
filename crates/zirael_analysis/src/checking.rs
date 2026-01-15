use std::collections::HashMap;

use crate::table::{InferCtx, TypeEnv, TypeTable};
use crate::ty::{Expectation, InferTy, TyVar, TyVarKind, TypeScheme};
use zirael_diagnostics::DiagnosticCtx;
use zirael_hir::expr::PathExpr;
use zirael_hir::ty::ArrayLen;
use zirael_hir::{
  Block, Const, Expr, ExprKind, Function, GenericParamKind, Generics, Hir,
  Literal, Method, Param, ParamKind, Pat, PatKind, Stmt, StmtKind, Ty, TyKind,
};
use zirael_parser::Mutability;
use zirael_parser::ast::types::PrimitiveKind;
use zirael_resolver::{DefId, DefKind, Resolver};
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::Span;

pub fn typeck_hir(
  hir: &Hir,
  dcx: &DiagnosticCtx,
  resolver: &Resolver,
) -> TypeTable {
  let mut checker = TyChecker::new(hir, dcx, resolver);

  checker.collect_signatures();

  for entry in hir.functions.iter() {
    let def_id = *entry.key();
    let func = entry.value();
    checker.typeck_function(def_id, &func);
  }

  for entry in hir.consts.iter() {
    let def_id = *entry.key();
    let konst = entry.value();
    checker.typeck_const(def_id, &konst);
  }

  checker.finalize_types();
  checker.table
}

pub struct TyChecker<'a> {
  pub hir: &'a Hir,
  pub dcx: &'a DiagnosticCtx,
  pub resolver: &'a Resolver,
  pub table: TypeTable,
  pub infcx: InferCtx,
}

impl<'a> TyChecker<'a> {
  pub fn new(
    hir: &'a Hir,
    dcx: &'a DiagnosticCtx,
    resolver: &'a Resolver,
  ) -> Self {
    Self {
      hir,
      dcx,
      resolver,
      table: TypeTable::new(),
      infcx: InferCtx::new(),
    }
  }

  pub fn hir(&self) -> &'a Hir {
    self.hir
  }

  pub fn dcx(&self) -> &'a DiagnosticCtx {
    self.dcx
  }

  fn collect_signatures(&mut self) {
    for entry in self.hir.functions.iter() {
      let def_id = *entry.key();
      let func = entry.value();
      let scheme = self.function_signature(&func);
      self.table.record_def_type(def_id, scheme);
    }

    for entry in self.hir.structs.iter() {
      let def_id = *entry.key();
      let strukt = entry.value();

      let ty_vars = self.register_generics(&strukt.generics);

      for field in &strukt.fields {
        let field_ty = self.lower_hir_ty(&field.ty);
        self
          .table
          .record_field_type(def_id, field.name.text(), field_ty);
      }

      let struct_ty = InferTy::Adt {
        def_id,
        args: ty_vars.iter().map(|&v| InferTy::Var(v)).collect(),
      };
      self.table.record_def_type(
        def_id,
        TypeScheme {
          vars: ty_vars.clone(),
          ty: struct_ty,
        },
      );

      for method in &strukt.methods {
        let method_scheme =
          self.method_signature_with_parent_generics(method, &ty_vars);
        self.table.record_method_type(
          def_id,
          method.name.text(),
          method_scheme,
        );
      }
    }

    for entry in self.hir.enums.iter() {
      let def_id = *entry.key();
      let eenum = entry.value();

      let ty_vars = self.register_generics(&eenum.generics);

      let enum_ty = InferTy::Adt {
        def_id,
        args: ty_vars.iter().map(|&v| InferTy::Var(v)).collect(),
      };
      self.table.record_def_type(
        def_id,
        TypeScheme {
          vars: ty_vars,
          ty: enum_ty,
        },
      );

      // TODO: Record enum variant types
    }

    for entry in self.hir.consts.iter() {
      let def_id = *entry.key();
      let konst = entry.value();
      self.infcx.clear_type_params();
      let ty = self.lower_hir_ty(&konst.ty);
      self.table.record_def_type(def_id, TypeScheme::mono(ty));
    }
  }

  fn register_generics(&self, generics: &Generics) -> Vec<TyVar> {
    self.infcx.clear_type_params();

    let mut ty_vars = Vec::new();
    for param in &generics.params {
      if matches!(param.kind, GenericParamKind::Type { .. }) {
        let var = self.infcx.get_or_create_type_param(param.def_id);
        ty_vars.push(var);
      }
    }
    ty_vars
  }

  fn function_signature(&self, func: &Function) -> TypeScheme {
    let ty_vars = self.register_generics(&func.generics);

    let params = self.param_types(&func.params);
    let ret = self.lower_hir_ty(&func.return_type);

    let fn_ty = InferTy::Fn {
      params,
      ret: Box::new(ret),
    };

    TypeScheme {
      vars: ty_vars,
      ty: fn_ty,
    }
  }

  fn param_types(&self, params: &Vec<Param>) -> Vec<InferTy> {
    params
      .iter()
      .filter_map(|p| match &p.kind {
        ParamKind::Regular { ty, .. } => Some(self.lower_hir_ty(ty)),
        ParamKind::Variadic { ty, .. } => Some(self.lower_hir_ty(ty)),
        ParamKind::SelfParam { .. } => None,
      })
      .collect()
  }

  fn method_signature_with_parent_generics(
    &self,
    method: &Method,
    parent_ty_vars: &[TyVar],
  ) -> TypeScheme {
    // TODO: Handle method-level generics

    let params = self.param_types(&method.params);

    let ret = method
      .return_type
      .as_ref()
      .map(|ty| self.lower_hir_ty(ty))
      .unwrap_or(InferTy::Unit);

    let fn_ty = InferTy::Fn {
      params,
      ret: Box::new(ret),
    };

    TypeScheme {
      vars: parent_ty_vars.to_vec(),
      ty: fn_ty,
    }
  }

  #[allow(dead_code)]
  fn method_signature(&self, method: &Method) -> TypeScheme {
    let params = self.param_types(&method.params);

    let ret = method
      .return_type
      .as_ref()
      .map(|ty| self.lower_hir_ty(ty))
      .unwrap_or(InferTy::Unit);

    let fn_ty = InferTy::Fn {
      params,
      ret: Box::new(ret),
    };

    TypeScheme::mono(fn_ty)
  }

  fn lower_hir_ty(&self, ty: &Ty) -> InferTy {
    match &ty.kind {
      TyKind::Infer => self.infcx.fresh(),
      TyKind::Primitive(p) => InferTy::Primitive(*p),
      TyKind::Path { def_id, segments } => {
        if let Some(def) = self.resolver.get_definition(*def_id)
          && def.kind == DefKind::TypeParam
        {
          let var = self.infcx.get_or_create_type_param(*def_id);
          return InferTy::Var(var);
        }

        let infer_args: Vec<InferTy> = segments
          .iter()
          .flat_map(|seg| seg.args.iter())
          .map(|t| self.lower_hir_ty(t))
          .collect();
        InferTy::Adt {
          def_id: *def_id,
          args: infer_args,
        }
      }
      TyKind::Ptr { mutability, ty } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(self.lower_hir_ty(ty)),
      },
      TyKind::Optional(ty) => {
        InferTy::Optional(Box::new(self.lower_hir_ty(ty)))
      }
      TyKind::Array { ty, len } => {
        let array_len = match len {
          ArrayLen::Const(n) => *n,
          ArrayLen::ConstExpr(_) => 0, // TODO: Evaluate const expression
        };
        InferTy::Array {
          ty: Box::new(self.lower_hir_ty(ty)),
          len: array_len,
        }
      }
      TyKind::Slice(ty) => InferTy::Slice(Box::new(self.lower_hir_ty(ty))),
      TyKind::Tuple(tys) => {
        InferTy::Tuple(tys.iter().map(|t| self.lower_hir_ty(t)).collect())
      }
      TyKind::Unit => InferTy::Unit,
      TyKind::Never => InferTy::Never,
      TyKind::Err => InferTy::Err,
      TyKind::Fn { params, ret } => InferTy::Fn {
        params: params.iter().map(|t| self.lower_hir_ty(t)).collect(),
        ret: Box::new(self.lower_hir_ty(ret)),
      },
    }
  }

  fn typeck_function(&mut self, _def_id: DefId, func: &Function) {
    let mut env = TypeEnv::new();

    for param in &func.params {
      match &param.kind {
        ParamKind::Regular { ty, .. } => {
          let param_ty = self.lower_hir_ty(ty);
          env.bind(param.def_id, param_ty);
        }
        ParamKind::Variadic { ty, .. } => {
          let param_ty = self.lower_hir_ty(ty);
          env.bind(param.def_id, InferTy::Slice(Box::new(param_ty)));
        }
        ParamKind::SelfParam { .. } => {
          // TODO: Bind self parameter
        }
      }
    }

    if let Some(body) = &func.body {
      let expected_ret = self.lower_hir_ty(&func.return_type);
      let body_ty = self.check_block(
        body,
        &mut env,
        &Expectation::has_type(expected_ret.clone()),
      );

      self.unify(&body_ty, &expected_ret, func.span);
    }
  }

  fn typeck_const(&mut self, _def_id: DefId, konst: &Const) {
    let mut env = TypeEnv::new();
    let expected = self.lower_hir_ty(&konst.ty);

    let init_ty = self.check_expr(
      &konst.value,
      &mut env,
      &Expectation::has_type(expected.clone()),
    );
    self.unify(&init_ty, &expected, konst.span);
  }

  fn check_block(
    &mut self,
    block: &Block,
    env: &mut TypeEnv,
    expect: &Expectation,
  ) -> InferTy {
    env.push_scope();

    let mut last_ty = InferTy::Unit;

    for stmt in &block.stmts {
      last_ty = self.check_stmt(stmt, env);
    }

    if let Some(expr) = &block.expr {
      last_ty = self.check_expr(expr, env, expect);
    }

    env.pop_scope();
    last_ty
  }

  fn check_stmt(&mut self, stmt: &Stmt, env: &mut TypeEnv) -> InferTy {
    match &stmt.kind {
      StmtKind::Local(local) => {
        let expected = match &local.ty {
          Some(ty) => self.lower_hir_ty(ty),
          None => self.infcx.fresh(),
        };

        if let Some(init) = &local.init {
          let init_ty = self.check_expr(
            init,
            env,
            &Expectation::has_type(expected.clone()),
          );
          self.unify(&init_ty, &expected, local.span);
        }

        self.check_pattern(&local.pat, &expected, env);

        InferTy::Unit
      }
      StmtKind::Expr(expr) => {
        self.check_expr(expr, env, &Expectation::none());
        InferTy::Unit
      }
      StmtKind::Semi(expr) => {
        self.check_expr(expr, env, &Expectation::none());
        InferTy::Unit
      }
    }
  }

  fn check_expr(
    &mut self,
    expr: &Expr,
    env: &mut TypeEnv,
    expect: &Expectation,
  ) -> InferTy {
    let ty = match &expr.kind {
      ExprKind::Literal(lit) => self.check_literal(lit),

      ExprKind::Path(path) => self.check_path(path, env),

      ExprKind::Binary { op: _, lhs, rhs } => {
        let lhs_ty = self.check_expr(lhs, env, &Expectation::none());
        let rhs_ty = self.check_expr(rhs, env, &Expectation::none());
        // TODO: Implement operator type checking
        self.unify(&lhs_ty, &rhs_ty, expr.span);
        lhs_ty
      }

      ExprKind::Unary { op: _, operand } => {
        let operand_ty = self.check_expr(operand, env, &Expectation::none());
        // TODO: Implement unary operator type checking
        operand_ty
      }

      ExprKind::Call { callee, args } => {
        self.check_call(callee, args, env, expr.span)
      }

      ExprKind::If {
        condition,
        then_block,
        else_branch,
      } => {
        let cond_ty = self.check_expr(
          condition,
          env,
          &Expectation::has_type(InferTy::bool()),
        );
        self.unify(&cond_ty, &InferTy::bool(), condition.span);

        let then_ty = self.check_block(then_block, env, expect);

        if let Some(else_expr) = else_branch {
          let else_ty = self.check_expr(else_expr, env, expect);
          self.unify(&then_ty, &else_ty, expr.span);
          then_ty
        } else {
          InferTy::Unit
        }
      }

      ExprKind::Block(block) => self.check_block(block, env, expect),

      ExprKind::Tuple(exprs) => {
        let tys: Vec<InferTy> = exprs
          .iter()
          .map(|e| self.check_expr(e, env, &Expectation::none()))
          .collect();
        InferTy::Tuple(tys)
      }

      ExprKind::Array(exprs) => {
        if exprs.is_empty() {
          InferTy::Array {
            ty: Box::new(self.infcx.fresh()),
            len: 0,
          }
        } else {
          let elem_ty = self.check_expr(&exprs[0], env, &Expectation::none());
          for e in exprs.iter().skip(1) {
            let ty =
              self.check_expr(e, env, &Expectation::has_type(elem_ty.clone()));
            self.unify(&ty, &elem_ty, e.span);
          }
          InferTy::Array {
            ty: Box::new(elem_ty),
            len: exprs.len(),
          }
        }
      }

      ExprKind::Index { object, index } => {
        let obj_ty = self.check_expr(object, env, &Expectation::none());
        let idx_ty = self.check_expr(index, env, &Expectation::none());

        // Index must be usize until we support operator overloading
        self.unify(
          &idx_ty,
          &InferTy::Primitive(PrimitiveKind::USize),
          index.span,
        );

        match self.infcx.resolve(&obj_ty) {
          InferTy::Array { ty, .. } | InferTy::Slice(ty) => *ty,
          _ => {
            // TODO: Report error - not indexable
            InferTy::Err
          }
        }
      }

      ExprKind::Field { object, field } => {
        let obj_ty = self.check_expr(object, env, &Expectation::none());
        self.check_field_access(&obj_ty, field, object.span)
      }

      ExprKind::Assign { target, value, .. } => {
        let target_ty = self.check_expr(target, env, &Expectation::none());
        let value_ty = self.check_expr(
          value,
          env,
          &Expectation::has_type(target_ty.clone()),
        );
        self.unify(&value_ty, &target_ty, expr.span);
        InferTy::Unit
      }

      ExprKind::Return { value } => {
        if let Some(val) = value {
          self.check_expr(val, env, expect);
        }
        InferTy::Never
      }

      ExprKind::Break { value, .. } => {
        if let Some(val) = value {
          self.check_expr(val, env, &Expectation::none());
        }
        InferTy::Never
      }

      ExprKind::Continue { .. } => InferTy::Never,

      _ => {
        // TODO: Handle remaining expression kinds
        self.infcx.fresh()
      }
    };

    self.table.record_node_type(expr.hir_id, ty.clone());
    ty
  }

  fn check_literal(&self, lit: &Literal) -> InferTy {
    match lit {
      Literal::Int { suffix, .. } => {
        if let Some(suffix) = suffix {
          InferTy::Primitive(suffix.to_primitive_kind())
        } else {
          self.infcx.fresh_int()
        }
      }
      Literal::Float { suffix, .. } => {
        if let Some(suffix) = suffix {
          InferTy::Primitive(suffix.to_primitive_kind())
        } else {
          self.infcx.fresh_float()
        }
      }
      Literal::Bool(_) => InferTy::bool(),
      Literal::Char(_) => InferTy::Primitive(PrimitiveKind::Char),
      Literal::String(_) => InferTy::Ptr {
        mutability: Mutability::Const,
        ty: Box::new(InferTy::Primitive(PrimitiveKind::U8)),
      },
      Literal::Byte(_) => InferTy::Primitive(PrimitiveKind::U8),
      Literal::Unit => InferTy::Unit,
    }
  }

  fn check_path(&self, path: &PathExpr, env: &TypeEnv) -> InferTy {
    if let Some(ty) = env.lookup(path.def_id) {
      return ty.clone();
    }

    if let Some(scheme) = self.table.def_type(path.def_id) {
      return self.instantiate(&scheme);
    }

    InferTy::Err
  }

  fn instantiate(&self, scheme: &TypeScheme) -> InferTy {
    if scheme.vars.is_empty() {
      return scheme.ty.clone();
    }

    let mut subst: HashMap<TyVar, InferTy> = HashMap::new();
    for &var in &scheme.vars {
      subst.insert(var, self.infcx.fresh());
    }

    self.apply_subst(&scheme.ty, &subst)
  }

  fn apply_subst(
    &self,
    ty: &InferTy,
    subst: &HashMap<TyVar, InferTy>,
  ) -> InferTy {
    match ty {
      InferTy::Var(var) => {
        if let Some(replacement) = subst.get(var) {
          replacement.clone()
        } else {
          ty.clone()
        }
      }
      InferTy::Adt { def_id, args } => InferTy::Adt {
        def_id: *def_id,
        args: args.iter().map(|t| self.apply_subst(t, subst)).collect(),
      },
      InferTy::Ptr {
        mutability,
        ty: inner,
      } => InferTy::Ptr {
        mutability: *mutability,
        ty: Box::new(self.apply_subst(inner, subst)),
      },
      InferTy::Optional(inner) => {
        InferTy::Optional(Box::new(self.apply_subst(inner, subst)))
      }
      InferTy::Array { ty: inner, len } => InferTy::Array {
        ty: Box::new(self.apply_subst(inner, subst)),
        len: *len,
      },
      InferTy::Slice(inner) => {
        InferTy::Slice(Box::new(self.apply_subst(inner, subst)))
      }
      InferTy::Tuple(tys) => {
        InferTy::Tuple(tys.iter().map(|t| self.apply_subst(t, subst)).collect())
      }
      InferTy::Fn { params, ret } => InferTy::Fn {
        params: params.iter().map(|t| self.apply_subst(t, subst)).collect(),
        ret: Box::new(self.apply_subst(ret, subst)),
      },
      _ => ty.clone(),
    }
  }

  fn check_call(
    &mut self,
    callee: &Expr,
    args: &[Expr],
    env: &mut TypeEnv,
    span: Span,
  ) -> InferTy {
    let callee_ty = self.check_expr(callee, env, &Expectation::none());
    let resolved = self.infcx.resolve(&callee_ty);

    match resolved {
      InferTy::Fn { params, ret } => {
        if args.len() != params.len() {
          // TODO: Report arity mismatch error
        }

        for (arg, param_ty) in args.iter().zip(params.iter()) {
          let arg_ty =
            self.check_expr(arg, env, &Expectation::has_type(param_ty.clone()));
          self.unify(param_ty, &arg_ty, arg.span);
        }

        *ret
      }
      InferTy::Var(_) => {
        let arg_tys: Vec<InferTy> = args
          .iter()
          .map(|a| self.check_expr(a, env, &Expectation::none()))
          .collect();
        let ret_ty = self.infcx.fresh();

        let expected_fn = InferTy::Fn {
          params: arg_tys,
          ret: Box::new(ret_ty.clone()),
        };
        self.unify(&callee_ty, &expected_fn, span);

        ret_ty
      }
      _ => {
        // TODO: Report error - not callable
        InferTy::Err
      }
    }
  }

  fn check_field_access(
    &self,
    obj_ty: &InferTy,
    field: &Identifier,
    _span: Span,
  ) -> InferTy {
    let resolved = self.infcx.resolve(obj_ty);

    match resolved {
      InferTy::Adt { def_id, .. } => {
        if let Some(field_ty) = self.table.field_type(def_id, &field.text()) {
          field_ty
        } else {
          // TODO: Report unknown field error
          InferTy::Err
        }
      }
      InferTy::Tuple(tys) => {
        // Check for tuple field access (e.g., tuple.0)
        if let Ok(idx) = field.text().parse::<usize>() {
          if idx < tys.len() {
            return tys[idx].clone();
          }
        }
        // TODO: Report error
        InferTy::Err
      }
      _ => {
        // TODO: Report error - no fields on this type
        InferTy::Err
      }
    }
  }

  fn check_pattern(
    &mut self,
    pat: &Pat,
    expected: &InferTy,
    env: &mut TypeEnv,
  ) {
    match &pat.kind {
      PatKind::Binding { def_id, .. } => {
        env.bind(*def_id, expected.clone());
        self.table.record_node_type(pat.hir_id, expected.clone());
      }
      PatKind::Wild => {
        self.table.record_node_type(pat.hir_id, expected.clone());
      }
      PatKind::Tuple(pats) => {
        if let InferTy::Tuple(tys) = expected {
          for (p, ty) in pats.iter().zip(tys.iter()) {
            self.check_pattern(p, ty, env);
          }
        } else {
          // TODO: Report pattern mismatch
        }
      }
      PatKind::Literal(_) => {
        // TODO: Check literal pattern
        self.table.record_node_type(pat.hir_id, expected.clone());
      }
      _ => {
        // TODO: Handle other patterns
      }
    }
  }

  fn finalize_types(&mut self) {
    let node_entries: Vec<_> = self
      .table
      .node_types
      .iter()
      .map(|e| (*e.key(), e.value().clone()))
      .collect();

    for (hir_id, ty) in node_entries {
      let resolved = self.infcx.resolve(&ty);
      let defaulted = self.default_ty_vars(resolved);
      self.table.node_types.insert(hir_id, defaulted);
    }

    let field_entries: Vec<_> = self
      .table
      .field_types
      .iter()
      .map(|e| (e.key().clone(), e.value().clone()))
      .collect();

    for (key, ty) in field_entries {
      let resolved = self.infcx.resolve(&ty);
      let defaulted = self.default_ty_vars(resolved);
      self.table.field_types.insert(key, defaulted);
    }

    let method_entries: Vec<_> = self
      .table
      .method_types
      .iter()
      .map(|e| (e.key().clone(), e.value().clone()))
      .collect();

    for (key, scheme) in method_entries {
      let finalized_scheme = self.finalize_scheme(scheme);
      self.table.method_types.insert(key, finalized_scheme);
    }

    let def_entries: Vec<_> = self
      .table
      .def_types
      .iter()
      .map(|e| (*e.key(), e.value().clone()))
      .collect();

    for (def_id, scheme) in def_entries {
      let finalized_scheme = self.finalize_scheme(scheme);
      self.table.def_types.insert(def_id, finalized_scheme);
    }
  }

  fn finalize_scheme(&self, scheme: TypeScheme) -> TypeScheme {
    let resolved = self.infcx.resolve(&scheme.ty);
    // For polymorphic type variables in the scheme, we keep them as-is
    let defaulted = self.default_ty_vars_except(resolved, &scheme.vars);
    TypeScheme {
      vars: scheme.vars,
      ty: defaulted,
    }
  }

  /// Default unconstrained type variables to concrete types.
  /// Ints default to i32, floats to f64, generals become errs because they couldn't be inferred
  fn default_ty_vars(&self, ty: InferTy) -> InferTy {
    self.default_ty_vars_except(ty, &[])
  }

  fn default_ty_vars_except(&self, ty: InferTy, except: &[TyVar]) -> InferTy {
    match ty {
      InferTy::Var(v) => {
        if except.contains(&v) {
          InferTy::Var(v)
        } else {
          match self.infcx.var_kind(v) {
            TyVarKind::Integer => InferTy::Primitive(PrimitiveKind::I32),
            TyVarKind::Float => InferTy::Primitive(PrimitiveKind::F64),
            TyVarKind::General => {
              // TODO: Emit "cannot infer type" diagnostic
              InferTy::Err
            }
          }
        }
      }
      InferTy::Adt { def_id, args } => InferTy::Adt {
        def_id,
        args: args
          .into_iter()
          .map(|t| self.default_ty_vars_except(t, except))
          .collect(),
      },
      InferTy::Ptr { mutability, ty } => InferTy::Ptr {
        mutability,
        ty: Box::new(self.default_ty_vars_except(*ty, except)),
      },
      InferTy::Optional(ty) => {
        InferTy::Optional(Box::new(self.default_ty_vars_except(*ty, except)))
      }
      InferTy::Array { ty, len } => InferTy::Array {
        ty: Box::new(self.default_ty_vars_except(*ty, except)),
        len,
      },
      InferTy::Slice(ty) => {
        InferTy::Slice(Box::new(self.default_ty_vars_except(*ty, except)))
      }
      InferTy::Tuple(tys) => InferTy::Tuple(
        tys
          .into_iter()
          .map(|t| self.default_ty_vars_except(t, except))
          .collect(),
      ),
      InferTy::Fn { params, ret } => InferTy::Fn {
        params: params
          .into_iter()
          .map(|t| self.default_ty_vars_except(t, except))
          .collect(),
        ret: Box::new(self.default_ty_vars_except(*ret, except)),
      },
      InferTy::Primitive(_)
      | InferTy::Unit
      | InferTy::Never
      | InferTy::Param { .. }
      | InferTy::Err => ty,
    }
  }
}
