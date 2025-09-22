use crate::TypeInference;
use std::collections::HashMap;
use zirael_parser::{
  ElseBranch, Expr, ExprKind, Stmt, StmtKind, SymbolKind, Type,
};
use zirael_utils::prelude::{warn, Identifier};

#[derive(Debug, Clone)]
pub enum UnificationResult {
  Identical(Type),
  Unified(Type),
  Incompatible,
}

impl<'reports> TypeInference<'reports> {
  pub fn unify_types(&mut self, left_ty: &Type, right_ty: &Type) -> UnificationResult {
    if self.eq(left_ty, right_ty) {
      return UnificationResult::Identical(left_ty.clone());
    }
    
    match (left_ty, right_ty) {
      (Type::Inferred, concrete) | (concrete, Type::Inferred) => {
        return UnificationResult::Unified(concrete.clone());
      }
      _ => {}
    }

    if let Some(unified) = self.try_unify_named_types(left_ty, right_ty) {
      return unified;
    }

    if let Some(unified) = self.try_unify_complex_types(left_ty, right_ty) {
      return unified;
    }

    if let Some(unified) = self.try_unify_through_monomorphization(left_ty, right_ty) {
      return unified;
    }

    UnificationResult::Incompatible
  }

  fn try_unify_named_types(&mut self, left_ty: &Type, right_ty: &Type) -> Option<UnificationResult> {
    let left_info = self.extract_generic_info(left_ty);
    let right_info = self.extract_generic_info(right_ty);

    let (left_name, left_generics) = left_info?;
    let (right_name, right_generics) = right_info?;

    if left_name != right_name || left_generics.len() != right_generics.len() {
      return Some(UnificationResult::Incompatible);
    }

    let mut unified_generics = Vec::new();
    let mut substitutions: HashMap<Identifier, Type> = HashMap::new();

    for (left_gen, right_gen) in left_generics.iter().zip(right_generics.iter()) {
      match self.unify_generic_params(left_gen, right_gen, &mut substitutions) {
        Some(unified_param) => unified_generics.push(unified_param),
        None => return Some(UnificationResult::Incompatible),
      }
    }

    for param in &mut unified_generics {
      self.apply_substitutions(param, &substitutions);
    }

    let unified_type = Type::Named { name: left_name, generics: unified_generics };
    Some(UnificationResult::Unified(unified_type))
  }

  fn try_unify_complex_types(&mut self, left_ty: &Type, right_ty: &Type) -> Option<UnificationResult> {
    match (left_ty, right_ty) {
      (Type::Pointer(left_inner), Type::Pointer(right_inner)) => {
        match self.unify_types(left_inner, right_inner) {
          UnificationResult::Identical(inner) | UnificationResult::Unified(inner) => {
            Some(UnificationResult::Unified(Type::Pointer(Box::new(inner))))
          }
          UnificationResult::Incompatible => Some(UnificationResult::Incompatible),
        }
      }
      
      (Type::Reference(left_inner), Type::Reference(right_inner)) => {
        match self.unify_types(left_inner, right_inner) {
          UnificationResult::Identical(inner) | UnificationResult::Unified(inner) => {
            Some(UnificationResult::Unified(Type::Reference(Box::new(inner))))
          }
          UnificationResult::Incompatible => Some(UnificationResult::Incompatible),
        }
      }
      
      (Type::Array(left_inner, left_size), Type::Array(right_inner, right_size)) => {
        if left_size != right_size {
          return Some(UnificationResult::Incompatible);
        }
        match self.unify_types(left_inner, right_inner) {
          UnificationResult::Identical(inner) | UnificationResult::Unified(inner) => {
            Some(UnificationResult::Unified(Type::Array(Box::new(inner), *left_size)))
          }
          UnificationResult::Incompatible => Some(UnificationResult::Incompatible),
        }
      }
      
      (Type::Function { params: left_params, return_type: left_ret }, 
       Type::Function { params: right_params, return_type: right_ret }) => {
        if left_params.len() != right_params.len() {
          return Some(UnificationResult::Incompatible);
        }
        
        let unified_ret = match self.unify_types(left_ret, right_ret) {
          UnificationResult::Identical(ret) | UnificationResult::Unified(ret) => ret,
          UnificationResult::Incompatible => return Some(UnificationResult::Incompatible),
        };
        
        let mut unified_params = Vec::new();
        for (left_param, right_param) in left_params.iter().zip(right_params.iter()) {
          match self.unify_types(left_param, right_param) {
            UnificationResult::Identical(param) | UnificationResult::Unified(param) => {
              unified_params.push(param);
            }
            UnificationResult::Incompatible => return Some(UnificationResult::Incompatible),
          }
        }
        
        Some(UnificationResult::Unified(Type::Function { 
          params: unified_params, 
          return_type: Box::new(unified_ret) 
        }))
      }
      
      _ => None,
    }
  }

  fn try_unify_through_monomorphization(&mut self, left_ty: &Type, right_ty: &Type) -> Option<UnificationResult> {
    match (left_ty, right_ty) {
      (Type::MonomorphizedSymbol(_), _) | (_, Type::MonomorphizedSymbol(_)) => {
        warn!("not available");
        None
      }
      
      (Type::Symbol(left_id), Type::Symbol(right_id)) => {
        if left_id == right_id {
          Some(UnificationResult::Identical(left_ty.clone()))
        } else {
          Some(UnificationResult::Incompatible)
        }
      }
      
      _ => None,
    }
  }

  fn extract_generic_info<'a>(&self, ty: &'a Type) -> Option<(Identifier, &'a [Type])> {
    match ty {
      Type::Named { name, generics } => Some((*name, generics.as_slice())),
      Type::Symbol(symbol_id) => {
        if let Ok(symbol) = self.symbol_table.get_symbol(*symbol_id) {
          match &symbol.kind {
            SymbolKind::Struct { .. } => {
              Some((symbol.name, &[]))
            }
            SymbolKind::Enum { .. } => {
              Some((symbol.name, &[]))
            }
            _ => None,
          }
        } else {
          None
        }
      }
      Type::MonomorphizedSymbol(_mono_id) => {
        None  
      }
      Type::String | Type::Char | Type::Int | Type::Uint | Type::Float | 
      Type::Bool | Type::Void | Type::Never | Type::Inferred | Type::Error => None,
      
      Type::Pointer(_) | Type::Reference(_) | Type::Array(_, _) | 
      Type::Function { .. } | Type::Variable { .. } | Type::BoundedVariable { .. } |
      Type::Id(_) => None,
    }
  }

  fn unify_generic_params(
    &mut self,
    left: &Type,
    right: &Type,
    substitutions: &mut HashMap<Identifier, Type>,
  ) -> Option<Type> {
    match (left, right) {
      (Type::Inferred, concrete) => Some(concrete.clone()),
      (concrete, Type::Inferred) => Some(concrete.clone()),
      
      (a, b) if self.eq(a, b) => Some(a.clone()),
      
      (Type::Variable { name: var_name, .. }, concrete) => {
        self.unify_with_substitution(*var_name, concrete, substitutions)
      }
      (concrete, Type::Variable { name: var_name, .. }) => {
        self.unify_with_substitution(*var_name, concrete, substitutions)
      }
      
      (Type::BoundedVariable { name: var_name, .. }, concrete) => {
        self.unify_with_substitution(*var_name, concrete, substitutions)
      }
      (concrete, Type::BoundedVariable { name: var_name, .. }) => {
        self.unify_with_substitution(*var_name, concrete, substitutions)
      }
      
      (Type::Symbol(left_id), Type::Symbol(right_id)) => {
        if left_id == right_id {
          Some(left.clone())
        } else {
          None
        }
      }
      
      (Type::MonomorphizedSymbol(left_id), Type::MonomorphizedSymbol(right_id)) => {
        if left_id == right_id {
          Some(left.clone())
        } else {
          None
        }
      }
      
      (Type::Pointer(left_inner), Type::Pointer(right_inner)) => {
        self.unify_generic_params(left_inner, right_inner, substitutions)
          .map(|inner| Type::Pointer(Box::new(inner)))
      }
      
      (Type::Reference(left_inner), Type::Reference(right_inner)) => {
        self.unify_generic_params(left_inner, right_inner, substitutions)
          .map(|inner| Type::Reference(Box::new(inner)))
      }
      
      (Type::Array(left_inner, left_size), Type::Array(right_inner, right_size)) => {
        if left_size != right_size {
          return None;
        }
        self.unify_generic_params(left_inner, right_inner, substitutions)
          .map(|inner| Type::Array(Box::new(inner), *left_size))
      }
      
      (Type::Function { params: left_params, return_type: left_ret }, 
       Type::Function { params: right_params, return_type: right_ret }) => {
        if left_params.len() != right_params.len() {
          return None;
        }
        
        let unified_ret = self.unify_generic_params(left_ret, right_ret, substitutions)?;
        
        let mut unified_params = Vec::new();
        for (left_param, right_param) in left_params.iter().zip(right_params.iter()) {
          let unified_param = self.unify_generic_params(left_param, right_param, substitutions)?;
          unified_params.push(unified_param);
        }
        
        Some(Type::Function { 
          params: unified_params, 
          return_type: Box::new(unified_ret) 
        })
      }
      
      (Type::Named { name: left_name, generics: left_generics }, 
       Type::Named { name: right_name, generics: right_generics }) => {
        if left_name != right_name || left_generics.len() != right_generics.len() {
          return None;
        }
        
        let mut unified_generics = Vec::new();
        for (left_gen, right_gen) in left_generics.iter().zip(right_generics.iter()) {
          let unified_gen = self.unify_generic_params(left_gen, right_gen, substitutions)?;
          unified_generics.push(unified_gen);
        }
        
        Some(Type::Named { name: *left_name, generics: unified_generics })
      }
      
      _ => {
        if self.eq(left, right) {
          Some(left.clone())
        } else {
          None
        }
      }
    }
  }

  fn unify_with_substitution(
    &mut self,
    var_name: Identifier,
    concrete: &Type,
    substitutions: &mut HashMap<Identifier, Type>,
  ) -> Option<Type> {
    if let Some(existing) = substitutions.get(&var_name) {
      if self.eq(existing, concrete) { 
        Some(concrete.clone()) 
      } else { 
        None 
      }
    } else {
      substitutions.insert(var_name, concrete.clone());
      Some(concrete.clone())
    }
  }

  fn apply_substitutions(&self, ty: &mut Type, substitutions: &HashMap<Identifier, Type>) {
    match ty {
      Type::Variable { name, .. } => {
        if let Some(substitution) = substitutions.get(name) {
          *ty = substitution.clone();
        }
      }
      Type::BoundedVariable { name, .. } => {
        if let Some(substitution) = substitutions.get(name) {
          *ty = substitution.clone();
        }
      }
      Type::Named { generics, .. } => {
        for generic in generics {
          self.apply_substitutions(generic, substitutions);
        }
      }
      Type::Pointer(inner) => {
        self.apply_substitutions(inner, substitutions);
      }
      Type::Reference(inner) => {
        self.apply_substitutions(inner, substitutions);
      }
      Type::Array(inner, _) => {
        self.apply_substitutions(inner, substitutions);
      }
      Type::Function { params, return_type } => {
        for param in params {
          self.apply_substitutions(param, substitutions);
        }
        self.apply_substitutions(return_type, substitutions);
      }
      
      Type::String | Type::Char | Type::Int | Type::Uint | Type::Float | 
      Type::Bool | Type::Void | Type::Never | Type::Inferred | Type::Error |
      Type::Symbol(_) | Type::MonomorphizedSymbol(_) | Type::Id(_) => {}
    }
  }

  pub fn update_expr_recursively(&mut self, expr: &mut Expr, resolved_ty: &Type) {
    match &mut expr.kind {
      ExprKind::Call { call, .. } | ExprKind::StaticCall { call, .. } => {
        self.update_exprs(&mut call.args);
      }
      ExprKind::MethodCall { chain, call, .. } => {
        self.update_exprs(chain);
        self.update_exprs(&mut call.args);
      }
      ExprKind::Binary { left, right, .. }
      | ExprKind::Assign(left, right)
      | ExprKind::AssignOp(left, _, right)
      | ExprKind::IndexAccess(left, right) => {
        self.update_expr_with_own_type(left);
        self.update_expr_with_own_type(right);
      }
      ExprKind::Unary(_, inner) | ExprKind::Paren(inner) => {
        self.update_expr_with_own_type(inner);
      }
      ExprKind::Ternary { condition, true_expr, false_expr } => {
        self.update_expr_with_own_type(condition);
        self.update_expr_recursively(true_expr, resolved_ty);
        self.update_expr_recursively(false_expr, resolved_ty);
      }
      ExprKind::FieldAccess(exprs) => {
        self.update_exprs(exprs);
      }
      ExprKind::StructInit { name, fields, .. } => {
        self.update_expr_with_own_type(name);
        for field_expr in fields.values_mut() {
          self.update_expr_with_own_type(field_expr);
        }
      }
      ExprKind::Block(stmts) => {
        for stmt in stmts {
          self.update_stmt_recursively(stmt);
        }
      }
      ExprKind::Match { scrutinee, arms } => {
        self.update_expr_with_own_type(scrutinee);
        for arm in arms {
          self.update_expr_recursively(&mut arm.body, resolved_ty);
        }
      }
      ExprKind::Literal(_)
      | ExprKind::Identifier(_, _)
      | ExprKind::Path(_)
      | ExprKind::CouldntParse(_) => {}
    }
  }

  fn update_expr_with_own_type(&mut self, expr: &mut Expr) {
    let expr_ty = expr.ty.clone();
    if self.has_generic_types(&expr_ty) {
      self.update_expr_recursively(expr, &expr_ty);
    }
  }

  fn update_exprs(&mut self, exprs: &mut [Expr]) {
    for expr in exprs {
      self.update_expr_with_own_type(expr);
    }
  }

  fn has_generic_types(&self, ty: &Type) -> bool {
    matches!(ty, Type::Named { generics, .. } if !generics.is_empty())
  }

  fn update_stmt_recursively(&mut self, stmt: &mut Stmt) {
    match &mut stmt.0 {
      StmtKind::Expr(expr) => {
        self.update_expr_with_own_type(expr);
      }
      StmtKind::Var(var_decl) => {
        self.update_expr_with_own_type(&mut var_decl.value);
      }
      StmtKind::Return(return_stmt) => {
        if let Some(value) = &mut return_stmt.value {
          self.update_expr_with_own_type(value);
        }
      }
      StmtKind::If(if_stmt) => {
        self.update_expr_with_own_type(&mut if_stmt.condition);
        for stmt in &mut if_stmt.then_branch {
          self.update_stmt_recursively(stmt);
        }
        if let Some(else_branch) = &mut if_stmt.else_branch {
          match else_branch {
            ElseBranch::Block(statements, _else_branch_id) => {
              for stmt in statements {
                self.update_stmt_recursively(stmt);
              }
            }
            ElseBranch::If(nested_if) => {
              self.update_expr_with_own_type(&mut nested_if.condition);
              for stmt in &mut nested_if.then_branch {
                self.update_stmt_recursively(stmt);
              }
              if let Some(nested_else) = &mut nested_if.else_branch {
                match nested_else {
                  ElseBranch::Block(nested_statements, _nested_else_id) => {
                    for stmt in nested_statements {
                      self.update_stmt_recursively(stmt);
                    }
                  }
                  ElseBranch::If(_) => {}
                }
              }
            }
          }
        }
      }
    }
  }
}
