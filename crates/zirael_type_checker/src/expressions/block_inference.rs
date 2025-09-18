use crate::TypeInference;
use zirael_parser::{AstId, AstWalker, ElseBranch, If, ScopeType, Stmt, StmtKind, Type};

impl<'reports> TypeInference<'reports> {
  pub fn infer_block(&mut self, stmts: &mut [Stmt], id: AstId) -> Type {
    self.infer_block_with_expected(stmts, id, None)
  }

  pub fn infer_block_with_expected(
    &mut self,
    stmts: &mut [Stmt],
    id: AstId,
    expected_type: Option<&Type>,
  ) -> Type {
    self.push_scope(ScopeType::Block(id));

    if stmts.is_empty() {
      self.pop_scope();
      return Type::Void;
    }

    let mut block_type = Type::Void;

    for stmt in stmts.iter_mut() {
      block_type = match &mut stmt.0 {
        StmtKind::Expr(expr) => {
          let expr_type = self.infer_expr_with_expected(expr, expected_type);
          if expr_type.is_never() {
            self.pop_scope();
            return Type::Never;
          }
          expr_type
        }
        StmtKind::Return(ret) => {
          let return_type = if let Some(expr) = ret.value.as_mut() {
            let expected_return_type = self.ctx.get_function_return_type().cloned();
            self.infer_expr_with_expected(expr, expected_return_type.as_ref())
          } else {
            Type::Void
          };
          self.pop_scope();
          return return_type;
        }
        StmtKind::Var(var) => {
          self.infer_variable(var);

          Type::Void
        }
        StmtKind::If(if_stmt) => {
          let if_type = self.infer_if_stmt(if_stmt);
          if if_type.is_never() {
            self.pop_scope();
            return Type::Never;
          }
          if_type
        }
      };
    }

    self.pop_scope();
    block_type
  }

  pub fn infer_if_stmt(&mut self, if_stmt: &mut If) -> Type {
    let condition_type = self.infer_expr(&mut if_stmt.condition);

    if !self.eq(&condition_type, &Type::Bool) {
      self.type_mismatch_with_context(
        &Type::Bool,
        &condition_type,
        if_stmt.condition.span.clone(),
        "if condition",
      );
    }

    let then_type = self.infer_block_type(&mut if_stmt.then_branch);
    let else_type =
      if_stmt.else_branch.as_mut().map(|else_branch| self.infer_else_branch(else_branch));

    self.combine_branch_types(then_type, else_type)
  }

  fn infer_else_branch(&mut self, else_branch: &mut ElseBranch) -> Type {
    match else_branch {
      ElseBranch::Block(statements, _id) => self.infer_block_type(statements),
      ElseBranch::If(nested_if) => self.infer_if_stmt(nested_if),
    }
  }

  fn infer_block_type(&mut self, statements: &mut [Stmt]) -> Type {
    let mut last_type = Type::Void;

    for stmt in statements {
      last_type = match &mut stmt.0 {
        StmtKind::Expr(expr) => {
          let expr_type = self.infer_expr(expr);
          if expr_type.is_never() {
            return Type::Never;
          }
          expr_type
        }
        StmtKind::Return(ret) => {
          return if let Some(expr) = ret.value.as_mut() {
            let expected_return_type = self.ctx.get_function_return_type().cloned();
            self.infer_expr_with_expected(expr, expected_return_type.as_ref())
          } else {
            Type::Void
          };
        }
        StmtKind::Var(var) => {
          self.infer_variable(var);
          Type::Void
        }
        StmtKind::If(nested_if) => {
          let if_type = self.infer_if_stmt(nested_if);
          if if_type.is_never() {
            return Type::Never;
          }
          if_type
        }
      };
    }

    last_type
  }

  fn combine_branch_types(&mut self, then_type: Type, else_type: Option<Type>) -> Type {
    match else_type {
      Some(else_t) => match (&then_type, &else_t) {
        (Type::Never, Type::Never) => Type::Never,
        (Type::Never, other) | (other, Type::Never) => other.clone(),
        (then_ty, else_ty) => {
          if self.eq(then_ty, else_ty) {
            then_ty.clone()
          } else {
            self.multi_label_error(
              &format!(
                "if-else branches have incompatible types: {} and {}",
                self.format_type(then_ty),
                self.format_type(else_ty)
              ),
              vec![],
            );
            Type::Void
          }
        }
      },
      None => {
        if then_type.is_never() {
          Type::Void
        } else {
          Type::Void
        }
      }
    }
  }
}
