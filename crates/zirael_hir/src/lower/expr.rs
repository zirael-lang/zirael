use crate::expr::{
  Block, Expr, ExprKind, FieldInit, Literal, Local, MatchArm, PathExpr,
  PathSegment, Stmt, StmtKind,
};
use crate::lower::context::LoweringContext;
use crate::pat::{Pat, PatKind};
use zirael_parser::ast::expressions::{self, ExprKind as AstExprKind};
use zirael_parser::ast::statements;
use zirael_source::prelude::Span;
use zirael_utils::prelude::Identifier;

impl LoweringContext<'_> {
  pub fn lower_expr(&mut self, expr: &expressions::Expr) -> Expr {
    let kind = match &expr.kind {
      AstExprKind::Literal(lit) => ExprKind::Literal(self.lower_literal(lit)),

      AstExprKind::Path(path) => {
        let def_id = self.get_def_id(expr.id);

        if let Some(def_id) = def_id {
          ExprKind::Path(PathExpr {
            def_id,
            segments: path
              .segments
              .iter()
              .map(|s| PathSegment {
                name: s.identifier,
                args: s.args.iter().map(|a| self.lower_type(a)).collect(),
              })
              .collect(),
          })
        } else {
          ExprKind::Err
        }
      }

      AstExprKind::SelfValue => {
        if let Some(def_id) = self.get_def_id(expr.id) {
          ExprKind::Path(PathExpr {
            def_id,
            segments: vec![PathSegment {
              name: Identifier::new("self", Span::dummy()),
              args: vec![],
            }],
          })
        } else {
          ExprKind::Err
        }
      }

      AstExprKind::Binary { op, left, right } => ExprKind::Binary {
        op: *op,
        lhs: Box::new(self.lower_expr(left)),
        rhs: Box::new(self.lower_expr(right)),
      },

      AstExprKind::Unary { op, operand } => ExprKind::Unary {
        op: *op,
        operand: Box::new(self.lower_expr(operand)),
      },

      AstExprKind::Assign { op, target, value } => ExprKind::Assign {
        op: *op,
        target: Box::new(self.lower_expr(target)),
        value: Box::new(self.lower_expr(value)),
      },

      AstExprKind::Cast { expr, target_type } => ExprKind::Cast {
        expr: Box::new(self.lower_expr(expr)),
        ty: self.lower_type(target_type),
      },

      AstExprKind::Call { callee, args } => ExprKind::Call {
        callee: Box::new(self.lower_expr(callee)),
        args: args.iter().map(|a| self.lower_expr(&a.value)).collect(),
      },

      AstExprKind::Field { object, field } => ExprKind::Field {
        object: Box::new(self.lower_expr(object)),
        field: *field,
      },

      AstExprKind::Index { object, index } => ExprKind::Index {
        object: Box::new(self.lower_expr(object)),
        index: Box::new(self.lower_expr(index)),
      },

      AstExprKind::AddrOf {
        mutability,
        operand,
      } => ExprKind::AddrOf {
        mutability: *mutability,
        operand: Box::new(self.lower_expr(operand)),
      },

      AstExprKind::Struct { path, fields } => {
        let def_id = self.get_def_id(path.id);

        if let Some(def_id) = def_id {
          ExprKind::Struct {
            def_id,
            fields: fields
              .iter()
              .map(|f| FieldInit {
                hir_id: self.next_hir_id(),
                name: f.name,
                value: self.lower_expr(&f.value),
                span: f.span,
              })
              .collect(),
          }
        } else {
          ExprKind::Err
        }
      }

      AstExprKind::Tuple(exprs) => {
        ExprKind::Tuple(exprs.iter().map(|e| self.lower_expr(e)).collect())
      }

      AstExprKind::Array(exprs) => {
        ExprKind::Array(exprs.iter().map(|e| self.lower_expr(e)).collect())
      }

      AstExprKind::Block(block) => ExprKind::Block(self.lower_block(block)),

      AstExprKind::If(if_expr) => self.lower_if_expr(if_expr),

      AstExprKind::Match(match_expr) => ExprKind::Match {
        scrutinee: Box::new(self.lower_expr(&match_expr.scrutinee)),
        arms: match_expr
          .arms
          .iter()
          .map(|a| self.lower_match_arm(a))
          .collect(),
      },

      AstExprKind::Loop(loop_expr) => ExprKind::Loop {
        body: self.lower_block(&loop_expr.body),
      },

      AstExprKind::While(_) | AstExprKind::For(_) | AstExprKind::Range(_) => {
        todo!("desugar")
      }

      AstExprKind::Break(break_expr) => ExprKind::Break {
        value: break_expr
          .value
          .as_ref()
          .map(|v| Box::new(self.lower_expr(v))),
      },

      AstExprKind::Continue(_) => ExprKind::Continue,

      AstExprKind::Return(ret_expr) => ExprKind::Return {
        value: ret_expr
          .value
          .as_ref()
          .map(|v| Box::new(self.lower_expr(v))),
      },

      AstExprKind::Ternary {
        condition,
        then_expr,
        else_expr,
      } => ExprKind::If {
        condition: Box::new(self.lower_expr(condition)),
        then_block: Block {
          hir_id: self.next_hir_id(),
          stmts: vec![],
          expr: Some(Box::new(self.lower_expr(then_expr))),
          span: then_expr.span,
        },
        else_branch: Some(Box::new(self.lower_expr(else_expr))),
      },

      AstExprKind::Builtin { name: _, args: _ } => todo!("handle builtins"),
    };

    Expr {
      hir_id: self.next_hir_id(),
      kind,
      span: expr.span,
    }
  }

  fn lower_if_expr(&mut self, if_expr: &expressions::IfExpr) -> ExprKind {
    let else_branch = match &if_expr.else_branch {
      None => None,
      Some(expressions::ElseBranch::Block(block)) => Some(Box::new(Expr {
        hir_id: self.next_hir_id(),
        kind: ExprKind::Block(self.lower_block(block)),
        span: block.span,
      })),
      Some(expressions::ElseBranch::If(nested_if)) => {
        let kind = self.lower_if_expr(nested_if);
        Some(Box::new(Expr {
          hir_id: self.next_hir_id(),
          kind,
          span: nested_if.span,
        }))
      }
    };

    ExprKind::If {
      condition: Box::new(self.lower_expr(&if_expr.condition)),
      then_block: self.lower_block(&if_expr.then_block),
      else_branch,
    }
  }

  fn lower_match_arm(&mut self, arm: &expressions::MatchArm) -> MatchArm {
    MatchArm {
      hir_id: self.next_hir_id(),
      pat: self.lower_ast_pattern(&arm.pattern),
      guard: None, // TODO: add guard support
      body: self.lower_expr(&arm.body),
      span: arm.span,
    }
  }

  pub fn lower_block(&mut self, block: &statements::Block) -> Block {
    let mut stmts = Vec::new();
    let mut trailing_expr = None;

    for (i, stmt) in block.statements.iter().enumerate() {
      let is_last = i == block.statements.len() - 1;

      match stmt {
        statements::Statement::Expr(expr_stmt)
          if is_last && !expr_stmt.has_semicolon =>
        {
          trailing_expr = Some(Box::new(self.lower_expr(&expr_stmt.expr)));
        }
        _ => {
          stmts.push(self.lower_stmt(stmt));
        }
      }
    }

    Block {
      hir_id: self.next_hir_id(),
      stmts,
      expr: trailing_expr,
      span: block.span,
    }
  }

  fn lower_stmt(&mut self, stmt: &statements::Statement) -> Stmt {
    let (kind, span) = match stmt {
      statements::Statement::VarDecl(var) => {
        let def_id =
          self.get_def_id(var.id).unwrap_or(zirael_resolver::DefId(0));

        let local = Local {
          hir_id: self.next_hir_id(),
          def_id,
          pat: Pat {
            hir_id: self.next_hir_id(),
            kind: PatKind::Binding {
              def_id,
              name: var.name,
              is_mut: var.is_mut,
              subpat: None,
            },
            span: *var.name.span(),
          },
          ty: var.ty.as_ref().map(|t| self.lower_type(t)),
          init: Some(self.lower_expr(&var.value)),
          span: var.span,
        };

        (StmtKind::Local(local), var.span)
      }

      statements::Statement::ConstDecl(c) => {
        let def_id = self.get_def_id(c.id).unwrap_or(zirael_resolver::DefId(0));

        let local = Local {
          hir_id: self.next_hir_id(),
          def_id,
          pat: Pat {
            hir_id: self.next_hir_id(),
            kind: PatKind::Binding {
              def_id,
              name: c.name,
              is_mut: false,
              subpat: None,
            },
            span: *c.name.span(),
          },
          ty: c.ty.as_ref().map(|t| self.lower_type(t)),
          init: Some(self.lower_expr(&c.value)),
          span: c.span,
        };

        (StmtKind::Local(local), c.span)
      }

      statements::Statement::Expr(expr_stmt) => {
        let expr = self.lower_expr(&expr_stmt.expr);
        if expr_stmt.has_semicolon {
          (StmtKind::Semi(expr), expr_stmt.span)
        } else {
          (StmtKind::Expr(expr), expr_stmt.span)
        }
      }

      statements::Statement::Block(block) => {
        let hir_block = self.lower_block(block);
        (
          StmtKind::Expr(Expr {
            hir_id: self.next_hir_id(),
            kind: ExprKind::Block(hir_block),
            span: block.span,
          }),
          block.span,
        )
      }
    };

    Stmt {
      hir_id: self.next_hir_id(),
      kind,
      span,
    }
  }

  pub(crate) fn lower_literal(&self, lit: &expressions::Literal) -> Literal {
    match lit {
      expressions::Literal::Int(i) => Literal::Int {
        value: i.value.clone(),
        base: i.base,
        suffix: i.suffix,
      },
      expressions::Literal::Float(f) => Literal::Float {
        value: f.value.clone(),
        suffix: f.suffix,
      },
      expressions::Literal::Bool(b) => Literal::Bool(b.value),
      expressions::Literal::Char(c) => Literal::Char(c.value),
      expressions::Literal::Byte(b) => Literal::Byte(b.value),
      expressions::Literal::String(s) => Literal::String(s.value.clone()),
      expressions::Literal::Unit(_) => Literal::Unit,
    }
  }

  pub(crate) fn get_literal_span(&self, lit: &expressions::Literal) -> Span {
    match lit {
      expressions::Literal::Int(i) => i.span,
      expressions::Literal::Float(f) => f.span,
      expressions::Literal::Bool(b) => b.span,
      expressions::Literal::Char(c) => c.span,
      expressions::Literal::Byte(b) => b.span,
      expressions::Literal::String(s) => s.span,
      expressions::Literal::Unit(u) => u.span,
    }
  }
}
