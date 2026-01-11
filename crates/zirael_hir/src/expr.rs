use crate::ids::HirId;
use crate::pat::Pat;
use crate::ty::Ty;
use zirael_parser::ast::expressions::{AssignOp, BinaryOp, UnaryOp};
use zirael_parser::ast::types::Mutability;
use zirael_parser::{FloatSuffix, IntSuffix, expressions::IntBase};
use zirael_resolver::DefId;
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Expr {
  pub hir_id: HirId,
  pub kind: ExprKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
  Literal(Literal),

  Path(PathExpr),

  Binary {
    op: BinaryOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
  },

  Unary {
    op: UnaryOp,
    operand: Box<Expr>,
  },

  Assign {
    op: AssignOp,
    target: Box<Expr>,
    value: Box<Expr>,
  },

  /// Type cast: `x as i32`
  Cast {
    expr: Box<Expr>,
    ty: Ty,
  },

  Call {
    callee: Box<Expr>,
    args: Vec<Expr>,
  },

  MethodCall {
    receiver: Box<Expr>,
    method: Identifier,
    args: Vec<Expr>,
  },

  /// Field access: `obj.field`
  Field {
    object: Box<Expr>,
    field: Identifier,
  },

  /// Index access: `arr[i]`
  Index {
    object: Box<Expr>,
    index: Box<Expr>,
  },

  /// Address-of: `&x`, `&mut x`
  AddrOf {
    mutability: Mutability,
    operand: Box<Expr>,
  },

  Struct {
    def_id: DefId,
    fields: Vec<FieldInit>,
  },

  /// Tuple: `(a, b, c)`
  Tuple(Vec<Expr>),

  /// Array: `[1, 2, 3]`
  Array(Vec<Expr>),

  Block(Block),

  If {
    condition: Box<Expr>,
    then_block: Block,
    else_branch: Option<Box<Expr>>,
  },

  Match {
    scrutinee: Box<Expr>,
    arms: Vec<MatchArm>,
  },

  /// Loop: `loop { }`
  Loop {
    body: Block,
  },

  /// Break: `break` or `break value`
  Break {
    value: Option<Box<Expr>>,
  },
  Continue,
  Return {
    value: Option<Box<Expr>>,
  },

  Err,
}

#[derive(Debug, Clone)]
pub struct PathExpr {
  pub def_id: DefId,
  pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
  pub name: Identifier,
  pub args: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct FieldInit {
  pub hir_id: HirId,
  pub name: Identifier,
  pub value: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
  pub hir_id: HirId,
  pub stmts: Vec<Stmt>,
  pub expr: Option<Box<Expr>>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Stmt {
  pub hir_id: HirId,
  pub kind: StmtKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
  Local(Local),
  Expr(Expr),
  /// Expression without semicolon (only valid as last stmt in block)
  Semi(Expr),
}

#[derive(Debug, Clone)]
pub struct Local {
  pub hir_id: HirId,
  pub def_id: DefId,
  pub pat: Pat,
  pub ty: Option<Ty>,
  pub init: Option<Expr>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
  pub hir_id: HirId,
  pub pat: Pat,
  pub guard: Option<Expr>,
  pub body: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Literal {
  Int {
    value: String,
    base: IntBase,
    suffix: Option<IntSuffix>,
  },
  Float {
    value: String,
    suffix: Option<FloatSuffix>,
  },
  Bool(bool),
  Char(char),
  Byte(u8),
  String(String),
  Unit,
}
