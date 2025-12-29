use crate::ast::NodeId;
use crate::ast::identifier::Ident;
use crate::ast::import::Path;
use crate::ast::statements::Block;
use crate::ast::types::{Type, TypePath};
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Expr {
  pub id: NodeId,
  pub kind: ExprKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
  // Literals
  Literal(Literal),

  // Identifiers and paths
  Ident(Ident),
  Path(PathExpr),
  SelfValue,

  // Binary operations
  Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },

  // Unary operations
  Unary { op: UnaryOp, operand: Box<Expr> },

  // Assignment
  Assign { op: AssignOp, target: Box<Expr>, value: Box<Expr> },

  // Ternary conditional
  Ternary { condition: Box<Expr>, then_expr: Box<Expr>, else_expr: Box<Expr> },

  // Type cast
  Cast { expr: Box<Expr>, target_type: Box<Type> },

  // Function call
  Call { callee: Box<Expr>, args: Vec<Argument> },

  // Field access
  Field { object: Box<Expr>, field: Ident },

  // Path qualifier (::)
  PathQualifier { base: Box<Expr>, segment: Ident, type_args: Option<Vec<Type>> },

  // Control flow
  If(IfExpr),
  Match(MatchExpr),
  Block(Block),

  // Grouping
  Paren(Box<Expr>),

  // Composite literals
  Tuple(Vec<Expr>),
  Array(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub struct PathExpr {
  pub id: NodeId,
  pub path: Path,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
  pub id: NodeId,
  pub condition: Box<Expr>,
  pub then_block: Block,
  pub else_branch: Option<ElseBranch>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ElseBranch {
  Block(Block),
  If(Box<IfExpr>),
}

#[derive(Debug, Clone)]
pub struct MatchExpr {
  pub id: NodeId,
  pub scrutinee: Box<Expr>,
  pub arms: Vec<MatchArm>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
  pub id: NodeId,
  pub pattern: Pattern,
  pub body: Expr,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Argument {
  pub id: NodeId,
  pub name: Option<Ident>,
  pub value: Expr,
  pub span: Span,
}

// Ops

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
  // Arithmetic
  Add,
  Sub,
  Mul,
  Div,
  Mod,

  // Comparison
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,

  // Logical
  And,
  Or,

  // Bitwise
  BitAnd,
  BitOr,
  BitXor,
  Shl,
  Shr,
}

impl BinaryOp {
  pub fn precedence(&self) -> u8 {
    match self {
      BinaryOp::Or => 12,
      BinaryOp::And => 11,
      BinaryOp::BitOr => 10,
      BinaryOp::BitXor => 9,
      BinaryOp::BitAnd => 8,
      BinaryOp::Eq | BinaryOp::Ne => 7,
      BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => 6,
      BinaryOp::Shl | BinaryOp::Shr => 5,
      BinaryOp::Add | BinaryOp::Sub => 4,
      BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 3,
    }
  }

  pub fn is_left_associative(&self) -> bool {
    true
  }
}

impl UnaryOp {
  pub fn precedence(&self) -> u8 {
    2
  }
}

impl AssignOp {
  pub fn precedence(&self) -> u8 {
    14
  }

  pub fn is_left_associative(&self) -> bool {
    false
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
  Not,    // !
  BitNot, // ~
  Neg,    // -
  Plus,   // +
  Deref,  // *
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
  Assign,       // =
  AddAssign,    // +=
  SubAssign,    // -=
  MulAssign,    // *=
  DivAssign,    // /=
  ModAssign,    // %=
  AndAssign,    // &&=
  OrAssign,     // ||=
  BitAndAssign, // &=
  BitOrAssign,  // |=
  BitXorAssign, // ^=
  ShlAssign,    // <<=
  ShrAssign,    // >>=
}

// Patterns
#[derive(Debug, Clone)]
pub enum Pattern {
  Wildcard(WildcardPat),
  Literal(Literal),
  Ident(Ident),
  Struct(StructPattern),
  Tuple(TuplePattern),
  Enum(EnumPattern),
}

#[derive(Debug, Clone)]
pub struct WildcardPat {
  pub id: NodeId,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructPattern {
  pub id: NodeId,
  pub path: TypePath,
  pub fields: Vec<StructPatternField>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StructPatternField {
  Full { name: Ident, pattern: Pattern },
  Shorthand(Ident),
}

#[derive(Debug, Clone)]
pub struct TuplePattern {
  pub id: NodeId,
  pub patterns: Vec<Pattern>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumPattern {
  pub id: NodeId,
  pub path: TypePath,
  pub patterns: Vec<Pattern>,
  pub span: Span,
}

// Literals
#[derive(Debug, Clone)]
pub enum Literal {
  Int(IntLit),
  Float(FloatLit),
  String(StringLit),
  Char(CharLit),
  Byte(ByteLit),
  Bool(BoolLit),
  Unit(UnitLit),
}

#[derive(Debug, Clone)]
pub struct UnitLit {
  pub id: NodeId,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IntLit {
  pub id: NodeId,
  pub value: String,
  pub base: IntBase,
  pub suffix: Option<IntSuffix>,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntBase {
  Decimal,
  Binary,
  Octal,
  Hexadecimal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntSuffix {
  I8,
  I16,
  I32,
  I64,
  I128,
  ISize,
  U8,
  U16,
  U32,
  U64,
  U128,
  USize,
}

#[derive(Debug, Clone)]
pub struct FloatLit {
  pub id: NodeId,
  pub value: String,
  pub suffix: Option<FloatSuffix>,
  pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatSuffix {
  F32,
  F64,
}

#[derive(Debug, Clone)]
pub struct StringLit {
  pub id: NodeId,
  pub value: String,
  pub raw: String,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CharLit {
  pub id: NodeId,
  pub value: char,
  pub raw: String,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ByteLit {
  pub id: NodeId,
  pub value: u8,
  pub raw: String,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BoolLit {
  pub id: NodeId,
  pub value: bool,
  pub span: Span,
}
