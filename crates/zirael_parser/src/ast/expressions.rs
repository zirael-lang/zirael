use crate::ast::NodeId;
use crate::ast::import::Path;
use crate::ast::statements::Block;
use crate::ast::types::{Mutability, Type};
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone)]
pub struct Expr {
  pub id: NodeId,
  pub kind: ExprKind,
  pub span: Span,
  pub is_const: bool,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
  // Literals
  Literal(Literal),

  // Identifiers and paths
  Path(Path),
  SelfValue,

  // Binary operations
  Binary {
    op: BinaryOp,
    left: Box<Expr>,
    right: Box<Expr>,
  },

  // Unary operations
  Unary {
    op: UnaryOp,
    operand: Box<Expr>,
  },

  // Assignment
  Assign {
    op: AssignOp,
    target: Box<Expr>,
    value: Box<Expr>,
  },

  // Ternary conditional
  Ternary {
    condition: Box<Expr>,
    then_expr: Box<Expr>,
    else_expr: Box<Expr>,
  },

  // Type cast
  Cast {
    expr: Box<Expr>,
    target_type: Box<Type>,
  },

  // Function call
  Call {
    callee: Box<Expr>,
    args: Vec<Argument>,
  },

  // Field access
  Field {
    object: Box<Expr>,
    field: Identifier,
  },

  Index {
    object: Box<Expr>,
    index: Box<Expr>,
  },

  AddrOf {
    mutability: Mutability,
    operand: Box<Expr>,
  },

  Struct {
    path: Path,
    fields: Vec<StructFieldInit>,
  },

  // Control flow
  If(IfExpr),
  Match(MatchExpr),
  Block(Block),
  Loop(LoopExpr),
  While(WhileExpr),
  For(ForExpr),
  Break(BreakExpr),
  Continue(ContinueExpr),
  Return(ReturnExpr),

  Range(RangeExpr),
  Builtin {
    name: Identifier,
    args: Vec<BuiltinArg>,
  },

  // Composite literals
  Tuple(Vec<Expr>),
  Array(Vec<Expr>),
}

impl Expr {
  pub fn new(kind: ExprKind, span: Span) -> Self {
    Self {
      id: NodeId::new(),
      kind,
      span,
      is_const: false,
    }
  }

  pub fn new_const(kind: ExprKind, span: Span) -> Self {
    Self {
      id: NodeId::new(),
      kind,
      span,
      is_const: true,
    }
  }

  pub fn dummy() -> Self {
    Self {
      id: NodeId::new(),
      kind: ExprKind::Literal(Literal::Unit(UnitLit {
        id: NodeId::new(),
        span: Span::default(),
      })),
      span: Span::default(),
      is_const: false,
    }
  }
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
pub struct StructFieldInit {
  pub id: NodeId,
  pub name: Identifier, // .field
  pub value: Expr,      // = value
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MatchExpr {
  pub id: NodeId,
  pub scrutinee: Box<Expr>,
  pub arms: Vec<MatchArm>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
  pub id: NodeId,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileExpr {
  pub id: NodeId,
  pub condition: Box<Expr>,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForExpr {
  pub id: NodeId,
  pub binding: Identifier,
  pub iterator: Box<Expr>,
  pub body: Block,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BreakExpr {
  pub id: NodeId,
  pub value: Option<Box<Expr>>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ContinueExpr {
  pub id: NodeId,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnExpr {
  pub id: NodeId,
  pub value: Option<Box<Expr>>,
  pub span: Span,
}

// Range expressions: start..end, start..=end, ..end, start.., ..
#[derive(Debug, Clone)]
pub struct RangeExpr {
  pub id: NodeId,
  pub start: Option<Box<Expr>>,
  pub end: Option<Box<Expr>>,
  pub inclusive: bool, // .. vs ..=
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum BuiltinArg {
  Type(Type),
  Expr(Expr),
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
  pub name: Option<Identifier>,
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
      Self::Or => 12,
      Self::And => 11,
      Self::BitOr => 10,
      Self::BitXor => 9,
      Self::BitAnd => 8,
      Self::Eq | Self::Ne => 7,
      Self::Lt | Self::Le | Self::Gt | Self::Ge => 6,
      Self::Shl | Self::Shr => 5,
      Self::Add | Self::Sub => 4,
      Self::Mul | Self::Div | Self::Mod => 3,
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
  Ident(Identifier),
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
  pub path: Path,
  pub fields: Vec<StructPatternField>,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StructPatternField {
  Full { name: Identifier, pattern: Pattern },
  Shorthand(Identifier),
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
  pub path: Path,
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

impl IntSuffix {
  pub fn parse_int_suffix(lexeme: &str, value: &str) -> Option<Self> {
    let suffix_str = lexeme.strip_prefix(value).unwrap_or("");
    let suffix_str = suffix_str.trim_start_matches(|c: char| {
      c == 'x'
        || c == 'X'
        || c == 'b'
        || c == 'B'
        || c == 'o'
        || c == 'O'
        || c.is_ascii_hexdigit()
        || c == '_'
    });

    match suffix_str {
      "i8" => Some(Self::I8),
      "i16" => Some(Self::I16),
      "i32" => Some(Self::I32),
      "i64" => Some(Self::I64),
      "i128" => Some(Self::I128),
      "isize" => Some(Self::ISize),
      "u8" => Some(Self::U8),
      "u16" => Some(Self::U16),
      "u32" => Some(Self::U32),
      "u64" => Some(Self::U64),
      "u128" => Some(Self::U128),
      "usize" => Some(Self::USize),
      _ => None,
    }
  }
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
