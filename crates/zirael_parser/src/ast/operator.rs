use strum::{EnumIter, EnumString};

#[derive(Debug, Clone, PartialEq, Eq, EnumIter, EnumString)]
pub enum UnaryOp {
  /// `-`
  #[strum(serialize = "-")]
  Minus,
  /// `!`
  #[strum(serialize = "!")]
  Not,
  /// `~`
  #[strum(serialize = "~")]
  BitwiseNot,
  /// `*`
  #[strum(serialize = "*")]
  Deref,
  /// `&x`
  #[strum(serialize = "&")]
  Ref,
  /// `box x`
  #[strum(serialize = "box")]
  Box,
}

#[derive(Debug, Clone, PartialEq, Eq, EnumIter, EnumString)]
pub enum BinaryOp {
  /// `+` - Addition
  #[strum(serialize = "+")]
  Add,
  /// `-` - Subtraction
  #[strum(serialize = "-")]
  Sub,
  /// `*` - Multiplication
  #[strum(serialize = "*")]
  Mul,
  /// `/` - Division
  #[strum(serialize = "/")]
  Div,
  /// `%` - Remainder
  #[strum(serialize = "%")]
  Rem,
  /// `==` - Equality
  #[strum(serialize = "==")]
  Eq,
  /// `!=` - Not equal
  #[strum(serialize = "!=")]
  Ne,
  /// `<` - Less than
  #[strum(serialize = "<")]
  Lt,
  /// `<=` - Less than or equal
  #[strum(serialize = "<=")]
  Le,
  /// `>` - Greater than
  #[strum(serialize = ">")]
  Gt,
  /// `>=` - Greater than or equal
  #[strum(serialize = ">=")]
  Ge,
  /// `&&` - Logical AND
  #[strum(serialize = "&&")]
  And,
  /// `||` - Logical OR
  #[strum(serialize = "||")]
  Or,
  /// `&` - Bitwise AND
  #[strum(serialize = "&")]
  BitAnd,
  /// `|` - Bitwise OR
  #[strum(serialize = "|")]
  BitOr,
  /// `^` - Bitwise XOR
  #[strum(serialize = "^")]
  BitXor,
  /// `<<` - Bitwise shift left
  #[strum(serialize = "<<")]
  Shl,
  /// `>>` - Bitwise shift right
  #[strum(serialize = ">>")]
  Shr,
}
