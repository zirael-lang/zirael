#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    /// `-`
    Minus,
    /// `!`
    Not,
    /// `~`
    BitwiseNot,
    /// `*`
    Deref,
    /// `&x`
    Ref,
    /// `box x`
    Box,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    /// `+` - Addition
    Add,
    /// `-` - Subtraction
    Sub,
    /// `*` - Multiplication
    Mul,
    /// `/` - Division
    Div,
    /// `%` - Remainder
    Rem,
    /// `==` - Equality
    Eq,
    /// `!=` - Not equal
    Ne,
    /// `<` - Less than
    Lt,
    /// `<=` - Less than or equal
    Le,
    /// `>` - Greater than
    Gt,
    /// `>=` - Greater than or equal
    Ge,
    /// `&&` - Logical AND
    And,
    /// `||` - Logical OR
    Or,
    /// `&` - Bitwise AND
    BitAnd,
    /// `|` - Bitwise OR
    BitOr,
    /// `^` - Bitwise XOR
    BitXor,
    /// `<<` - Bitwise shift left
    Shl,
    /// `>>` - Bitwise shift right
    Shr,
}
