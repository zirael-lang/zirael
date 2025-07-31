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
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}
