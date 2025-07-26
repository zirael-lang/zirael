pub mod keyword;

use colored::Colorize;
use id_arena::Id;
use std::{
    fmt,
    fmt::{Debug, Formatter},
    path::PathBuf,
};
use zirael_utils::prelude::*;

#[derive(Clone, Debug)]
pub struct Ast {
    pub items: Vec<Item>,
}

impl Ast {
    pub fn new(items: Vec<Item>) -> Self {
        Self { items }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(Identifier),
    Binary { left: Box<Expr>, op: BinaryOp, right: Box<Expr> },
    Block(Vec<Stmt>),
    Assign(Box<Expr>, Box<Expr>),
    AssignOp(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(Box<UnaryOp>, Box<Expr>),
    Paren(Box<Expr>),
    Call { callee: Box<Expr>, args: Vec<Expr> },
    FieldAccess(Vec<Expr>),

    CouldntParse(CouldntParse),
}

impl ExprKind {
    pub fn couldnt_parse() -> Self {
        Self::CouldntParse(CouldntParse)
    }
}

#[derive(Clone, PartialEq)]
pub struct CouldntParse;

impl Debug for CouldntParse {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", "CouldntParse".bright_red().bold().underline())
    }
}

#[derive(Clone, Debug, PartialEq)]
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr(pub ExprKind);

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt(pub StmtKind);

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Expr(Expr),
    Var(VarDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub name: Identifier,
    /// Defaults to [Type::Inferred]
    pub ty: Type,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    String,
    Char,
    Int,
    Float,
    Bool,
    Void,

    Pointer(Box<Type>),

    Reference(Box<Type>),
    MutableReference(Box<Type>),

    Array(Box<Type>, Option<usize>),

    Function { params: Vec<Type>, return_type: Box<ReturnType> },

    Named { name: Identifier, generics: Vec<Type> },

    // Type placeholder for inference
    Inferred,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub attributes: Vec<Attribute>,
    pub name: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Function(Function),
    Class(ClassDeclaration),
    Enum(EnumDeclaration),
    Import(ImportKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportKind {
    Path(PathBuf),
    ExternalModule(Vec<Identifier>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Identifier,
    pub modifiers: FunctionModifiers,
    pub signature: FunctionSignature,
    pub body: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionModifiers {
    pub is_async: bool,
    pub is_const: bool,
    pub is_extern: bool,
    pub abi: Option<Abi>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Abi(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub generics: Vec<GenericParameter>,
    pub parameters: Vec<Parameter>,
    pub return_type: ReturnType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnType {
    Default,
    Type(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: Type,
    pub kind: ParameterKind,
    pub default_value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum ParameterKind {
    #[default]
    Plain,
    Variadic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParameter {
    pub name: Identifier,
    pub constraints: Vec<TraitBound>,
    pub default_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitBound {
    pub name: Identifier,
    pub generic_args: Vec<GenericArg>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericArg {
    Type(Type),
    Named { name: Identifier, ty: Type },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: Identifier,
    pub args: Option<Vec<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDeclaration {
    pub name: Identifier,
    pub generics: Vec<GenericParameter>,
    pub fields: Vec<ClassField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassField {
    pub name: Identifier,
    pub field_type: Type,
    pub is_public: bool,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDeclaration {
    pub name: Identifier,
    pub generics: Option<Vec<GenericParameter>>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: Identifier,
    pub data: EnumVariantData,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantData {
    Unit,
    Tuple(Vec<Type>),
    Class(Vec<ClassField>),
}

#[derive(Debug, Clone, PartialEq)]
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
