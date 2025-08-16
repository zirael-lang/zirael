use crate::{
    Ast, ScopeId, SymbolId,
    ast::{
        expr::Expr,
        types::{GenericParameter, Type},
    },
};
use id_arena::Id;
use std::path::PathBuf;
use zirael_utils::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub id: AstId,
    pub kind: ItemKind,
    pub attributes: Vec<Attribute>,
    pub name: Identifier,
    pub span: Span,
    pub symbol_id: Option<SymbolId>,
}
pub type AstId = Id<()>;

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Function(Function),
    Struct(StructDeclaration),
    Enum(EnumDeclaration),
    Import(ImportKind, Span),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportKind {
    Path(PathBuf),
    ExternalModule(Vec<Identifier>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: AstId,
    pub name: Identifier,
    pub modifiers: FunctionModifiers,
    pub signature: FunctionSignature,
    pub body: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionModifiers {
    pub is_async: bool,
    pub is_const: bool,
    pub is_extern: bool,
    pub abi: Option<Abi>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Abi(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub generics: Vec<GenericParameter>,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: Type,
    pub kind: ParameterKind,
    pub default_value: Option<Expr>,
    pub span: Span,
    pub symbol_id: Option<SymbolId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum ParameterKind {
    #[default]
    Plain,
    Variadic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: Identifier,
    pub args: Option<Vec<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub id: AstId,
    pub name: Identifier,
    pub generics: Vec<GenericParameter>,
    pub fields: Vec<StructField>,
    pub methods: Vec<Item>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Identifier,
    pub ty: Type,
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
    Struct(Vec<StructField>),
}
