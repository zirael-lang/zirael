use crate::ast::{
    expr::Expr,
    types::{GenericParameter, ReturnType, Type},
};
use std::path::PathBuf;
use zirael_utils::prelude::*;

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
