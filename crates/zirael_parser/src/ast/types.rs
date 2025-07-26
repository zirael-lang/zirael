use zirael_utils::prelude::*;

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
pub enum ReturnType {
    Default,
    Type(Type),
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
