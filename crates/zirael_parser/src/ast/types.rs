use zirael_utils::prelude::*;

pub type TypeVarId = u32;

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
    Array(Box<Type>, Option<usize>),
    Function { params: Vec<Type>, return_type: Box<Type> },
    Named { name: Identifier, generics: Vec<Type> },

    Inferred,
    Error,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float)
    }
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
