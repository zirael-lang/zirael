use id_arena::Id;
use zirael_utils::prelude::{Identifier, Span};

pub mod scopes;
mod table;

use crate::{
    ClassField, EnumVariant, Expr, FunctionModifiers, FunctionSignature, GenericParameter, Type,
};
pub use scopes::*;
pub use table::*;

pub type SymbolId = Id<Symbol>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct VariableMove {
    pub from: Span,
    pub to: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Variable {
        ty: Type,
        is_heap: bool,                  // allocated with `box`
        is_moved: Option<VariableMove>, // ownership has been moved
    },
    Constant {
        ty: Type,
        value: Option<Expr>,
    },
    Function {
        signature: FunctionSignature,
        modifiers: FunctionModifiers,
    },
    Parameter {
        ty: Type,
        is_variadic: bool,
        default_value: Option<Expr>,
    },
    Class {
        fields: Vec<ClassField>,
        generics: Vec<GenericParameter>,
    },
    Enum {
        generics: Option<Vec<GenericParameter>>,
        variants: Vec<EnumVariant>,
    },
    Temporary {
        ty: Type,
        lifetime: TemporaryLifetime,
    },
}

impl SymbolKind {
    pub fn name(&self) -> &str {
        match self {
            SymbolKind::Variable { .. } => "variable",
            SymbolKind::Constant { .. } => "constant",
            SymbolKind::Function { .. } => "function",
            SymbolKind::Parameter { .. } => "parameter",
            SymbolKind::Class { .. } => "class",
            SymbolKind::Enum { .. } => "enum",
            SymbolKind::Temporary { .. } => "temporary",
        }
    }

    pub fn is_value(&self) -> bool {
        match self {
            SymbolKind::Variable { .. } | SymbolKind::Parameter { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemporaryLifetime {
    Expression,
    Statement,
    Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: Identifier,
    pub kind: SymbolKind,
    pub scope: ScopeId,
    pub source_location: Option<Span>,
    pub is_used: bool,
    pub declaration_order: usize,
    pub imported_from: Option<ScopeId>,
}
