use id_arena::Id;
use zirael_utils::prelude::{Identifier, Span};

mod relations;
pub mod scopes;
mod table;

use crate::{
    EnumVariant, EnumVariantData, Expr, FunctionModifiers, FunctionSignature, GenericParameter,
    StructField, Type,
};
pub use relations::*;
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
    Struct {
        fields: Vec<StructField>,
        generics: Vec<GenericParameter>,
        methods: Vec<SymbolId>,
    },
    TypeExtension {
        ty: Type,
        methods: Vec<SymbolId>,
    },
    Enum {
        generics: Vec<GenericParameter>,
        variants: Vec<SymbolId>,
        methods: Vec<SymbolId>,
    },
    EnumVariant {
        parent_enum: SymbolId,
        data: EnumVariantData,
    },
    Temporary {
        ty: Type,
        lifetime: TemporaryLifetime,
    },
}

impl SymbolKind {
    pub fn name(&self) -> &str {
        match self {
            Self::Variable { .. } => "variable",
            Self::Constant { .. } => "constant",
            Self::Function { .. } => "function",
            Self::Parameter { .. } => "parameter",
            Self::Struct { .. } => "struct",
            Self::Enum { .. } => "enum",
            Self::EnumVariant { .. } => "enum variant",
            Self::Temporary { .. } => "temporary",
            Self::TypeExtension { .. } => "type extension",
        }
    }

    pub fn is_value(&self) -> bool {
        match self {
            Self::Variable { .. } | Self::Parameter { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub canonical_symbol: SymbolId,
}
