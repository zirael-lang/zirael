use std::fmt::{Display, Formatter};
use strum::{EnumIter, EnumProperty, EnumString, IntoEnumIterator as _};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy, EnumIter, EnumString, EnumProperty)]
#[strum(serialize_all = "lowercase")]
pub enum Keyword {
    #[strum(props(category = "declaration"))]
    Fn,
    #[strum(props(category = "declaration"))]
    Struct,
    #[strum(props(category = "declaration"))]
    Extension,
    #[strum(props(category = "declaration"))]
    Enum,
    #[strum(props(category = "declaration"))]
    Import,
    #[strum(props(category = "declaration"))]
    Mod,
    #[strum(props(category = "declaration"))]
    Var,
    #[strum(props(category = "control"))]
    Box,
    #[strum(props(category = "control"))]
    Return,
    #[strum(props(category = "control"))]
    Match,

    #[strum(props(category = "modifier"))]
    Extern,
    #[strum(props(category = "modifier"))]
    Const,
    #[strum(props(category = "modifier"))]
    Async,

    // types
    #[strum(props(category = "type"))]
    Int,
    #[strum(props(category = "type"))]
    Uint,
    #[strum(props(category = "type"))]
    Float,
    #[strum(props(category = "type"))]
    Bool,
    #[strum(props(category = "type"))]
    String,
    #[strum(props(category = "type"))]
    Char,
    #[strum(props(category = "type"))]
    Void,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Keyword {
    pub fn all() -> impl Iterator<Item = Self> {
        Self::iter()
    }

    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Fn => "fn",
            Self::Struct => "struct",
            Self::Extension => "extension",
            Self::Enum => "enum",
            Self::Import => "import",
            Self::Mod => "mod",
            Self::Var => "var",
            Self::Box => "box",
            Self::Return => "return",
            Self::Match => "match",
            Self::Extern => "extern",
            Self::Const => "const",
            Self::Async => "async",
            Self::Int => "int",
            Self::Uint => "uint",
            Self::Float => "float",
            Self::Bool => "bool",
            Self::String => "string",
            Self::Char => "char",
            Self::Void => "void",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        s.parse().ok()
    }

    pub fn is_valid(s: &str) -> bool {
        s.parse::<Self>().is_ok()
    }

    pub fn types() -> impl Iterator<Item = Self> {
        Self::iter().filter(|keyword| keyword.get_str("category") == Some("type"))
    }

    pub fn declarations() -> impl Iterator<Item = Self> {
        Self::iter().filter(|keyword| keyword.get_str("category") == Some("declaration"))
    }

    pub fn control() -> impl Iterator<Item = Self> {
        Self::iter().filter(|keyword| keyword.get_str("category") == Some("control"))
    }

    pub fn modifier() -> impl Iterator<Item = Self> {
        Self::iter().filter(|keyword| keyword.get_str("category") == Some("modifier"))
    }
}
