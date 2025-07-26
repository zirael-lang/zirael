use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    sync::OnceLock,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum Keyword {
    Fn,
    Class,
    Enum,
    Import,
    Var,

    Extern,
    Const,
    Async,

    // types
    Int,
    Float,
    Bool,
    String,
    Char,
    Void,
    Mut,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Keyword {
    pub const ALL: &'static [Self] = &[
        Self::Fn,
        Self::Class,
        Self::Enum,
        Self::Import,
        Self::Var,
        Self::Extern,
        Self::Const,
        Self::Async,
        Self::Int,
        Self::Float,
        Self::Bool,
        Self::String,
        Self::Char,
        Self::Void,
        Self::Mut,
    ];

    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Fn => "fn",
            Self::Class => "class",
            Self::Enum => "enum",
            Self::Import => "import",
            Self::Var => "var",
            Self::Extern => "extern",
            Self::Const => "const",
            Self::Async => "async",
            Self::Int => "int",
            Self::Float => "float",
            Self::Bool => "bool",
            Self::String => "string",
            Self::Char => "char",
            Self::Void => "void",
            Self::Mut => "mut",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        static KEYWORD_MAP: OnceLock<HashMap<&'static str, Keyword>> = OnceLock::new();

        let map = KEYWORD_MAP.get_or_init(|| {
            let mut map = HashMap::new();

            for &keyword in Self::ALL {
                map.insert(keyword.as_str(), keyword);
            }

            map
        });

        map.get(s).copied()
    }

    pub fn is_valid(s: &str) -> bool {
        Self::from_str(s).is_some()
    }

    pub const fn is_type(&self) -> bool {
        matches!(
            self,
            Self::Int | Self::Float | Self::Bool | Self::String | Self::Char | Self::Void
        )
    }

    pub const fn is_declaration(&self) -> bool {
        matches!(self, Self::Fn | Self::Class | Self::Enum | Self::Import | Self::Var)
    }

    pub const fn is_modifier(&self) -> bool {
        matches!(self, Self::Extern | Self::Const | Self::Async)
    }
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::from_str(s).ok_or(())
    }
}
