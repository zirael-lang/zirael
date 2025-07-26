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
    Mut
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Keyword {
    pub const ALL: &'static [Keyword] = &[
        Keyword::Fn,
        Keyword::Class,
        Keyword::Enum,
        Keyword::Import,
        Keyword::Var,
        Keyword::Extern,
        Keyword::Const,
        Keyword::Async,
        Keyword::Int,
        Keyword::Float,
        Keyword::Bool,
        Keyword::String,
        Keyword::Char,
        Keyword::Void,
        Keyword::Mut,
    ];

    pub const fn as_str(&self) -> &'static str {
        match self {
            Keyword::Fn => "fn",
            Keyword::Class => "class",
            Keyword::Enum => "enum",
            Keyword::Import => "import",
            Keyword::Var => "var",
            Keyword::Extern => "extern",
            Keyword::Const => "const",
            Keyword::Async => "async",
            Keyword::Int => "int",
            Keyword::Float => "float",
            Keyword::Bool => "bool",
            Keyword::String => "string",
            Keyword::Char => "char",
            Keyword::Void => "void",
            Keyword::Mut => "mut",
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
            Keyword::Int
                | Keyword::Float
                | Keyword::Bool
                | Keyword::String
                | Keyword::Char
                | Keyword::Void
        )
    }

    pub const fn is_declaration(&self) -> bool {
        matches!(
            self,
            Keyword::Fn | Keyword::Class | Keyword::Enum | Keyword::Import | Keyword::Var
        )
    }

    pub const fn is_modifier(&self) -> bool {
        matches!(self, Keyword::Extern | Keyword::Const | Keyword::Async)
    }
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Self::from_str(s).ok_or(())
    }
}
