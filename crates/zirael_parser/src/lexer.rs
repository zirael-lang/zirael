use crate::ast::Keyword;
use logos::Logos;
use std::{
    fmt,
    fmt::{Display, Formatter},
    ops::Range,
};

#[derive(Debug, Logos, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"/\*[^*]*\*+([^/*][^*]*\*+)*/")]
pub enum TokenKind {
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    #[token("fn", |_| Keyword::Fn)]
    #[token("class", |_| Keyword::Class)]
    #[token("enum", |_| Keyword::Enum)]
    #[token("import", |_| Keyword::Import)]
    #[token("var", |_| Keyword::Var)]
    #[token("extern", |_| Keyword::Extern)]
    #[token("const", |_| Keyword::Const)]
    #[token("async", |_| Keyword::Async)]
    #[token("int", |_| Keyword::Int)]
    #[token("float", |_| Keyword::Float)]
    #[token("bool", |_| Keyword::Bool)]
    #[token("string", |_| Keyword::String)]
    #[token("char", |_| Keyword::Char)]
    #[token("void", |_| Keyword::Void)]
    #[token("mut", |_| Keyword::Mut)]
    #[token("box", |_| Keyword::Box)]
    #[token("return", |_| Keyword::Return)]
    #[token("uint", |_| Keyword::Uint)]
    Keyword(Keyword),

    // Brackets and braces
    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token("[")]
    BracketOpen,

    #[token("]")]
    BracketClose,

    #[token("(")]
    ParenOpen,

    #[token(")")]
    ParenClose,

    // Basic punctuation
    #[token(".")]
    Dot,

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token("_", priority = 3)]
    Underscore,

    // Assignment and comparison operators
    #[token("=")]
    Equals,

    #[token("==")]
    EqualsEquals,

    #[token("!=")]
    NotEquals,

    #[token("<")]
    LessThan,

    #[token("<=")]
    LessThanOrEqual,

    #[token(">")]
    GreaterThan,

    #[token(">=")]
    GreaterThanOrEqual,

    // Arithmetic operators
    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("%")]
    Modulo,

    #[token("**")]
    Power,

    // Increment/decrement
    #[token("++")]
    Increment,

    #[token("--")]
    Decrement,

    // Assignment operators
    #[token("+=")]
    PlusEquals,

    #[token("-=")]
    MinusEquals,

    #[token("*=")]
    MultiplyEquals,

    #[token("/=")]
    DivideEquals,

    #[token("%=")]
    ModuloEquals,

    // Logical operators
    #[token("&&")]
    LogicalAnd,

    #[token("||")]
    LogicalOr,

    #[token("!")]
    LogicalNot,

    // Bitwise operators
    #[token("&")]
    BitwiseAnd,

    #[token("|")]
    BitwiseOr,

    #[token("^")]
    BitwiseXor,

    #[token("~")]
    BitwiseNot,

    #[token("<<")]
    LeftShift,

    #[token(">>")]
    RightShift,

    // Other common symbols
    #[token("?")]
    Question,

    #[token("#")]
    Hash,

    #[token("@")]
    At,

    #[token("$")]
    Dollar,

    #[token("->")]
    Arrow,

    #[token("=>")]
    FatArrow,

    #[token("::")]
    DoubleColon,

    #[regex(r"-?(?:0|[1-9]\d*)", |lex| lex.slice().parse::<i64>().unwrap())]
    Integer(i64),

    #[regex(r"-?(?:0|[1-9]\d*)\.\d+(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    #[regex(r"-?(?:0|[1-9]\d*)[eE][+-]?\d+", |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),

    #[regex(r#""([^"\\\x00-\x1F]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    String(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Identifier(String),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TokenKind::{
            Arrow, At, BitwiseAnd, BitwiseNot, BitwiseOr, BitwiseXor, Bool, BraceClose, BraceOpen,
            BracketClose, BracketOpen, Colon, Comma, Decrement, Divide, DivideEquals, Dollar, Dot,
            DoubleColon, Equals, EqualsEquals, FatArrow, Float, GreaterThan, GreaterThanOrEqual,
            Hash, Identifier, Increment, Integer, Keyword, LeftShift, LessThan, LessThanOrEqual,
            LogicalAnd, LogicalNot, LogicalOr, Minus, MinusEquals, Modulo, ModuloEquals, Multiply,
            MultiplyEquals, NotEquals, ParenClose, ParenOpen, Plus, PlusEquals, Power, Question,
            RightShift, Semicolon, String, Underscore,
        };

        match self {
            Bool(true) => write!(f, "true"),
            Bool(false) => write!(f, "false"),
            Keyword(keyword) => write!(f, "{keyword}"),
            BraceOpen => write!(f, "{{"),
            BraceClose => write!(f, "}}"),
            BracketOpen => write!(f, "["),
            BracketClose => write!(f, "]"),
            ParenOpen => write!(f, "("),
            ParenClose => write!(f, ")"),
            Dot => write!(f, "."),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            Colon => write!(f, ":"),
            Equals => write!(f, "="),
            EqualsEquals => write!(f, "=="),
            NotEquals => write!(f, "!="),
            LessThan => write!(f, "<"),
            LessThanOrEqual => write!(f, "<="),
            GreaterThan => write!(f, ">"),
            GreaterThanOrEqual => write!(f, ">="),
            Underscore => write!(f, "_"),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Multiply => write!(f, "*"),
            Divide => write!(f, "/"),
            Modulo => write!(f, "%"),
            Power => write!(f, "**"),
            Increment => write!(f, "++"),
            Decrement => write!(f, "--"),
            PlusEquals => write!(f, "+="),
            MinusEquals => write!(f, "-="),
            MultiplyEquals => write!(f, "*="),
            DivideEquals => write!(f, "/="),
            ModuloEquals => write!(f, "%="),
            LogicalAnd => write!(f, "&&"),
            LogicalOr => write!(f, "||"),
            LogicalNot => write!(f, "!"),
            BitwiseAnd => write!(f, "&"),
            BitwiseOr => write!(f, "|"),
            BitwiseXor => write!(f, "^"),
            BitwiseNot => write!(f, "~"),
            LeftShift => write!(f, "<<"),
            RightShift => write!(f, ">>"),
            Question => write!(f, "?"),
            Hash => write!(f, "#"),
            At => write!(f, "@"),
            Dollar => write!(f, "$"),
            Arrow => write!(f, "->"),
            FatArrow => write!(f, "=>"),
            DoubleColon => write!(f, "::"),
            Integer(n) => write!(f, "{n}"),
            Float(n) => write!(f, "{n}"),
            String(s) => write!(f, "{s}"),
            Identifier(ident) => write!(f, "{ident}"),
        }
    }
}

pub fn get_tokens(input: &str) -> Vec<Token> {
    let mut lexer = TokenKind::lexer(input);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        if let Ok(token) = token {
            tokens.push(Token { kind: token, span: lexer.span() });
        }
    }

    tokens
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
