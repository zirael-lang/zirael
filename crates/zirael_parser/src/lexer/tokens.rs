use std::fmt;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
  // Literals
  StringLiteral(String),   // "hello"
  ByteLiteral(u8),         // b'A'
  CharLiteral(char),       // 'x'
  IntegerLiteral(IntBase), // 123, 0xFF, 0b1010
  FloatLiteral(String),    // 3.14, 1e-10

  // Keywords
  // Control flow
  If,       // if
  Else,     // else
  While,    // while
  For,      // for
  Loop,     // loop
  Return,   // return
  Match,    // match
  In,       // in
  Break,    // break
  Continue, // continue

  // Declarations
  Func,      // func
  Struct,    // struct
  Enum,      // enum
  Interface, // interface
  Impl,      // impl
  Type,      // type
  Var,       // var
  Const,     // const
  Mut,       // mut

  // Modules
  Mod,       // mod
  Import,    // import
  As,        // as
  Package,   // package
  SelfValue, // self
  Super,     // super
  From,      // from

  // Visibility
  Pub, // pub

  // Literals (boolean)
  True,  // true
  False, // false

  // Reserved Keywords
  Await, // await
  Async, // async

  // Identifiers
  Identifier(String),

  // Comments
  DocComment(String),

  // Punctuation
  LeftParen,    // (
  RightParen,   // )
  LeftBrace,    // {
  RightBrace,   // }
  LeftBracket,  // [
  RightBracket, // ]
  Comma,        // ,
  Semicolon,    // ;
  Colon,        // :
  Dot,          // .
  DotDot,       // ..
  DotDotEq,     // ..=
  Hash,         // #
  At,           // @
  Underscore,   // _
  DotDotDot,    // ...

  // Operators
  // Arithmetic
  Plus,    // +
  Minus,   // -
  Star,    // *
  Slash,   // /
  Percent, // %

  // Assignment
  Assign,        // =
  PlusAssign,    // +=
  MinusAssign,   // -=
  StarAssign,    // *=
  SlashAssign,   // /=
  PercentAssign, // %=
  AmpAssign,     // &=
  PipeAssign,    // |=
  CaretAssign,   // ^=
  ShlAssign,     // <<=
  ShrAssign,     // >>=
  AndAssign,     // &&=
  OrAssign,      // ||=

  // Comparison
  EqEq,  // ==
  NotEq, // !=
  Lt,    // <
  LtEq,  // <=
  Gt,    // >
  GtEq,  // >=

  // Logical
  AndAnd, // &&
  OrOr,   // ||
  Not,    // !

  // Bitwise
  Amp,   // &
  Pipe,  // |
  Caret, // ^
  Tilde, // ~
  Shl,   // <<
  Shr,   // >>

  Question,   // ?
  Arrow,      // ->
  ColonColon, // ::

  Whitespace,
  Eof, // end of file
}

/// Integer literal base
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntBase {
  Decimal(String),     // 123
  Binary(String),      // 0b1010
  Octal(String),       // 0o77
  Hexadecimal(String), // 0xFF
}

#[derive(Debug, Clone)]
pub struct Token {
  pub kind: TokenType,
  pub span: Span,
  pub lexeme: String,
}

impl Token {
  pub fn new(token_type: TokenType, span: Span, lexeme: String) -> Self {
    Self {
      kind: token_type,
      span,
      lexeme,
    }
  }
}

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      // Literals
      Self::StringLiteral(s) => write!(f, "string literal \"{s}\""),
      Self::ByteLiteral(b) => write!(f, "byte literal b'{}'", *b as char),
      Self::CharLiteral(c) => write!(f, "character literal '{c}'"),
      Self::IntegerLiteral(_) => write!(f, "integer literal"),
      Self::FloatLiteral(s) => write!(f, "float literal {s}"),
      Self::Identifier(s) => write!(f, "identifier `{s}`"),
      Self::DocComment(_) => write!(f, "doc comment"),

      // Keywords - Control flow
      Self::If => write!(f, "keyword `if`"),
      Self::Else => write!(f, "keyword `else`"),
      Self::While => write!(f, "keyword `while`"),
      Self::For => write!(f, "keyword `for`"),
      Self::Loop => write!(f, "keyword `loop`"),
      Self::Return => write!(f, "keyword `return`"),
      Self::Match => write!(f, "keyword `match`"),
      Self::In => write!(f, "keyword `in`"),
      Self::Break => write!(f, "keyword `break`"),
      Self::Continue => write!(f, "keyword `continue`"),

      // Keywords - Declarations
      Self::Func => write!(f, "keyword `func`"),
      Self::Struct => write!(f, "keyword `struct`"),
      Self::Enum => write!(f, "keyword `enum`"),
      Self::Interface => write!(f, "keyword `interface`"),
      Self::Impl => write!(f, "keyword `impl`"),
      Self::Type => write!(f, "keyword `type`"),
      Self::Var => write!(f, "keyword `var`"),
      Self::Const => write!(f, "keyword `const`"),
      Self::Mut => write!(f, "keyword `mut`"),

      // Keywords - Modules
      Self::Mod => write!(f, "keyword `mod`"),
      Self::Import => write!(f, "keyword `import`"),
      Self::As => write!(f, "keyword `as`"),
      Self::Package => write!(f, "keyword `package`"),
      Self::SelfValue => write!(f, "keyword `self`"),
      Self::Super => write!(f, "keyword `super`"),
      Self::From => write!(f, "keyword `from`"),

      // Keywords - Visibility
      Self::Pub => write!(f, "keyword `pub`"),

      // Keywords - Literals
      Self::True => write!(f, "keyword `true`"),
      Self::False => write!(f, "keyword `false`"),

      // Keywords - Reserved
      Self::Await => write!(f, "keyword `await`"),
      Self::Async => write!(f, "keyword `async`"),

      // Punctuation
      Self::LeftParen => write!(f, "`(`"),
      Self::RightParen => write!(f, "`)`"),
      Self::LeftBrace => write!(f, "`{{`"),
      Self::RightBrace => write!(f, "`}}`"),
      Self::LeftBracket => write!(f, "`[`"),
      Self::RightBracket => write!(f, "`]`"),
      Self::Comma => write!(f, "`,`"),
      Self::Semicolon => write!(f, "`;`"),
      Self::Colon => write!(f, "`:`"),
      Self::Dot => write!(f, "`.`"),
      Self::DotDot => write!(f, "`..`"),
      Self::DotDotEq => write!(f, "`..=`"),
      Self::Hash => write!(f, "`#`"),
      Self::At => write!(f, "`@`"),
      Self::Underscore => write!(f, "`_`"),
      Self::DotDotDot => write!(f, "`...`"),

      // Operators - Arithmetic
      Self::Plus => write!(f, "`+`"),
      Self::Minus => write!(f, "`-`"),
      Self::Star => write!(f, "`*`"),
      Self::Slash => write!(f, "`/`"),
      Self::Percent => write!(f, "`%`"),

      // Operators - Assignment
      Self::Assign => write!(f, "`=`"),
      Self::PlusAssign => write!(f, "`+=`"),
      Self::MinusAssign => write!(f, "`-=`"),
      Self::StarAssign => write!(f, "`*=`"),
      Self::SlashAssign => write!(f, "`/=`"),
      Self::PercentAssign => write!(f, "`%=`"),
      Self::AmpAssign => write!(f, "`&=`"),
      Self::PipeAssign => write!(f, "`|=`"),
      Self::CaretAssign => write!(f, "`^=`"),
      Self::ShlAssign => write!(f, "`<<=`"),
      Self::ShrAssign => write!(f, "`>>=`"),
      Self::AndAssign => write!(f, "`&&=`"),
      Self::OrAssign => write!(f, "`||=`"),

      // Operators - Comparison
      Self::EqEq => write!(f, "`==`"),
      Self::NotEq => write!(f, "`!=`"),
      Self::Lt => write!(f, "`<`"),
      Self::LtEq => write!(f, "`<=`"),
      Self::Gt => write!(f, "`>`"),
      Self::GtEq => write!(f, "`>=`"),

      // Operators - Logical
      Self::AndAnd => write!(f, "`&&`"),
      Self::OrOr => write!(f, "`||`"),
      Self::Not => write!(f, "`!`"),

      // Operators - Bitwise
      Self::Amp => write!(f, "`&`"),
      Self::Pipe => write!(f, "`|`"),
      Self::Caret => write!(f, "`^`"),
      Self::Tilde => write!(f, "`~`"),
      Self::Shl => write!(f, "`<<`"),
      Self::Shr => write!(f, "`>>`"),

      // Other
      Self::Question => write!(f, "`?`"),
      Self::Arrow => write!(f, "`->`"),
      Self::ColonColon => write!(f, "`::`"),
      Self::Whitespace => write!(f, "whitespace"),
      Self::Eof => write!(f, "end of file"),
    }
  }
}
