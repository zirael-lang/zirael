use std::fmt;
use zirael_utils::prelude::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
  // Literals
  StringLiteral(String),   // "hello"
  ByteLiteral(u8),         // b'A'
  CharLiteral(char),       // 'x'
  IntegerLiteral(IntBase), // 123, 0xFF, 0b1010
  FloatLiteral(String),    // 3.14, 1e-10

  // Keywords
  // Control flow
  If,     // if
  Else,   // else
  While,  // while
  For,    // for
  Loop,   // loop
  Return, // return
  Match,  // match
  In,     // in

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

  Whitespace, // spaces, tabs, newlines (usually skipped)
  Eof,        // end of file
}

/// Integer literal base
#[derive(Debug, Clone, PartialEq)]
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
    Token { kind: token_type, span, lexeme }
  }
}

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      // Literals
      TokenType::StringLiteral(s) => write!(f, "string literal \"{}\"", s),
      TokenType::ByteLiteral(b) => write!(f, "byte literal b'{}'", *b as char),
      TokenType::CharLiteral(c) => write!(f, "character literal '{}'", c),
      TokenType::IntegerLiteral(_) => write!(f, "integer literal"),
      TokenType::FloatLiteral(s) => write!(f, "float literal {}", s),
      TokenType::Identifier(s) => write!(f, "identifier `{}`", s),
      TokenType::DocComment(_) => write!(f, "doc comment"),

      // Keywords - Control flow
      TokenType::If => write!(f, "keyword `if`"),
      TokenType::Else => write!(f, "keyword `else`"),
      TokenType::While => write!(f, "keyword `while`"),
      TokenType::For => write!(f, "keyword `for`"),
      TokenType::Loop => write!(f, "keyword `loop`"),
      TokenType::Return => write!(f, "keyword `return`"),
      TokenType::Match => write!(f, "keyword `match`"),
      TokenType::In => write!(f, "keyword `in`"),

      // Keywords - Declarations
      TokenType::Func => write!(f, "keyword `func`"),
      TokenType::Struct => write!(f, "keyword `struct`"),
      TokenType::Enum => write!(f, "keyword `enum`"),
      TokenType::Interface => write!(f, "keyword `interface`"),
      TokenType::Impl => write!(f, "keyword `impl`"),
      TokenType::Type => write!(f, "keyword `type`"),
      TokenType::Var => write!(f, "keyword `var`"),
      TokenType::Const => write!(f, "keyword `const`"),
      TokenType::Mut => write!(f, "keyword `mut`"),

      // Keywords - Modules
      TokenType::Mod => write!(f, "keyword `mod`"),
      TokenType::Import => write!(f, "keyword `import`"),
      TokenType::As => write!(f, "keyword `as`"),
      TokenType::Package => write!(f, "keyword `package`"),
      TokenType::SelfValue => write!(f, "keyword `self`"),
      TokenType::Super => write!(f, "keyword `super`"),

      // Keywords - Visibility
      TokenType::Pub => write!(f, "keyword `pub`"),

      // Keywords - Literals
      TokenType::True => write!(f, "keyword `true`"),
      TokenType::False => write!(f, "keyword `false`"),

      // Keywords - Reserved
      TokenType::Await => write!(f, "keyword `await`"),
      TokenType::Async => write!(f, "keyword `async`"),

      // Punctuation
      TokenType::LeftParen => write!(f, "`(`"),
      TokenType::RightParen => write!(f, "`)`"),
      TokenType::LeftBrace => write!(f, "`{{`"),
      TokenType::RightBrace => write!(f, "`}}`"),
      TokenType::LeftBracket => write!(f, "`[`"),
      TokenType::RightBracket => write!(f, "`]`"),
      TokenType::Comma => write!(f, "`,`"),
      TokenType::Semicolon => write!(f, "`;`"),
      TokenType::Colon => write!(f, "`:`"),
      TokenType::Dot => write!(f, "`.`"),
      TokenType::Hash => write!(f, "`#`"),
      TokenType::At => write!(f, "`@`"),
      TokenType::Underscore => write!(f, "`_`"),
      TokenType::DotDotDot => write!(f, "`...`"),

      // Operators - Arithmetic
      TokenType::Plus => write!(f, "`+`"),
      TokenType::Minus => write!(f, "`-`"),
      TokenType::Star => write!(f, "`*`"),
      TokenType::Slash => write!(f, "`/`"),
      TokenType::Percent => write!(f, "`%`"),

      // Operators - Assignment
      TokenType::Assign => write!(f, "`=`"),
      TokenType::PlusAssign => write!(f, "`+=`"),
      TokenType::MinusAssign => write!(f, "`-=`"),
      TokenType::StarAssign => write!(f, "`*=`"),
      TokenType::SlashAssign => write!(f, "`/=`"),
      TokenType::PercentAssign => write!(f, "`%=`"),
      TokenType::AmpAssign => write!(f, "`&=`"),
      TokenType::PipeAssign => write!(f, "`|=`"),
      TokenType::CaretAssign => write!(f, "`^=`"),
      TokenType::ShlAssign => write!(f, "`<<=`"),
      TokenType::ShrAssign => write!(f, "`>>=`"),
      TokenType::AndAssign => write!(f, "`&&=`"),
      TokenType::OrAssign => write!(f, "`||=`"),

      // Operators - Comparison
      TokenType::EqEq => write!(f, "`==`"),
      TokenType::NotEq => write!(f, "`!=`"),
      TokenType::Lt => write!(f, "`<`"),
      TokenType::LtEq => write!(f, "`<=`"),
      TokenType::Gt => write!(f, "`>`"),
      TokenType::GtEq => write!(f, "`>=`"),

      // Operators - Logical
      TokenType::AndAnd => write!(f, "`&&`"),
      TokenType::OrOr => write!(f, "`||`"),
      TokenType::Not => write!(f, "`!`"),

      // Operators - Bitwise
      TokenType::Amp => write!(f, "`&`"),
      TokenType::Pipe => write!(f, "`|`"),
      TokenType::Caret => write!(f, "`^`"),
      TokenType::Tilde => write!(f, "`~`"),
      TokenType::Shl => write!(f, "`<<`"),
      TokenType::Shr => write!(f, "`>>`"),

      // Other
      TokenType::Question => write!(f, "`?`"),
      TokenType::Arrow => write!(f, "`->`"),
      TokenType::ColonColon => write!(f, "`::`"),
      TokenType::Whitespace => write!(f, "whitespace"),
      TokenType::Eof => write!(f, "end of file"),
    }
  }
}
