use crate::{Path, TokenType};
use std::fmt;
use std::fmt::{Display, Formatter};
use zirael_diagnostic_macro::Diagnostic;
use zirael_source::span::Span;

#[derive(Diagnostic)]
#[error("{expected} {context}, but found {found}")]
#[code(PARSE_UNEXPECTED_TOKEN)]
pub struct UnexpectedToken {
  pub expected: ExpectedTokens,
  pub found: TokenType,
  pub context: String,

  #[error("found here")]
  pub span: Span,
}

#[derive(Debug)]
pub struct ExpectedTokens(Vec<TokenType>);

impl ExpectedTokens {
  pub fn one(token: TokenType) -> Self {
    Self(vec![token])
  }

  pub fn many(tokens: Vec<TokenType>) -> Self {
    Self(tokens)
  }
}

impl Display for ExpectedTokens {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self.0.as_slice() {
      [t] => write!(f, "expected {t}"),
      ts => {
        f.write_str("expected one of: ")?;
        for (i, t) in ts.iter().enumerate() {
          if i != 0 {
            f.write_str(", ")?;
          }
          write!(f, "{t}")?;
        }
        Ok(())
      }
    }
  }
}

#[derive(Diagnostic)]
#[error(
  "`mod` takes a path to the module (eg. path::to::module), not a string literal"
)]
#[code(PARSE_MOD_STRING_LIT)]
pub struct ModStringLit {
  #[error("invalid use here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected identifier found {found}")]
#[code(PARSE_MOD_EXPECTED_IDENTIFIER)]
pub struct ExpectedIdentifier {
  pub found: TokenType,
  #[error("expected identifier here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("found `const` alone in a type")]
#[code(PARSE_CONST_ALONE_IN_A_TYPE)]
#[help("if you meant to type a const pointer add a star like this: *const <>")]
pub struct ConstAloneInType {
  #[error("alone `const` found here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected a function or an identifier after const, found {found}")]
#[code(PARSE_CONST_EXPECTED_FUNC_OR_IDENT)]
pub struct ConstExpectedFuncOrIdent {
  pub found: TokenType,
  #[error("unexpected part found here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("const items need type annotations")]
#[code(PARSE_CONST_NEED_TYPE_ANNOTATIONS)]
pub struct ConstItemsNeedTypeAnnotation {
  #[error("valid type expected for this item")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("const items cannot be uninitialized")]
#[code(PARSE_CONST_CANNOT_BE_UNINITIALIZED)]
pub struct ConstCannotBeUninitialized {
  #[error("valid expression expected for this item")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected a type")]
#[code(PARSE_EXPECTED_TYPE)]
pub struct ExpectedType {
  #[error("type expected here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("`self` and `package` are only allowed in root of the path")]
#[code(PARSE_SELF_AND_PACKAGE_ROOT_ONLY)]
#[help("only super is allowed at any position")]
pub struct SelfAndPackageRootOnly {
  #[error("found here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected `super` or an identifier in the path, found {found}")]
#[code(PARSE_EXPECTED_SUPER_OR_IDENT_PATH)]
pub struct ExpectedSuperOrIdentPath {
  #[error("invalid part found here")]
  pub span: Span,
  pub found: TokenType,
}

#[derive(Diagnostic)]
#[warning("function names have to be camel case")]
#[code(PARSE_FUNCTION_CAMEL_CASE)]
// TODO: add suggestion to change the name
pub struct FunctionCamelCase {
  #[warning("in this name here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected an identifier for a generic parameter, found {found}")]
#[code(PARSE_EXPECTED_IDENTIFIER_IN_GENERIC)]
pub struct ExpectedIdentifierInGeneric {
  #[error("invalid part found here")]
  pub span: Span,
  pub found: TokenType,
}

#[derive(Diagnostic)]
#[error("expected a type path for a type bound, found {found}")]
#[code(PARSE_EXPECTED_TYPE_PATH_FOR_BOUND)]
#[help("type path can be simple like Display or std::math::Add<i32>")]
pub struct ExpectedTypePathForBound {
  #[error("invalid part found here")]
  pub span: Span,
  pub found: TokenType,
}

#[derive(Diagnostic)]
#[error("trailing `+` found in type bound")]
#[code(PARSE_TRAILING_PLUS_IN_TYPE_BOUND)]
pub struct TrailingPlusInTypeBound {
  #[error("trailing plus here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error(
  "expected an identifier or `...` to begin a function parameter, found {found}"
)]
#[code(PARSE_EXPECTED_IDENT_OR_DOTS)]
pub struct ExpectedIdentOrDots {
  #[error("invalid start here")]
  pub span: Span,
  pub found: TokenType,
}

#[derive(Diagnostic)]
#[error("variadic parameters do not accept default values")]
#[code(PARSE_VARIADIC_NO_DEFAULT)]
pub struct VariadicNoDefault {
  #[error("this is not allowed")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("all variables have to be initialized")]
#[code(PARSE_ALL_VARS_INIT)]
pub struct AllVarsInitialized {
  #[error("this one isn't")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected an expression")]
#[code(PARSE_EXPECTED_EXPR)]
pub struct ExpectedExpression {
  #[error("expected expression here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected an expression, found {found}")]
#[code(PARSE_EXPECTED_EXPR_FOUND)]
pub struct ExpectedExpressionFound {
  pub found: TokenType,
  #[error("expected expression here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("`break` can only be used inside a loop")]
#[code(PARSE_BREAK_OUTSIDE_LOOP)]
pub struct BreakOutsideLoop {
  #[error("invalid `break` here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("`continue` can only be used inside a loop")]
#[code(PARSE_CONTINUE_OUTSIDE_LOOP)]
pub struct ContinueOutsideLoop {
  #[error("invalid `continue` here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected `=>` after match arm pattern")]
#[code(PARSE_EXPECTED_FAT_ARROW)]
pub struct ExpectedFatArrow {
  pub found: TokenType,
  #[error("expected `=>` here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected a pattern")]
#[code(PARSE_EXPECTED_PATTERN)]
pub struct ExpectedPattern {
  pub found: TokenType,
  #[error("expected pattern here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected a builtin function name after `@`")]
#[code(PARSE_EXPECTED_BUILTIN_NAME)]
pub struct ExpectedBuiltinName {
  pub found: TokenType,
  #[error("expected identifier here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("invalid assignment target")]
#[code(PARSE_INVALID_ASSIGN_TARGET)]
#[help(
  "assignment targets must be a variable, field access, or index expression"
)]
pub struct InvalidAssignTarget {
  #[error("cannot assign to this expression")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected field name after `.`")]
#[code(PARSE_EXPECTED_FIELD_NAME)]
pub struct ExpectedFieldName {
  pub found: TokenType,
  #[error("expected identifier here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("missing `:` after `?` in ternary expression")]
#[code(PARSE_MISSING_COLON_IN_TERNARY)]
#[help("ternary expressions have the form: condition ? then_expr : else_expr")]
pub struct MissingColonInTernary {
  #[error("expected `:` here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("missing `in` keyword in for loop")]
#[code(PARSE_MISSING_IN_KEYWORD)]
#[help("for loops have the form: for <binding> in <iterator> {{ ... }}")]
pub struct MissingInKeyword {
  pub found: TokenType,
  #[error("expected `in` here, found `{found}`")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("unclosed delimiter `{open}`")]
#[code(PARSE_UNCLOSED_DELIMITER)]
pub struct UnclosedDelimiter {
  pub open: &'static str,
  pub close: &'static str,
  #[error("this `{open}` was never closed")]
  pub open_span: Span,
  #[error("expected `{close}` here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("empty match expression")]
#[code(PARSE_EMPTY_MATCH)]
#[help("match expressions require at least one arm")]
pub struct EmptyMatch {
  #[error("this match has no arms")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("duplicate named argument `{name}`")]
#[code(PARSE_DUPLICATE_NAMED_ARG)]
pub struct DuplicateNamedArg {
  pub name: String,
  #[error("first used here")]
  pub first_span: Span,
  #[error("duplicate here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected expression after `{op}` operator")]
#[code(PARSE_MISSING_OPERAND)]
#[help("binary operators require an expression on both sides")]
pub struct MissingOperand {
  pub op: String,
  #[error("expected expression here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected expression after struct field `.{field}`")]
#[code(PARSE_EXPECTED_FIELD_VALUE)]
#[help("use `.field = value` or `.field` for shorthand initialization")]
pub struct ExpectedFieldValue {
  pub field: String,
  #[error("expected `=` or `,` here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("module `{module}` couldn't be resolved")]
#[code(PARSE_MODULE_NOT_FOUND)]
pub struct ModuleNotFound {
  pub module: Path,

  #[error("in this `mod` item")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("generic arguments are not allowed in `super` segment")]
#[code(PARSE_GENERICS_IN_SUPER)]
pub struct GenericsInSuper {
  #[error("found here. these are useless")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("generic arguments can't appear after the first path segment")]
#[help(
  "generic arguments in this place don't make sense. you can only attach generic arguments to items"
)]
#[code(PARSE_GENERICS_FIRST_SEGMENT)]
pub struct GenericsFirstSegment {
  #[error("found here")]
  pub span: Span,
}

// TODO: add a proper code suggestion
#[derive(Diagnostic)]
#[error("there is no point of aliasing an import binding")]
#[help("just use the aliased name")]
#[code(PARSE_ALIASING_A_BINDING)]
pub struct AliasingABinding {
  #[error("found here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected wildcard, identifier or import list, found {found}")]
#[help("just use the aliased name")]
#[code(PARSE_UNEXPECTED_IMPORT_KIND)]
pub struct UnexpectedImportKind {
  #[error("found here")]
  pub span: Span,
  pub found: TokenType,
}

#[derive(Diagnostic)]
#[error("imports take a path not a string literal")]
#[code(PARSE_IMPORT_NOT_A_PATH)]
pub struct ImportNotAPath {
  #[error("found here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("expected '(' to open parameter list")]
#[code(PARSE_EXPECTED_PAREN_OPEN_LIST)]
pub struct ExpectedParenToOpenList {
  #[error("expected here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("array repeat syntax cannot be mixed with comma-separated elements")]
#[code(PARSE_REPEAT_SYNTAX_ONLY_AT_START)]
#[help("repeat syntax must be `[value; count]`, not mixed with other elements")]
pub struct RepeatSyntaxOnlyAtStart {
  #[error("semicolon found here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("invalid array repeat syntax")]
#[code(PARSE_INVALID_REPEAT_SYNTAX)]
#[help("array repeat syntax is `[value; count]` with no additional elements")]
pub struct InvalidRepeatSyntax {
  #[error("unexpected tokens after repeat count")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("repeat syntax requires exactly one value before the semicolon")]
#[code(PARSE_REPEAT_SYNTAX_REQUIRED_VALUE)]
pub struct RepeatSyntaxRequiredValue {
  #[error("semicolon without preceding value")]
  pub span: Span,
}
