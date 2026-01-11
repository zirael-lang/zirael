use zirael_diagnostic_macro::Diagnostic;
use zirael_source::span::Span;

#[derive(Diagnostic)]
#[error("cannot find `{name}` in this scope")]
#[code(RESOLVE_UNDEFINED_NAME)]
pub struct UndefinedName {
  pub name: String,
  #[error("not found in this scope")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("cannot find type `{name}` in this scope")]
#[code(RESOLVE_UNDEFINED_TYPE)]
pub struct UndefinedType {
  pub name: String,
  #[error("not found in this scope")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("the name `{name}` is defined multiple times")]
#[code(RESOLVE_DUPLICATE_DEFINITION)]
#[help("consider renaming one of the definitions")]
pub struct DuplicateDefinition {
  pub name: String,
  #[error("redefined here")]
  pub span: Span,
  #[error("previous definition here")]
  pub previous: Span,
}

#[derive(Diagnostic)]
#[error("cannot find module `{name}`")]
#[code(RESOLVE_UNDEFINED_MODULE)]
pub struct UndefinedModule {
  pub name: String,
  #[error("not found")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("cyclic import detected")]
#[code(RESOLVE_CYCLIC_IMPORT)]
#[help("break the cycle by restructuring your imports")]
pub struct CyclicImport {
  #[error("cycle involves this module")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("`{name}` is private")]
#[code(RESOLVE_PRIVATE_ITEM)]
pub struct PrivateItem {
  pub name: String,
  #[error("private item accessed here")]
  pub span: Span,
}

#[derive(Diagnostic)]
#[error("unresolved import `{path}`")]
#[code(RESOLVE_UNRESOLVED_IMPORT)]
pub struct UnresolvedImport {
  pub path: String,
  #[error("could not resolve")]
  pub span: Span,
}
