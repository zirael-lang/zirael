mod def;
pub mod errors;
mod import_order;
mod module_resolver;
mod resolver;
mod ribs;
mod scope;
mod symbol;
mod visitor;

pub use def::{DefId, DefKind, Definition};
pub use import_order::{ImportGraph, ResolutionOrder};
pub use module_resolver::ModuleResolver;
pub use resolver::Resolver;
pub use ribs::{Rib, RibKind};
pub use scope::{Scope, ScopeId, ScopeKind, Scopes};
pub use symbol::{Symbol, SymbolKind, SymbolTable};
pub use visitor::ResolveVisitor;

pub mod prelude {
  pub use crate::def::{DefId, DefKind, Definition};
  pub use crate::import_order::{ImportGraph, ResolutionOrder};
  pub use crate::module_resolver::ModuleResolver;
  pub use crate::resolver::Resolver;
  pub use crate::ribs::{Rib, RibKind};
  pub use crate::scope::{Scope, ScopeId, ScopeKind, Scopes};
  pub use crate::symbol::{Symbol, SymbolKind, SymbolTable};
  pub use crate::visitor::ResolveVisitor;
}
