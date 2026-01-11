// TODO:
// - `for` loops → `loop` + iterator protocol
// - `while` loops → `loop` + `if` + `break`
// - `?` operator → `match` with early return
// - Method calls → function calls with self
// - Operator overloading → trait method calls

mod context;
mod expr;
mod item;
mod pat;
mod ty;

pub use context::LoweringContext;

use crate::hir::Hir;
use zirael_diagnostics::DiagnosticCtx;
use zirael_parser::module::Modules;
use zirael_resolver::Resolver;

pub fn lower_to_hir(
  resolver: &Resolver,
  modules: &Modules,
  _dcx: &DiagnosticCtx,
) -> Hir {
  let hir = Hir::new();

  for module in modules.all() {
    let mut ctx = LoweringContext::new(resolver, &hir, module.source_file_id);
    ctx.lower_module(&module.node);
  }

  hir
}
