use crate::prelude::warn;
use zirael_parser::{
    AstWalker, Expr, Function, LexedModule, ModuleId, ScopeType, SymbolKind, SymbolTable,
    WalkerWithAst, impl_walker_with_ast,
};
use zirael_utils::prelude::*;

pub struct NameResolution<'reports> {
    pub symbol_table: SymbolTable,
    pub reports: Reports<'reports>,
    pub processed_file: Option<SourceFileId>,
    pub sources: Sources,
}

impl<'reports> NameResolution<'reports> {
    pub fn new(table: &SymbolTable, reports: &Reports<'reports>, sources: &Sources) -> Self {
        Self {
            symbol_table: table.clone(),
            reports: reports.clone(),
            processed_file: None,
            sources: sources.clone(),
        }
    }
}
impl_walker_with_ast!(NameResolution);

impl AstWalker for NameResolution<'_> {
    fn walk_function(&mut self, func: &mut Function) {
        self.symbol_table.push_scope(ScopeType::Function(func.name));

        self.walk_identifier(&mut func.name);
        self.walk_function_modifiers(&mut func.modifiers);
        self.walk_function_signature(&mut func.signature);

        if let Some(body) = &mut func.body {
            self.walk_expr(body);
        }

        self.pop_scope();
    }

    fn visit_function_call(&mut self, callee: &mut Expr, _args: &mut [Expr]) {
        let span = callee.span.clone();

        let Some((ident, ident_sym_id)) = callee.as_identifier() else {
            self.error("expected identifier in function call", vec![], vec![]);
            return;
        };

        if let Some(id) = self.symbol_table.lookup(ident) {
            let symbol = self.symbol_table.get_symbol_unchecked(&id);
            match symbol.kind {
                SymbolKind::Function { .. } => {
                    *ident_sym_id = Some(id);
                }
                _ => {
                    let kind = article(symbol.kind.name()).dimmed().bold();
                    self.error(
                        &format!("expected a function, found {kind}"),
                        vec![(format!("attempted to call {kind}"), span)],
                        vec![],
                    );
                }
            }
        } else {
            let mut report = ReportBuilder::builder(
                &format!(
                    "couldn't find a suitable function named {}",
                    resolve(ident).dimmed().bold()
                ),
                ReportKind::Error,
            )
            .label("attempted to call", span);

            if let Some(sym_id) = self
                .symbol_table
                .find_similar_symbol(ident, |sym| matches!(sym.kind, SymbolKind::Function { .. }))
            {
                let symbol = self.symbol_table.get_symbol_unchecked(&sym_id);
                let scope = if let Some(imported_scope) = symbol.imported_from {
                    self.symbol_table.get_scope_unchecked(imported_scope)
                } else {
                    self.symbol_table.get_scope_unchecked(symbol.scope)
                };
                if let ScopeType::Module(file_id) = scope.scope_type {
                    let path = self.sources.get_unchecked(file_id).path();
                    let path_string = path.display().to_string();
                    let span = symbol.source_location.unwrap_or_default();
                    report = report.label_custom(
                        &format!(
                            "did you mean to call {} from {}?",
                            resolve(&symbol.name).dimmed().bold(),
                            path_string.dimmed().bold()
                        ),
                        span,
                        &path,
                        Color::BrightGreen,
                    );
                }
            }
            if let Some(file_id) = self.processed_file {
                self.reports.add(file_id, report);
            } else {
                warn!("report outside of a file: {:#?}", report);
            }
        }
    }
}
