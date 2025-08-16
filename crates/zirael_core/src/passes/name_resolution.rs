use crate::prelude::{AstId, WalkerContext, warn};
use zirael_parser::{
    AstWalker, Expr, Function, LexedModule, ModuleId, ScopeType, Symbol, SymbolId, SymbolKind,
    SymbolTable, impl_ast_walker, item::Item,
};
use zirael_utils::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedSymbol {
    Function,
    Variable,
    Constant,
    Struct,
    Enum,
    Parameter,
    Type,
    Value,
    Any,
}

impl ExpectedSymbol {
    fn matches(&self, kind: &SymbolKind) -> bool {
        match (self, kind) {
            (ExpectedSymbol::Function, SymbolKind::Function { .. }) => true,
            (ExpectedSymbol::Variable, SymbolKind::Variable { .. }) => true,
            (ExpectedSymbol::Constant, SymbolKind::Constant { .. }) => true,
            (ExpectedSymbol::Struct, SymbolKind::Struct { .. }) => true,
            (ExpectedSymbol::Enum, SymbolKind::Enum { .. }) => true,
            (ExpectedSymbol::Parameter, SymbolKind::Parameter { .. }) => true,
            (ExpectedSymbol::Type, SymbolKind::Struct { .. } | SymbolKind::Enum { .. }) => true,
            (
                ExpectedSymbol::Value,
                SymbolKind::Variable { .. }
                | SymbolKind::Constant { .. }
                | SymbolKind::Parameter { .. },
            ) => true,
            (ExpectedSymbol::Any, _) => true,
            _ => false,
        }
    }

    fn name(&self) -> &'static str {
        match self {
            ExpectedSymbol::Function => "function",
            ExpectedSymbol::Variable => "variable",
            ExpectedSymbol::Constant => "constant",
            ExpectedSymbol::Struct => "struct",
            ExpectedSymbol::Enum => "enum",
            ExpectedSymbol::Parameter => "parameter",
            ExpectedSymbol::Type => "type",
            ExpectedSymbol::Value => "value",
            ExpectedSymbol::Any => "symbol",
        }
    }
}

impl_ast_walker!(NameResolution, {
    current_item: Option<SymbolId>
});

impl<'reports> NameResolution<'reports> {
    fn resolve_identifier(
        &mut self,
        ident: &Identifier,
        span: Span,
        expected: ExpectedSymbol,
    ) -> Option<SymbolId> {
        if let Some(id) = self.symbol_table.lookup(ident) {
            let symbol = self.symbol_table.get_symbol_unchecked(&id);

            if !expected.matches(&symbol.kind) {
                let found_kind = article(symbol.kind.name()).dimmed().bold();
                let expected_kind = article(expected.name()).dimmed().bold();
                self.error(
                    &format!("expected {expected_kind}, found {found_kind}"),
                    vec![(format!("found {found_kind} here"), span)],
                    vec![],
                );
                return None;
            }
            Some(id)
        } else {
            self.report_unknown_symbol(ident, span, expected);
            None
        }
    }

    fn report_unknown_symbol(&mut self, ident: &Identifier, span: Span, expected: ExpectedSymbol) {
        let mut report = ReportBuilder::builder(
            &format!("couldn't find {} named {}", expected.name(), resolve(ident).dimmed().bold()),
            ReportKind::Error,
        )
        .label("not found", span);

        if let Some(sym_id) =
            self.symbol_table.find_similar_symbol(ident, |sym: &Symbol| expected.matches(&sym.kind))
        {
            let symbol = self.symbol_table.get_symbol_unchecked(&sym_id);
            report = self.add_suggestion_to_report(report, &symbol);
        }

        if let Some(file_id) = self.processed_file {
            self.reports.add(file_id, report);
        } else {
            warn!("report outside of a file: {:#?}", report);
        }
    }

    fn add_suggestion_to_report(
        &self,
        mut report: ReportBuilder<'reports>,
        symbol: &Symbol,
    ) -> ReportBuilder<'reports> {
        let scope = if let Some(imported_scope) = symbol.imported_from {
            self.symbol_table.get_scope_unchecked(imported_scope)
        } else {
            self.symbol_table.get_scope_unchecked(symbol.scope)
        };

        if let ScopeType::Module(file_id) = scope.scope_type {
            let path = self.sources.get_unchecked(file_id).path();
            let path_string = path.display().to_string();
            let span = symbol.source_location.clone().unwrap_or_default();
            report = report.label_custom(
                &format!(
                    "did you mean {} from {}?",
                    resolve(&symbol.name).dimmed().bold(),
                    path_string.dimmed().bold()
                ),
                span,
                &path,
                Color::BrightCyan,
            );
        }
        report
    }
}

impl<'reports> AstWalker<'reports> for NameResolution<'reports> {
    fn visit_item(&mut self, _item: &mut Item) {
        self.current_item = _item.symbol_id;
    }

    fn visit_function_call(&mut self, callee: &mut Expr, _args: &mut [Expr]) {
        let span = callee.span.clone();
        let Some((ident, ident_sym_id)) = callee.as_identifier() else {
            self.error("expected identifier in function call", vec![], vec![]);
            return;
        };

        if let Some(id) = self.resolve_identifier(ident, span, ExpectedSymbol::Function) {
            *ident_sym_id = Some(id);

            self.symbol_table.new_relation(self.current_item.unwrap(), id);
        }
    }

    fn visit_identifier(&mut self, id: &mut Identifier, sym_id: &mut Option<SymbolId>, span: Span) {
        if let Some(id) = self.resolve_identifier(id, span, ExpectedSymbol::Value) {
            self.symbol_table.mark_used(id).expect("invalid symbol id");
            *sym_id = Some(id);
        }
    }

    fn visit_struct_init(&mut self, name: &mut Expr, _fields: &mut HashMap<Identifier, Expr>) {
        let span = name.span.clone();
        let Some((ident, ident_sym_id)) = name.as_identifier() else {
            self.error("expected identifier in struct init", vec![], vec![]);
            return;
        };

        if let Some(id) = self.resolve_identifier(ident, span, ExpectedSymbol::Struct) {
            *ident_sym_id = Some(id);

            self.symbol_table.new_relation(self.current_item.unwrap(), id);
        }
    }
}
