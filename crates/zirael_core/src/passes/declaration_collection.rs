use crate::prelude::{ReportKind, WalkerWithAst, debug};
use std::{any::Any, env::var, fmt::format, path::PathBuf};
use zirael_parser::{
    Ast, AstWalker, Dependency, DependencyGraph, Function, ImportConflict, ImportKind, ItemKind,
    LexedModule, ModuleId, Parameter, ParameterKind, ScopeType, Symbol, SymbolKind, SymbolTable,
    SymbolTableError, VarDecl, impl_walker_with_ast,
};
use zirael_utils::{
    prelude::{Colorize, Identifier, ReportBuilder, Reports, Sources, Span, resolve},
    sources::SourceFileId,
};

pub struct DeclarationCollection<'reports> {
    pub symbol_table: SymbolTable,
    pub reports: Reports<'reports>,
    pub processed_file: Option<SourceFileId>,
    pub sources: Sources,
}

impl<'reports> DeclarationCollection<'reports> {
    pub fn new(table: &SymbolTable, reports: &Reports<'reports>, sources: &Sources) -> Self {
        Self {
            symbol_table: table.clone(),
            reports: reports.clone(),
            processed_file: None,
            sources: sources.clone(),
        }
    }

    pub fn collect(&mut self, modules: &mut Vec<LexedModule>) {
        self.walk(modules);
        for module in modules {
            let ModuleId::File(file_id) = module.id else {
                continue;
            };

            self.symbol_table.push_scope(ScopeType::Module(file_id));
            self.processed_file = Some(file_id);

            let import_items = self.extract_import_items(&module.ast);
            for (import_kind, span) in import_items {
                self.handle_import(import_kind, span, file_id);
            }

            self.pop_scope();
        }
    }

    fn extract_import_items<'imports>(
        &self,
        ast: &'imports Ast,
    ) -> Vec<(&'imports ImportKind, &'imports Span)> {
        ast.items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Import(import_kind, span) => Some((import_kind, span)),
                _ => None,
            })
            .collect()
    }

    fn handle_import(
        &mut self,
        import_kind: &ImportKind,
        span: &Span,
        current_file_id: SourceFileId,
    ) {
        match import_kind {
            ImportKind::Path(path) => {
                self.process_path_import(path, span, current_file_id);
            }
            ImportKind::ExternalModule(_) => {
                todo!("importing from external modules is not supported yet")
            }
        }
    }

    fn process_path_import(&mut self, path: &PathBuf, span: &Span, current_file_id: SourceFileId) {
        let source_module = self
            .sources
            .get_by_path(path)
            .expect("this should be checked before collection of declarations");

        let import_result =
            self.symbol_table.import_all_from_module(source_module, current_file_id);

        match import_result {
            Ok(symbols) => {
                debug!("Successfully imported symbols from module {:?}: {:?}", path, symbols);
            }
            Err(error) => {
                self.handle_import_error(error, path, span, current_file_id);
            }
        }
    }

    fn handle_import_error(
        &mut self,
        error: SymbolTableError,
        path: &PathBuf,
        span: &Span,
        file_id: SourceFileId,
    ) {
        let base_report = ReportBuilder::builder(
            &format!("failed to import symbols from module: {:?}", error),
            ReportKind::Error,
        )
        .label("while processing this import", span.clone());

        match error {
            SymbolTableError::ImportConflict(conflicts) => {
                self.handle_import_conflicts(conflicts, path, base_report, file_id);
            }
            _ => {
                self.reports.add(file_id, base_report);
            }
        }
    }

    fn handle_import_conflicts(
        &mut self,
        conflicts: Vec<ImportConflict>,
        path: &PathBuf,
        base_report: ReportBuilder<'reports>,
        file_id: SourceFileId,
    ) {
        for conflict in conflicts {
            let existing_symbol = self.symbol_table.get_symbol_unchecked(&conflict.existing_id);
            let new_symbol = self.symbol_table.get_symbol_unchecked(&conflict.new_id);

            let conflict_message = format!(
                "import conflict while importing module {}",
                path.display().to_string().dimmed().bold()
            );

            let conflict_details = format!(
                "imported {} conflicts with local {}",
                self.format_symbol_description(&new_symbol),
                self.format_symbol_description(&existing_symbol)
            );

            let conflict_report = base_report
                .clone()
                .message(&conflict_message)
                .label(&conflict_details, existing_symbol.source_location.unwrap_or(0..0));

            self.reports.add(file_id, conflict_report);
        }
    }

    fn format_symbol_description(&self, symbol: &Symbol) -> String {
        format!("{} {}", symbol.kind.name(), resolve(&symbol.name)).dimmed().bold().to_string()
    }

    pub fn register_symbol(&mut self, name: Identifier, kind: SymbolKind, span: Span) {
        let file_id =
            self.processed_file.expect("when registering a symbol, the current file must be known");
        let symbol_name = resolve(&name);
        let symbol_type = kind.name();

        match self.symbol_table.insert(name, kind.clone(), Some(span.clone())) {
            Ok(_) => {}
            Err(SymbolTableError::SymbolAlreadyExists { existing_id, .. }) => {
                println!("{:?}", self.symbol_table.get_symbol(existing_id).unwrap());
                if let Some(existing_symbol) = self.symbol_table.get_symbol(existing_id) {
                    self.reports.add(
                        file_id,
                        ReportBuilder::builder(
                            &format!("{} already declared", symbol_name.dimmed().bold()),
                            ReportKind::Error,
                        )
                        .label(
                            &format!("redeclared here as a {}", symbol_type.dimmed().bold()),
                            span,
                        )
                        .label(
                            &format!(
                                "first declared here as a {}",
                                existing_symbol.kind.name().dimmed().bold()
                            ),
                            existing_symbol.source_location.unwrap_or(0..0),
                        ),
                    );
                }
            }
            Err(other_error) => {
                self.reports.add(
                    file_id,
                    ReportBuilder::builder(
                        &format!(
                            "failed to register {}: {:?}",
                            symbol_name.dimmed().bold(),
                            other_error
                        ),
                        ReportKind::Error,
                    )
                    .label("failed to register", span),
                );
            }
        }
    }
}
impl_walker_with_ast!(DeclarationCollection);

impl AstWalker for DeclarationCollection<'_> {
    fn walk_function(&mut self, func: &mut Function) {
        self.register_symbol(
            func.name,
            SymbolKind::Function {
                signature: func.signature.clone(),
                modifiers: func.modifiers.clone(),
            },
            func.span.clone(),
        );

        self.symbol_table.push_scope(ScopeType::Function(func.name));

        self.walk_identifier(&mut func.name);
        self.walk_function_modifiers(&mut func.modifiers);
        self.walk_function_signature(&mut func.signature);

        if let Some(body) = &mut func.body {
            self.walk_expr(body);
        }

        self.pop_scope();
    }

    fn visit_parameter(&mut self, param: &mut Parameter) {
        let param = param.clone();
        self.register_symbol(
            param.name,
            SymbolKind::Parameter {
                ty: param.ty,
                is_variadic: param.kind == ParameterKind::Variadic,
                default_value: param.default_value,
            },
            param.span,
        );
    }

    fn visit_var_decl(&mut self, var_decl: &mut VarDecl) {
        let var = var_decl.clone();
        self.register_symbol(var.name, SymbolKind::Variable { ty: var.ty }, var.span)
    }
}
