use crate::prelude::ReportKind;
use std::{any::Any, env::var, fmt::format};
use zirael_parser::{
    AstWalker, Dependency, DependencyGraph, Function, LexedModule, ModuleId, Parameter,
    ParameterKind, ScopeType, SymbolKind, SymbolTable, SymbolTableError, VarDecl,
};
use zirael_utils::{
    prelude::{Colorize, Identifier, ReportBuilder, Reports, Span, resolve},
    sources::SourceFileId,
};

pub struct DeclarationCollection<'reports> {
    pub symbol_table: SymbolTable,
    pub reports: Reports<'reports>,
    pub processed_file: Option<SourceFileId>,
}

impl<'reports> DeclarationCollection<'reports> {
    pub fn new(table: &SymbolTable, reports: &Reports<'reports>) -> Self {
        Self { symbol_table: table.clone(), reports: reports.clone(), processed_file: None }
    }

    pub fn collect(&mut self, modules: Vec<LexedModule>) {
        for module in modules {
            let ModuleId::File(file_id) = module.id else {
                continue;
            };
            self.symbol_table.push_scope(ScopeType::Module(file_id));
            self.processed_file = Some(file_id);
            self.walk_ast(&module.ast);
            self.pop_scope();
        }
    }

    pub fn pop_scope(&mut self) {
        if let Err(err) = self.symbol_table.pop_scope()
            && let Some(file_id) = self.processed_file
        {
            self.reports.add(
                file_id,
                ReportBuilder::builder(
                    &format!("failed to pop a scope: {:?}", err),
                    ReportKind::Error,
                ),
            )
        }
    }

    pub fn register_symbol(&mut self, name: Identifier, kind: SymbolKind, span: Span) {
        let file_id =
            self.processed_file.expect("when registering a symbol, the current file must be known");
        let symbol_name = resolve(&name);
        let symbol_type = kind.name();

        match self.symbol_table.insert(name, kind.clone(), Some(span.clone())) {
            Ok(_) => {}
            Err(SymbolTableError::SymbolAlreadyExists { existing_id, .. }) => {
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

impl AstWalker for DeclarationCollection<'_> {
    fn walk_function(&mut self, func: &Function) {
        self.register_symbol(
            func.name,
            SymbolKind::Function {
                signature: func.signature.clone(),
                modifiers: func.modifiers.clone(),
            },
            func.span.clone(),
        );

        self.symbol_table.push_scope(ScopeType::Function(func.name));

        self.walk_identifier(&func.name);
        self.walk_function_modifiers(&func.modifiers);
        self.walk_function_signature(&func.signature);

        if let Some(body) = &func.body {
            self.walk_expr(body);
        }

        self.pop_scope();
    }

    fn visit_parameter(&mut self, param: &Parameter) {
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

    fn visit_var_decl(&mut self, var_decl: &VarDecl) {
        let var = var_decl.clone();
        self.register_symbol(var.name, SymbolKind::Variable { ty: var.ty }, var.span)
    }
}
