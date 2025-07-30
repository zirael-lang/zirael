use crate::{
    AstWalker, Expr, LexedModule, ModuleId,
    ast::{
        ClassDeclaration, ClassField, EnumDeclaration, EnumVariant, Function, FunctionModifiers,
        FunctionSignature, GenericParameter, Parameter, ReturnType, Type,
    },
};
use id_arena::{Arena, Id};
use std::{collections::HashMap, ops::Range, sync::Arc};
use strsim::levenshtein;
use zirael_utils::prelude::*;

pub type SymbolId = Id<Symbol>;
pub type ScopeId = Id<Scope>;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Variable { ty: Type },
    Constant { ty: Type, value: Option<Expr> },
    Function { signature: FunctionSignature, modifiers: FunctionModifiers },
    Parameter { ty: Type, is_variadic: bool, default_value: Option<Expr> },
    Class { fields: Vec<ClassField>, generics: Vec<GenericParameter> },
    Enum { generics: Option<Vec<GenericParameter>>, variants: Vec<EnumVariant> },
    Temporary { ty: Type, lifetime: TemporaryLifetime },
}

impl SymbolKind {
    pub fn name(&self) -> &str {
        match self {
            SymbolKind::Variable { .. } => "variable",
            SymbolKind::Constant { .. } => "constant",
            SymbolKind::Function { .. } => "function",
            SymbolKind::Parameter { .. } => "parameter",
            SymbolKind::Class { .. } => "class",
            SymbolKind::Enum { .. } => "enum",
            SymbolKind::Temporary { .. } => "temporary",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemporaryLifetime {
    Expression,
    Statement,
    Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: Identifier,
    pub kind: SymbolKind,
    pub scope: ScopeId,
    pub source_location: Option<Span>,
    pub is_used: bool,
    pub declaration_order: usize,
    pub imported_from: Option<ScopeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub children: Vec<ScopeId>,
    pub symbols: HashMap<Identifier, SymbolId>,
    pub scope_type: ScopeType,
    pub depth: usize,
    pub imported_modules: Vec<ScopeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeType {
    Global,
    Module(SourceFileId),
    Function(Identifier),
    Block,
    Class(Identifier),
    Enum(Identifier),
    Loop,
    Conditional,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolTableError {
    SymbolAlreadyExists { name: Identifier, existing_id: SymbolId, scope: ScopeId },
    SymbolNotFound { name: Identifier, scope: ScopeId },
    InvalidScope(ScopeId),
    ScopeNotFound(ScopeId),
    CircularScopeReference,
    ImportConflict(Vec<ImportConflict>),
    InvalidImportTarget(ScopeId),
    CannotImportFromSelf,
    ModuleNotFound(SourceFileId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportConflict {
    pub name: Identifier,
    pub existing_id: SymbolId,
    pub new_id: SymbolId,
}

#[derive(Debug)]
struct SymbolTableImpl {
    symbols: Arena<Symbol>,
    scopes: Arena<Scope>,
    current_scope: ScopeId,
    global_scope: ScopeId,
    declaration_counter: usize,
    name_lookup: HashMap<(Identifier, ScopeId), SymbolId>,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable(Arc<RwLock<SymbolTableImpl>>);

impl Default for SymbolTableImpl {
    fn default() -> Self {
        let mut symbols = Arena::new();
        let mut scopes = Arena::new();

        let global_scope = Scope {
            parent: None,
            children: Vec::new(),
            symbols: HashMap::new(),
            scope_type: ScopeType::Global,
            depth: 0,
            imported_modules: Vec::new(),
        };

        let global_scope_id = scopes.alloc(global_scope);

        Self {
            symbols,
            scopes,
            current_scope: global_scope_id,
            global_scope: global_scope_id,
            declaration_counter: 0,
            name_lookup: HashMap::new(),
        }
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Default::default()
    }

    fn read<R>(&self, reader: impl FnOnce(&SymbolTableImpl) -> R) -> R {
        reader(&self.0.read())
    }

    fn write<R>(&self, writer: impl FnOnce(&mut SymbolTableImpl) -> R) -> R {
        writer(&mut self.0.write())
    }

    pub fn insert(
        &self,
        name: Identifier,
        kind: SymbolKind,
        span: Option<Span>,
    ) -> Result<SymbolId, SymbolTableError> {
        self.write(|table| {
            let current_scope = table.current_scope;

            if let Some(&existing_id) = table.name_lookup.get(&(name, current_scope)) {
                return Err(SymbolTableError::SymbolAlreadyExists {
                    name,
                    existing_id,
                    scope: current_scope,
                });
            }

            let symbol = Symbol {
                name,
                kind,
                scope: current_scope,
                source_location: span,
                is_used: false,
                declaration_order: table.declaration_counter,
                imported_from: None,
            };

            table.declaration_counter += 1;
            let symbol_id = table.symbols.alloc(symbol);

            table.name_lookup.insert((name, current_scope), symbol_id);

            if let Some(scope) = table.scopes.get_mut(current_scope) {
                scope.symbols.insert(name, symbol_id);
            }

            Ok(symbol_id)
        })
    }

    pub fn import_all_from_module(
        &self,
        source_file: SourceFileId,
        target_file: SourceFileId,
    ) -> Result<Vec<SymbolId>, SymbolTableError> {
        self.write(|table| {
            let source_module = table
                .find_module_by_source_id(source_file)
                .ok_or(SymbolTableError::ModuleNotFound(source_file))?;
            let target_module = table
                .find_module_by_source_id(target_file)
                .ok_or(SymbolTableError::ModuleNotFound(target_file))?;

            if source_file == target_file {
                return Err(SymbolTableError::CannotImportFromSelf);
            }

            let mut imported_symbols = Vec::new();
            let mut conflicts = Vec::new();

            let source_symbols = table.get_originally_declared_symbols(source_module);

            for (symbol_name, new_id) in source_symbols {
                if let Some(&existing_id) = table.name_lookup.get(&(symbol_name, target_module)) {
                    conflicts.push(ImportConflict { name: symbol_name, existing_id, new_id });
                    continue;
                }

                let imported_symbol_id = table.create_imported_symbol(
                    new_id,
                    symbol_name,
                    target_module,
                    source_module,
                )?;

                imported_symbols.push(imported_symbol_id);
            }

            table.record_module_import(target_module, source_module);

            if !conflicts.is_empty() {
                return Err(SymbolTableError::ImportConflict(conflicts));
            }

            Ok(imported_symbols)
        })
    }

    pub fn get_imported_modules(&self, scope: ScopeId) -> Vec<ScopeId> {
        self.read(|table| {
            table.scopes.get(scope).map(|s| s.imported_modules.clone()).unwrap_or_default()
        })
    }

    pub fn get_import_source(&self, symbol_id: SymbolId) -> Option<ScopeId> {
        self.read(|table| table.symbols.get(symbol_id).and_then(|symbol| symbol.imported_from))
    }

    pub fn get_imported_symbols(&self, scope: ScopeId) -> Vec<(SymbolId, ScopeId)> {
        self.read(|table| {
            table
                .scopes
                .get(scope)
                .map(|scope| {
                    scope
                        .symbols
                        .values()
                        .filter_map(|&symbol_id| {
                            table.symbols.get(symbol_id).and_then(|symbol| {
                                symbol.imported_from.map(|source| (symbol_id, source))
                            })
                        })
                        .collect()
                })
                .unwrap_or_default()
        })
    }

    pub fn find_module_by_source(&self, source_file: SourceFileId) -> Option<ScopeId> {
        self.read(|table| {
            table.scopes.iter()
                .find(|(_, scope)| matches!(scope.scope_type, ScopeType::Module(id) if id == source_file))
                .map(|(id, _)| id)
        })
    }

    pub fn lookup(&self, name: &Identifier) -> Option<SymbolId> {
        self.read(|table| {
            let mut current_scope = Some(table.current_scope);

            while let Some(scope_id) = current_scope {
                if let Some(&symbol_id) = table.name_lookup.get(&(*name, scope_id)) {
                    return Some(symbol_id);
                }

                current_scope = table.scopes.get(scope_id)?.parent;
            }

            None
        })
    }

    pub fn lookup_symbol(&self, name: &Identifier) -> Option<Symbol> {
        let symbol_id = self.lookup(name)?;
        self.get_symbol(symbol_id)
    }

    pub fn lookup_in_scope(&self, name: Identifier, scope: ScopeId) -> Option<SymbolId> {
        self.read(|table| table.name_lookup.get(&(name, scope)).copied())
    }

    pub fn get_symbol(&self, id: SymbolId) -> Option<Symbol> {
        self.read(|table| table.symbols.get(id).cloned())
    }

    pub fn get_symbol_unchecked(&self, id: &SymbolId) -> Symbol {
        self.read(|table| table.symbols.get(*id).cloned().unwrap())
    }

    pub fn mark_used(&self, id: SymbolId) -> Result<(), SymbolTableError> {
        self.write(|table| {
            if let Some(symbol) = table.symbols.get_mut(id) {
                symbol.is_used = true;
                Ok(())
            } else {
                Err(SymbolTableError::SymbolNotFound {
                    name: default_ident(),
                    scope: table.current_scope,
                })
            }
        })
    }

    pub fn push_scope(&self, scope_type: ScopeType) -> ScopeId {
        self.write(|table| {
            let current_scope = table.current_scope;
            if let Some(existing_child) = table.scopes.get(current_scope).and_then(|scope| {
                scope.children.iter().find(|&&child_id| {
                    table
                        .scopes
                        .get(child_id)
                        .map(|child| child.scope_type == scope_type)
                        .unwrap_or(false)
                })
            }) {
                table.current_scope = *existing_child;
                return *existing_child;
            }

            let parent_id = table.current_scope;
            let parent_depth = table.scopes.get(parent_id).map(|s| s.depth).unwrap_or(0);

            let new_scope = Scope {
                parent: Some(parent_id),
                children: Vec::new(),
                symbols: HashMap::new(),
                scope_type,
                depth: parent_depth + 1,
                imported_modules: Vec::new(),
            };

            let scope_id = table.scopes.alloc(new_scope);

            if let Some(parent) = table.scopes.get_mut(parent_id) {
                parent.children.push(scope_id);
            }

            table.current_scope = scope_id;
            scope_id
        })
    }

    pub fn pop_scope(&self) -> Result<ScopeId, SymbolTableError> {
        self.write(|table| {
            let current_scope = table.current_scope;

            if let Some(scope) = table.scopes.get(current_scope) {
                if let Some(parent_id) = scope.parent {
                    table.current_scope = parent_id;
                    Ok(current_scope)
                } else {
                    Err(SymbolTableError::InvalidScope(current_scope))
                }
            } else {
                Err(SymbolTableError::ScopeNotFound(current_scope))
            }
        })
    }

    pub fn current_scope(&self) -> ScopeId {
        self.read(|table| table.current_scope)
    }

    pub fn global_scope(&self) -> ScopeId {
        self.read(|table| table.global_scope)
    }

    pub fn get_scope(&self, id: ScopeId) -> Option<Scope> {
        self.read(|table| table.scopes.get(id).cloned())
    }

    pub fn get_scope_unchecked(&self, id: ScopeId) -> Scope {
        self.read(|table| table.scopes.get(id).cloned().unwrap())
    }

    pub fn get_symbols_in_scope(&self, scope_id: ScopeId) -> Vec<(Identifier, SymbolId)> {
        self.read(|table| {
            table
                .scopes
                .get(scope_id)
                .map(|scope| scope.symbols.iter().map(|(&name, &id)| (name, id)).collect())
                .unwrap_or_default()
        })
    }

    pub fn get_unused_symbols(&self) -> Vec<(SymbolId, Symbol)> {
        self.read(|table| {
            table
                .symbols
                .iter()
                .filter(|(_, symbol)| !symbol.is_used)
                .map(|(id, symbol)| (id, symbol.clone()))
                .collect()
        })
    }

    pub fn update_symbol_kind(
        &self,
        id: SymbolId,
        new_kind: SymbolKind,
    ) -> Result<(), SymbolTableError> {
        self.write(|table| {
            if let Some(symbol) = table.symbols.get_mut(id) {
                symbol.kind = new_kind;
                Ok(())
            } else {
                Err(SymbolTableError::SymbolNotFound {
                    name: default_ident(),
                    scope: table.current_scope,
                })
            }
        })
    }

    pub fn is_ancestor_scope(&self, ancestor: ScopeId, descendant: ScopeId) -> bool {
        self.read(|table| {
            let mut current = Some(descendant);

            while let Some(scope_id) = current {
                if scope_id == ancestor {
                    return true;
                }
                current = table.scopes.get(scope_id).expect("missing").parent;
            }

            false
        })
    }

    pub fn get_accessible_symbols(&self) -> Vec<(Identifier, SymbolId, ScopeId)> {
        self.read(|table| {
            let mut symbols = Vec::new();
            let mut current_scope = Some(table.current_scope);

            while let Some(scope_id) = current_scope {
                if let Some(scope) = table.scopes.get(scope_id) {
                    for (&name, &symbol_id) in &scope.symbols {
                        symbols.push((name, symbol_id, scope_id));
                    }
                    current_scope = scope.parent;
                } else {
                    break;
                }
            }

            symbols
        })
    }

    pub fn get_c_identifier(&self, id: SymbolId) -> Option<String> {
        self.read(|table| {
            table.symbols.get(id).map(|symbol| match &symbol.kind {
                SymbolKind::Temporary { .. } => {
                    format!("__zirael_temp_{}", id.index())
                }
                _ => {
                    let base_name = resolve(&symbol.name);
                    if table.needs_mangling(id) {
                        format!("__zirael_{}_{}", base_name, symbol.scope.index())
                    } else {
                        base_name
                    }
                }
            })
        })
    }

    pub fn get_temporaries_by_lifetime(
        &self,
        lifetime: TemporaryLifetime,
    ) -> Vec<(SymbolId, Symbol)> {
        self.read(|table| {
            table
                .symbols
                .iter()
                .filter(|(_, symbol)| {
                    matches!(&symbol.kind, SymbolKind::Temporary { lifetime: temp_lifetime, .. }
                            if temp_lifetime == &lifetime)
                })
                .map(|(id, symbol)| (id, symbol.clone()))
                .collect()
        })
    }

    pub fn clear(&self) {
        self.write(|table| {
            *table = SymbolTableImpl::default();
        })
    }

    pub fn insert_temporary(
        &self,
        ty: Type,
        lifetime: TemporaryLifetime,
        span: Option<Span>,
    ) -> Result<SymbolId, SymbolTableError> {
        let temp_name = self.write(|table| {
            let temp_count = table
                .symbols
                .iter()
                .filter(|(_, s)| matches!(s.kind, SymbolKind::Temporary { .. }))
                .count();
            get_or_intern(&format!("__temp_{}", temp_count))
        });

        let kind = SymbolKind::Temporary { ty, lifetime };
        self.insert(temp_name, kind, span)
    }

    pub fn find_similar_symbol(
        &self,
        name: &Identifier,
        predicate: impl Fn(&Symbol) -> bool,
    ) -> Option<SymbolId> {
        self.read(|table| {
            let mut current_scope = Some(table.current_scope);

            while let Some(scope_id) = current_scope {
                let symbols_in_scope = self.get_symbols_in_scope(scope_id);

                if let Some(symbol_id) =
                    symbols_in_scope.iter().find_map(|(symbol_name, symbol_id)| {
                        let sym = self.get_symbol_unchecked(symbol_id);

                        let is_similar = levenshtein(&resolve(name), &resolve(symbol_name)) <= 2
                            && predicate(&sym);

                        if is_similar { Some(*symbol_id) } else { None }
                    })
                {
                    return Some(symbol_id);
                }

                current_scope = table.scopes.get(scope_id)?.parent;
            }

            None
        })
    }
}

impl SymbolTableImpl {
    fn find_module_by_source_id(&self, source_file: SourceFileId) -> Option<ScopeId> {
        self.scopes
            .iter()
            .find(
                |(_, scope)| matches!(scope.scope_type, ScopeType::Module(id) if id == source_file),
            )
            .map(|(id, _)| id)
    }

    fn get_originally_declared_symbols(&self, module_id: ScopeId) -> Vec<(Identifier, SymbolId)> {
        let Some(module_scope) = self.scopes.get(module_id) else {
            return Vec::new();
        };

        module_scope
            .symbols
            .iter()
            .filter_map(|(&name, &symbol_id)| {
                let symbol = self.symbols.get(symbol_id)?;
                if symbol.imported_from.is_none() { Some((name, symbol_id)) } else { None }
            })
            .collect()
    }

    fn create_imported_symbol(
        &mut self,
        source_symbol_id: SymbolId,
        symbol_name: Identifier,
        target_module: ScopeId,
        source_module: ScopeId,
    ) -> Result<SymbolId, SymbolTableError> {
        let source_symbol = self
            .symbols
            .get(source_symbol_id)
            .ok_or(SymbolTableError::SymbolNotFound { name: symbol_name, scope: source_module })?
            .clone();

        let imported_symbol = Symbol {
            name: symbol_name,
            kind: source_symbol.kind,
            scope: target_module,
            source_location: source_symbol.source_location,
            is_used: false,
            declaration_order: self.declaration_counter,
            imported_from: Some(source_module),
        };

        self.declaration_counter += 1;
        let imported_symbol_id = self.symbols.alloc(imported_symbol);

        self.name_lookup.insert((symbol_name, target_module), imported_symbol_id);

        if let Some(target_scope) = self.scopes.get_mut(target_module) {
            target_scope.symbols.insert(symbol_name, imported_symbol_id);
        }

        Ok(imported_symbol_id)
    }

    fn record_module_import(&mut self, target_module: ScopeId, source_module: ScopeId) {
        if let Some(target_scope) = self.scopes.get_mut(target_module) {
            if !target_scope.imported_modules.contains(&source_module) {
                target_scope.imported_modules.push(source_module);
            }
        }
    }

    fn needs_mangling(&self, id: SymbolId) -> bool {
        if let Some(symbol) = self.symbols.get(id) {
            let name = symbol.name;
            self.symbols.iter().any(|(other_id, other_symbol)| {
                other_id != id
                    && other_symbol.name == name
                    && self.scopes_would_conflict_in_c(symbol.scope, other_symbol.scope)
            })
        } else {
            false
        }
    }

    fn scopes_would_conflict_in_c(&self, scope1: ScopeId, scope2: ScopeId) -> bool {
        if let (Some(s1), Some(s2)) = (self.scopes.get(scope1), self.scopes.get(scope2)) {
            match (&s1.scope_type, &s2.scope_type) {
                (ScopeType::Global, _) | (_, ScopeType::Global) => true,
                (ScopeType::Function(f1), ScopeType::Function(f2)) => f1 == f2,
                _ => false,
            }
        } else {
            false
        }
    }
}

pub trait WalkerWithAst<'reports>: AstWalker {
    fn symbol_table(&self) -> &SymbolTable;
    fn reports(&self) -> &Reports<'reports>;
    fn processed_file(&self) -> Option<SourceFileId>;
    fn set_processed_file(&mut self, file_id: SourceFileId);

    fn pop_scope(&mut self) {
        if let Err(err) = self.symbol_table().pop_scope()
            && let Some(file_id) = self.processed_file()
        {
            self.reports().add(
                file_id,
                ReportBuilder::builder(
                    &format!("failed to pop a scope: {:?}", err),
                    ReportKind::Error,
                ),
            )
        }
    }

    fn walk(&mut self, modules: &mut Vec<LexedModule>) {
        for module in modules {
            let ModuleId::File(file_id) = module.id else {
                continue;
            };

            self.symbol_table().push_scope(ScopeType::Module(file_id));
            self.set_processed_file(file_id);
            self.walk_ast(&mut module.ast);
            self.pop_scope();
        }
    }

    fn error(&mut self, message: &str, labels: Vec<(String, Range<usize>)>, notes: Vec<String>) {
        if let Some(file_id) = self.processed_file() {
            let mut report = ReportBuilder::builder(message, ReportKind::Error);
            for note in notes {
                report = report.note(&note);
            }
            for (msg, span) in labels {
                report = report.label(&msg, span);
            }
            self.reports().add(file_id, report);
        } else {
            warn!("report outside of a file: {}", message);
        }
    }
}

#[macro_export]
macro_rules! impl_walker_with_ast {
    ($struct_name:ident) => {
        impl<'reports> WalkerWithAst<'reports> for $struct_name<'reports> {
            fn symbol_table(&self) -> &SymbolTable {
                &self.symbol_table
            }

            fn reports(&self) -> &Reports<'reports> {
                &self.reports
            }

            fn processed_file(&self) -> Option<SourceFileId> {
                self.processed_file
            }

            fn set_processed_file(&mut self, file_id: SourceFileId) {
                self.processed_file = Some(file_id);
            }
        }
    };
}
