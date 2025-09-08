use crate::prelude::{ReportKind, WalkerContext, debug};
use std::path::{Path, PathBuf};
use zirael_parser::{
  Ast, AstId, AstWalker, Dependencies, ElseBranch, If, ImportConflict, ImportKind, ItemKind,
  LexedModule, MatchArm, ModuleId, Parameter, ParameterKind, Pattern, ScopeType, Stmt, Symbol,
  SymbolId, SymbolKind, SymbolTable, SymbolTableError, Type, VarDecl, impl_ast_walker, item::Item,
};
use zirael_utils::{
  prelude::{
    Colorize as _, Identifier, ReportBuilder, Reports, Sources, Span, default_ident, resolve,
  },
  sources::SourceFileId,
};

impl_ast_walker!(DeclarationCollection, {
    packages: Dependencies,
    used_externals: Vec<String>
});

impl<'reports> DeclarationCollection<'reports> {
  pub fn collect(&mut self, modules: &mut Vec<LexedModule>) {
    self.walk_modules(modules);
    for module in modules {
      let ModuleId::File(file_id) = module.id else {
        continue;
      };

      self.push_scope(ScopeType::Module(file_id));
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
    ast
      .items
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
      ImportKind::ExternalModule(module) => {
        let mut parts = module.clone();
        let name = resolve(&parts[0]);
        let pkg = self.packages.get(&name);

        if let Some(pkg) = pkg {
          if module.len() == 1 {
            self.process_path_import(pkg.entrypoint(), span, current_file_id);
          } else {
            parts.remove(0);
            let mut path = pkg.entrypoint().parent().unwrap().to_path_buf();

            for part in parts {
              let part = resolve(&part);
              path = path.join(part);
            }

            self.process_path_import(&path.with_extension("zr"), span, current_file_id);
          }

          self.used_externals.push(name.clone());
        } else {
          self.error(
            &format!("couldn't find package: {}", name.dimmed().bold()),
            vec![("in this import statement".to_owned(), span.clone())],
            vec![],
          );
        }
      }
    }
  }

  fn process_path_import(&mut self, path: &PathBuf, span: &Span, current_file_id: SourceFileId) {
    let source_module = self.sources.get_by_path(path);

    let Some(source_module) = source_module else {
      self.error(
        &format!("couldn't find module: {}", path.display().to_string().dimmed().bold()),
        vec![("in this import statement".to_owned(), span.clone())],
        vec!["if the file exists, make sure it was discovered by the compiler".to_owned()],
      );
      return;
    };

    let import_result = self.symbol_table.import_all_from_module(source_module, current_file_id);

    match import_result {
      Ok(symbols) => {
        debug!("Successfully imported symbols from module {path:?}: {symbols:?}");
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
      format!("failed to import symbols from module: {error:?}"),
      ReportKind::Error,
    )
    .label("while processing this import", span.clone());

    match error {
      SymbolTableError::ImportConflict(conflicts) => {
        self.handle_import_conflicts(conflicts, path, &base_report, file_id);
      }
      _ => {
        self.reports.add(file_id, base_report);
      }
    }
  }

  fn handle_import_conflicts(
    &self,
    conflicts: Vec<ImportConflict>,
    path: &Path,
    base_report: &ReportBuilder<'reports>,
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
        .label(&conflict_details, existing_symbol.source_location.unwrap_or_default());

      self.reports.add(file_id, conflict_report);
    }
  }

  fn format_symbol_description(&self, symbol: &Symbol) -> String {
    format!("{} {}", symbol.kind.name(), resolve(&symbol.name)).dimmed().bold().to_string()
  }

  pub fn register_symbol(
    &mut self,
    name: Identifier,
    kind: &SymbolKind,
    span: Span,
  ) -> Option<SymbolId> {
    let file_id =
      self.processed_file.expect("when registering a symbol, the current file must be known");
    let symbol_name = resolve(&name);
    let symbol_type = kind.name();

    match self.symbol_table.insert(name, kind.clone(), Some(span.clone())) {
      Ok(id) => Some(id),
      Err(SymbolTableError::SymbolAlreadyExists { existing_id, .. }) => {
        if let Some(existing_symbol) = self.symbol_table.get_symbol(existing_id) {
          self.reports.add(
            file_id,
            ReportBuilder::builder(
              format!("{} already declared", symbol_name.dimmed().bold()),
              ReportKind::Error,
            )
            .label(&format!("redeclared here as a {}", symbol_type.dimmed().bold()), span)
            .label(
              &format!("first declared here as a {}", existing_symbol.kind.name().dimmed().bold()),
              existing_symbol.source_location.unwrap_or_default(),
            ),
          );
        }
        None
      }
      Err(other_error) => {
        self.reports.add(
          file_id,
          ReportBuilder::builder(
            format!("failed to register {}: {:?}", symbol_name.dimmed().bold(), other_error),
            ReportKind::Error,
          )
          .label("failed to register", span),
        );
        None
      }
    }
  }
}

impl<'reports> AstWalker<'reports> for DeclarationCollection<'reports> {
  fn walk_modules(&mut self, modules: &mut Vec<LexedModule>) {
    for module in modules {
      let ModuleId::File(file_id) = module.id else {
        continue;
      };

      self.symbol_table.create_scope(ScopeType::Module(file_id));
      self.set_processed_file(file_id);
      self.walk_ast(&mut module.ast);
      self.symbol_table.exit_scope().unwrap();
    }
  }

  fn walk_item(&mut self, item: &mut Item) {
    let id = match &mut item.kind {
      ItemKind::Function(func) => {
        let sym = self.register_symbol(
          func.name,
          &SymbolKind::Function {
            signature: func.signature.clone(),
            modifiers: func.modifiers.clone(),
          },
          func.span.clone(),
        );

        self.symbol_table.create_scope(ScopeType::Function(func.id));

        self.walk_function_modifiers(&mut func.modifiers);
        self.walk_function_signature(&mut func.signature);

        if let Some(body) = &mut func.body {
          self.walk_expr(body);
        }

        self.symbol_table.exit_scope().unwrap();

        self
          .symbol_table
          .update_symbol_kind(sym.unwrap(), |kind| {
            if let SymbolKind::Function { signature, .. } = kind {
              *signature = func.signature.clone();
            }

            kind.clone()
          })
          .unwrap();

        sym
      }
      ItemKind::Struct(struct_def) => {
        let sym = self.register_symbol(
          struct_def.name,
          &SymbolKind::Struct {
            generics: struct_def.generics.clone(),
            fields: struct_def.fields.clone(),
            methods: vec![],
          },
          struct_def.span.clone(),
        );
        let mut methods = vec![];

        self.symbol_table.create_scope(ScopeType::Struct(struct_def.id));
        for func in &mut struct_def.methods {
          self.walk_item(func);

          let func_id = func.symbol_id.unwrap();
          self.symbol_table.add_symbol_child(sym.unwrap(), func_id);
          methods.push(func_id);
        }
        self.symbol_table.exit_scope().unwrap();

        self
          .symbol_table
          .update_symbol_kind(sym.unwrap(), |_kind| SymbolKind::Struct {
            generics: struct_def.generics.clone(),
            fields: struct_def.fields.clone(),
            methods: methods.clone(),
          })
          .unwrap();

        sym
      }
      ItemKind::Enum(enum_def) => {
        let sym = self.register_symbol(
          enum_def.name,
          &SymbolKind::Enum {
            generics: enum_def.generics.clone(),
            variants: vec![],
            methods: vec![],
            id: enum_def.id,
          },
          enum_def.span.clone(),
        );

        let mut variants = vec![];
        for variant in &mut enum_def.variants {
          let variant_sym = self.register_symbol(
            variant.name,
            &SymbolKind::EnumVariant { parent_enum: sym.unwrap(), data: variant.data.clone() },
            variant.span.clone(),
          );
          variant.symbol_id = variant_sym;
          variants.push(variant_sym.unwrap());
          self.symbol_table.add_symbol_child(sym.unwrap(), variant_sym.unwrap());
        }

        let mut methods = vec![];

        self.symbol_table.create_scope(ScopeType::Enum(enum_def.id));
        for func in &mut enum_def.methods {
          self.walk_item(func);

          let func_id = func.symbol_id.unwrap();
          self.symbol_table.add_symbol_child(sym.unwrap(), func_id);
          methods.push(func_id);
        }
        self.symbol_table.exit_scope().unwrap();

        self
          .symbol_table
          .update_symbol_kind(sym.unwrap(), |_kind| SymbolKind::Enum {
            generics: enum_def.generics.clone(),
            variants,
            methods: methods.clone(),
            id: enum_def.id,
          })
          .unwrap();

        sym
      }
      ItemKind::TypeExtension(ty_ext) => {
        let sym = self.register_symbol(
          default_ident(),
          &SymbolKind::TypeExtension { ty: ty_ext.ty.clone(), methods: vec![] },
          ty_ext.span.clone(),
        );
        let mut methods = vec![];

        self.symbol_table.create_scope(ScopeType::TypeExtension(ty_ext.id));
        for func in &mut ty_ext.items {
          self.walk_item(func);

          let func_id = func.symbol_id.unwrap();
          self.symbol_table.add_symbol_child(sym.unwrap(), func_id);
          methods.push(func_id);
        }
        self.symbol_table.exit_scope().unwrap();

        self
          .symbol_table
          .update_symbol_kind(sym.unwrap(), |_kind| SymbolKind::TypeExtension {
            ty: ty_ext.ty.clone(),
            methods: methods.clone(),
          })
          .unwrap();

        sym
      }
      ItemKind::Import(..) => None,
    };

    item.symbol_id = id;
  }

  fn walk_block(&mut self, stmts: &mut Vec<Stmt>, id: AstId) {
    self.symbol_table.create_scope(ScopeType::Block(id));

    for stmt in stmts {
      self.walk_stmt(stmt);
    }

    self.symbol_table.exit_scope().unwrap();
  }

  fn walk_if(&mut self, if_stmt: &mut If) {
    self.walk_expr(&mut if_stmt.condition);

    self.symbol_table.create_scope(ScopeType::Block(if_stmt.then_branch_id));
    for stmt in &mut if_stmt.then_branch {
      self.walk_stmt(stmt);
    }
    self.symbol_table.exit_scope().unwrap();

    if let Some(else_branch) = &mut if_stmt.else_branch {
      self.walk_else_branch(else_branch);
    }
  }

  fn walk_else_branch(&mut self, else_branch: &mut ElseBranch) {
    match else_branch {
      ElseBranch::Block(statements, else_id) => {
        self.symbol_table.create_scope(ScopeType::Block(*else_id));
        for stmt in statements {
          self.walk_stmt(stmt);
        }
        self.symbol_table.exit_scope().unwrap();
      }
      ElseBranch::If(nested_if) => {
        self.walk_if(nested_if);
      }
    }
  }

  fn visit_parameter(&mut self, param: &mut Parameter) {
    let p = param.clone();
    let sym_id = self.register_symbol(
      p.name,
      &SymbolKind::Parameter {
        ty: p.ty,
        is_variadic: p.kind == ParameterKind::Variadic,
        default_value: p.default_value,
      },
      p.span,
    );
    param.symbol_id = sym_id;
  }

  fn visit_var_decl(&mut self, var_decl: &mut VarDecl) {
    let v = var_decl.clone();

    let sym_id = self.register_symbol(
      v.name,
      &SymbolKind::Variable { ty: v.ty, is_heap: false, is_moved: None },
      v.span,
    );
    var_decl.symbol_id = sym_id;
  }

  fn visit_match_arm(&mut self, _arm: &mut MatchArm) {
    match &mut _arm.pattern {
      Pattern::EnumVariant { fields, .. } => {
        if let Some(fields) = fields {
          for field in fields {
            let sym_id = self.register_symbol(
              field.name,
              &SymbolKind::MatchBinding { ty: Type::Inferred },
              field.span.clone(),
            );

            field.sym_id = sym_id;
          }
        }
      }
      _ => {}
    }
  }
}
