use crate::{
  AstId, CallInfo, LexedModule, ModuleId, Path, PathSegment, Return, ScopeType, SymbolTable,
  TypeExtension,
  ast::{
    Abi, Ast, Attribute, BinaryOp, ElseBranch, EnumDeclaration, EnumVariant, EnumVariantData, Expr,
    ExprKind, Function, FunctionModifiers, FunctionSignature, GenericArg, GenericParameter, If,
    ImportKind, Item, ItemKind, Literal, MatchArm, Parameter, ParameterKind, Pattern, PatternField,
    Stmt, StmtKind, StructDeclaration, StructField, TraitBound, Type, UnaryOp, VarDecl,
  },
  symbols::SymbolId,
};
use std::ops::Range;
use zirael_utils::prelude::*;

pub trait WalkerContext<'reports> {
  fn symbol_table(&self) -> &SymbolTable;
  fn symbol_table_mut(&mut self) -> &mut SymbolTable;
  fn reports(&self) -> &Reports<'reports>;
  fn processed_file(&self) -> Option<SourceFileId>;
  fn set_processed_file(&mut self, file_id: SourceFileId);
  fn sources(&self) -> &Sources;
}

pub trait AstWalker<'reports>: WalkerContext<'reports> {
  fn walk_modules(&mut self, modules: &mut Vec<LexedModule>) {
    for module in modules {
      let ModuleId::File(file_id) = module.id else {
        continue;
      };

      self.push_scope(ScopeType::Module(file_id));
      self.set_processed_file(file_id);
      self.walk_ast(&mut module.ast);
      self.pop_scope();
    }
  }

  fn walk_ast(&mut self, ast: &mut Ast) {
    for item in &mut ast.items {
      self.walk_item(item);
    }
  }

  fn walk_item(&mut self, item: &mut Item) {
    self.visit_item(item);

    for attr in &mut item.attributes {
      self.walk_attribute(attr);
    }

    self.walk_item_kind(&mut item.kind);
  }

  fn walk_item_kind(&mut self, kind: &mut ItemKind) {
    match kind {
      ItemKind::Function(func) => self.walk_function(func),
      ItemKind::Struct(_struct) => self.walk_struct_declaration(_struct),
      ItemKind::Enum(enum_decl) => self.walk_enum_declaration(enum_decl),
      ItemKind::Import(import, _) => self.walk_import_kind(import),
      ItemKind::TypeExtension(ty_ext) => self.walk_type_extension(ty_ext),
    }
  }

  fn walk_import_kind(&mut self, import: &mut ImportKind) {
    self.visit_import_kind(import);
    match import {
      ImportKind::Path(_) => {}
      ImportKind::ExternalModule(_identifiers) => {}
    }
  }

  fn walk_function(&mut self, func: &mut Function) {
    self.visit_function(func);
    self.push_scope(ScopeType::Function(func.id));

    self.walk_function_modifiers(&mut func.modifiers);
    self.walk_function_signature(&mut func.signature);

    if let Some(body) = &mut func.body {
      self.walk_expr(body);
    }

    self.pop_scope();
  }

  fn walk_function_modifiers(&mut self, modifiers: &mut FunctionModifiers) {
    self.visit_function_modifiers(modifiers);
    if let Some(abi) = &mut modifiers.abi {
      self.walk_abi(abi);
    }
  }

  fn walk_abi(&mut self, abi: &mut Abi) {
    self.visit_abi(abi);
  }

  fn walk_function_signature(&mut self, sig: &mut FunctionSignature) {
    self.visit_function_signature(sig);

    for generic in &mut sig.generics {
      self.walk_generic_parameter(generic);
    }

    for param in &mut sig.parameters {
      self.walk_parameter(param);
    }

    self.walk_type(&mut sig.return_type);
  }

  fn walk_parameter(&mut self, param: &mut Parameter) {
    self.visit_parameter(param);
    self.walk_type(&mut param.ty);
    self.walk_parameter_kind(&mut param.kind);

    if let Some(default) = &mut param.default_value {
      self.walk_expr(default);
    }
  }

  fn walk_parameter_kind(&mut self, kind: &mut ParameterKind) {
    self.visit_parameter_kind(kind);
  }

  fn walk_attribute(&mut self, attr: &mut Attribute) {
    self.visit_attribute(attr);

    if let Some(args) = &mut attr.args {
      for arg in args {
        self.walk_expr(arg);
      }
    }
  }

  fn walk_type_extension(&mut self, _ty_ext: &mut TypeExtension) {
    self.visit_type_extension(_ty_ext);

    self.push_scope(ScopeType::TypeExtension(_ty_ext.id));

    for item in &mut _ty_ext.items {
      self.walk_item(item);
    }

    self.pop_scope();
  }

  fn walk_struct_declaration(&mut self, _struct: &mut StructDeclaration) {
    self.visit_struct_declaration(_struct);

    self.push_scope(ScopeType::Struct(_struct.id));

    for generic in &mut _struct.generics {
      self.walk_generic_parameter(generic);
    }

    for field in &mut _struct.fields {
      self.walk_struct_field(field);
    }

    for item in &mut _struct.methods {
      self.walk_item(item);
    }

    self.pop_scope();
  }

  fn walk_struct_field(&mut self, field: &mut StructField) {
    self.visit_struct_field(field);
    self.walk_type(&mut field.ty);

    for attr in &mut field.attributes {
      self.walk_attribute(attr);
    }
  }

  fn walk_enum_declaration(&mut self, en: &mut EnumDeclaration) {
    self.visit_enum_declaration(en);

    self.push_scope(ScopeType::Enum(en.id));

    for generic in &mut en.generics {
      self.walk_generic_parameter(generic);
    }

    for variant in &mut en.variants {
      self.walk_enum_variant(variant);
    }

    for item in &mut en.methods {
      self.walk_item(item);
    }

    self.pop_scope();
  }

  fn walk_enum_variant(&mut self, variant: &mut EnumVariant) {
    self.visit_enum_variant(variant);
    self.walk_enum_variant_data(&mut variant.data);

    for attr in &mut variant.attributes {
      self.walk_attribute(attr);
    }
  }

  fn walk_enum_variant_data(&mut self, data: &mut EnumVariantData) {
    self.visit_enum_variant_data(data);
    match data {
      EnumVariantData::Unit => {}
      EnumVariantData::Struct(fields) => {
        for field in fields {
          self.walk_struct_field(field);
        }
      }
    }
  }

  fn walk_expr(&mut self, expr: &mut Expr) {
    self.visit_expr(expr);
    self.walk_expr_kind(expr);
  }

  fn walk_block(&mut self, stmts: &mut Vec<Stmt>, id: AstId) {
    self.push_scope(ScopeType::Block(id));

    for stmt in stmts {
      self.walk_stmt(stmt);
    }

    self.pop_scope();
  }

  fn walk_expr_kind(&mut self, expr: &mut Expr) {
    match &mut expr.kind {
      ExprKind::Literal(lit) => self.walk_literal(lit),
      ExprKind::Identifier(id, sym_id) => self.walk_identifier(id, sym_id, expr.span.clone()),
      ExprKind::Path(path) => self.walk_path(path),
      ExprKind::Binary { left, op, right } => {
        self.walk_expr(left);
        self.visit_binary_op(op);
        self.walk_expr(right);
      }
      ExprKind::Ternary { condition, true_expr, false_expr } => {
        self.walk_expr(condition);
        self.walk_expr(true_expr);
        self.walk_expr(false_expr);
      }
      ExprKind::Block(stmts) => self.walk_block(stmts, expr.id),
      ExprKind::Assign(lhs, rhs) => {
        self.visit_assign(lhs, rhs);
        self.walk_expr(lhs);
        self.walk_expr(rhs);
      }
      ExprKind::AssignOp(lhs, op, rhs) => {
        self.walk_expr(lhs);
        self.visit_binary_op(op);
        self.walk_expr(rhs);
      }
      ExprKind::Unary(op, expr) => {
        self.visit_unary(op, expr);
        self.walk_expr(expr);
      }
      ExprKind::Paren(expr) => {
        self.walk_expr(expr);
      }
      ExprKind::Call { callee, call, .. } => {
        self.visit_function_call(callee, &mut call.args);
        for arg in &mut call.args {
          self.walk_expr(arg);
        }
      }
      ExprKind::FieldAccess(exprs) => self.visit_field_access(exprs),
      ExprKind::MethodCall { chain, call } => {
        self.visit_method_call(chain, &mut call.args, &mut call.call_info);
      }
      ExprKind::StaticCall { callee, call } => {
        self.visit_static_call(callee, &mut call.args, &mut call.call_info);
      }
      ExprKind::IndexAccess(expr, index) => {
        self.walk_expr(expr);
        self.walk_expr(index);
      }
      ExprKind::StructInit { fields, name, .. } => {
        self.visit_struct_init(name, fields);
        for field in fields.values_mut() {
          self.walk_expr(field);
        }
      }
      ExprKind::Match { scrutinee, arms } => {
        self.visit_match(scrutinee, arms);
        self.walk_expr(scrutinee);
        for arm in arms {
          self.walk_match_arm(arm);
        }
      }
      ExprKind::CouldntParse(_) => {}
    }
  }

  fn walk_literal(&mut self, lit: &mut Literal) {
    self.visit_literal(lit);
  }

  fn walk_generic_parameter(&mut self, generic: &mut GenericParameter) {
    self.visit_generic_parameter(generic);

    for constraint in &mut generic.constraints {
      self.walk_trait_bound(constraint);
    }

    if let Some(default) = &mut generic.default_type {
      self.walk_type(default);
    }
  }

  fn walk_trait_bound(&mut self, bound: &mut TraitBound) {
    self.visit_trait_bound(bound);

    for arg in &mut bound.generic_args {
      self.walk_generic_arg(arg);
    }
  }

  fn walk_generic_arg(&mut self, arg: &mut GenericArg) {
    self.visit_generic_arg(arg);
    match arg {
      GenericArg::Type(ty) => self.walk_type(ty),
      GenericArg::Named { name: _, ty } => {
        self.walk_type(ty);
      }
    }
  }

  fn walk_type(&mut self, ty: &mut Type) {
    self.visit_type(ty);
    match ty {
      Type::String
      | Type::Char
      | Type::Int
      | Type::Uint
      | Type::Float
      | Type::Bool
      | Type::Void
      | Type::Inferred
      | Type::Variable { .. } => {}
      Type::MonomorphizedSymbol(_) => {}
      Type::Pointer(inner) | Type::Reference(inner) => {
        self.walk_type(inner);
      }
      Type::Array(inner, _) => {
        self.walk_type(inner);
      }
      Type::Function { params, return_type } => {
        for param in params {
          self.walk_type(param);
        }
        self.walk_type(return_type);
      }
      Type::Named { name: _, generics } => {
        for generic in generics {
          self.walk_type(generic);
        }
      }
      _ => warn!("Unhandled type: {ty:?}"),
    }
  }

  fn walk_identifier(&mut self, id: &mut Identifier, sym_id: &mut Option<SymbolId>, span: Span) {
    self.visit_identifier(id, sym_id, span);
  }

  fn walk_path(&mut self, path: &mut Path) {
    self.visit_path(path);
    for segment in &mut path.segments {
      self.walk_path_segment(segment);
    }
  }

  fn walk_path_segment(&mut self, segment: &mut PathSegment) {
    self.visit_path_segment(segment);
    for type_arg in &mut segment.type_args {
      self.walk_type(type_arg);
    }
  }

  fn walk_stmt(&mut self, stmt: &mut Stmt) {
    self.visit_stmt(stmt);
    self.walk_stmt_kind(&mut stmt.0);
  }

  fn walk_stmt_kind(&mut self, kind: &mut StmtKind) {
    self.visit_stmt_kind(kind);
    match kind {
      StmtKind::Expr(expr) => self.walk_expr(expr),
      StmtKind::Var(var_decl) => self.walk_var_decl(var_decl),
      StmtKind::Return(ret) => self.walk_return(ret),
      StmtKind::If(if_stmt) => self.walk_if(if_stmt),
    }
  }

  fn walk_return(&mut self, ret: &mut Return) {
    self.visit_return(ret);
    if let Some(expr) = &mut ret.value {
      self.walk_expr(expr);
    }
  }

  fn walk_if(&mut self, if_stmt: &mut If) {
    self.visit_if(if_stmt);

    self.walk_expr(&mut if_stmt.condition);

    self.push_scope(ScopeType::Block(if_stmt.then_branch_id));
    for stmt in &mut if_stmt.then_branch {
      self.walk_stmt(stmt);
    }
    self.pop_scope();

    if let Some(else_branch) = &mut if_stmt.else_branch {
      self.walk_else_branch(else_branch);
    }
  }

  fn walk_else_branch(&mut self, else_branch: &mut ElseBranch) {
    self.visit_else_branch(else_branch);
    match else_branch {
      ElseBranch::Block(statements, else_branch_id) => {
        self.push_scope(ScopeType::Block(*else_branch_id));
        for stmt in statements {
          self.walk_stmt(stmt);
        }
        self.pop_scope();
      }
      ElseBranch::If(nested_if) => {
        self.walk_if(nested_if);
      }
    }
  }

  fn walk_var_decl(&mut self, var_decl: &mut VarDecl) {
    self.visit_var_decl(var_decl);
    self.walk_type(&mut var_decl.ty);
    self.walk_expr(&mut var_decl.value);
  }

  fn push_scope(&mut self, scope_type: ScopeType) {
    let _ = self.symbol_table_mut().push_scope(scope_type);
  }

  fn pop_scope(&mut self) {
    if let Err(err) = self.symbol_table_mut().pop_scope() {
      self.error(&format!("Failed to pop scope: {err:?}"), vec![], vec![]);
    }
  }

  fn error(&mut self, message: &str, labels: Vec<(String, Span)>, notes: Vec<String>) {
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
      warn!("Report outside of a file: {message}");
    }
  }

  fn report(&mut self, report: ReportBuilder<'reports>) {
    if let Some(file_id) = self.processed_file() {
      self.reports().add(file_id, report);
    } else {
      warn!("Report outside of a file: {report:?}");
    }
  }

  fn visit_item(&mut self, _item: &mut Item) {}
  fn visit_import_kind(&mut self, _import: &mut ImportKind) {}
  fn visit_function(&mut self, _func: &mut Function) {}
  fn visit_function_modifiers(&mut self, _modifiers: &mut FunctionModifiers) {}
  fn visit_abi(&mut self, _abi: &mut Abi) {}
  fn visit_function_signature(&mut self, _sig: &mut FunctionSignature) {}
  fn visit_parameter(&mut self, _param: &mut Parameter) {}
  fn visit_parameter_kind(&mut self, _kind: &mut ParameterKind) {}
  fn visit_attribute(&mut self, _attr: &mut Attribute) {}
  fn visit_struct_declaration(&mut self, _struct: &mut StructDeclaration) {}
  fn visit_type_extension(&mut self, _ty_ext: &mut TypeExtension) {}
  fn visit_struct_field(&mut self, _field: &mut StructField) {}
  fn visit_enum_declaration(&mut self, _enum_decl: &mut EnumDeclaration) {}
  fn visit_enum_variant(&mut self, _variant: &mut EnumVariant) {}
  fn visit_enum_variant_data(&mut self, _data: &mut EnumVariantData) {}
  fn visit_expr(&mut self, _expr: &mut Expr) {}
  fn visit_literal(&mut self, _lit: &mut Literal) {}
  fn visit_binary_op(&mut self, _op: &mut BinaryOp) {}
  fn visit_unary(&mut self, _op: &mut UnaryOp, _expr: &mut Expr) {}
  fn visit_stmt_kind(&mut self, _kind: &mut StmtKind) {}
  fn visit_generic_parameter(&mut self, _param: &mut GenericParameter) {}
  fn visit_trait_bound(&mut self, _bound: &mut TraitBound) {}
  fn visit_generic_arg(&mut self, _arg: &mut GenericArg) {}
  fn visit_type(&mut self, _ty: &mut Type) {}
  fn visit_identifier(
    &mut self,
    _id: &mut Identifier,
    _sym_id: &mut Option<SymbolId>,
    _span: Span,
  ) {
  }
  fn visit_path(&mut self, _path: &mut Path) {}
  fn visit_path_segment(&mut self, _segment: &mut PathSegment) {}
  fn visit_stmt(&mut self, _stmt: &mut Stmt) {}
  fn visit_var_decl(&mut self, _var_decl: &mut VarDecl) {}
  fn visit_function_call(&mut self, _callee: &mut Expr, _args: &mut [Expr]) {}
  fn visit_box(&mut self, _expr: &mut Expr) {}
  fn visit_assign(&mut self, _lhs: &mut Expr, _rhs: &mut Expr) {}
  fn visit_return(&mut self, _ret: &mut Return) {}
  fn visit_if(&mut self, _if_stmt: &mut If) {}
  fn visit_else_branch(&mut self, _else_branch: &mut ElseBranch) {}
  fn visit_struct_init(&mut self, _name: &mut Expr, _fields: &mut HashMap<Identifier, Expr>) {}
  fn visit_field_access(&mut self, _exprs: &mut Vec<Expr>) {}
  fn visit_method_call(
    &mut self,
    _chain: &mut Vec<Expr>,
    _args: &mut Vec<Expr>,
    _call_info: &mut Option<CallInfo>,
  ) {
  }
  fn visit_static_call(
    &mut self,
    _callee: &mut Expr,
    _args: &mut Vec<Expr>,
    _call_info: &mut Option<CallInfo>,
  ) {
  }
  fn visit_match(&mut self, _scrutinee: &mut Expr, _arms: &mut Vec<MatchArm>) {}
  fn visit_match_arm(&mut self, _arm: &mut MatchArm) {}
  fn visit_pattern(&mut self, _pattern: &mut Pattern) {}
  fn visit_pattern_field(&mut self, _field: &mut PatternField) {}

  fn walk_match_arm(&mut self, arm: &mut MatchArm) {
    self.visit_match_arm(arm);
    self.walk_pattern(&mut arm.pattern);
    self.walk_expr(&mut arm.body);
  }

  fn walk_pattern(&mut self, pattern: &mut Pattern) {
    self.visit_pattern(pattern);
    match pattern {
      Pattern::Wildcard => {}
      Pattern::Identifier(_) => {}
      Pattern::Literal(_) => {}
      Pattern::EnumVariant { fields, .. } => {
        if let Some(fields) = fields {
          for field in fields {
            self.walk_pattern_field(field);
          }
        }
      }
      Pattern::Struct { fields, .. } => {
        for field in fields {
          self.walk_pattern_field(field);
        }
      }
    }
  }

  fn walk_pattern_field(&mut self, field: &mut PatternField) {
    self.visit_pattern_field(field);
    if let Some(pattern) = &mut field.pattern {
      self.walk_pattern(pattern);
    }
  }
}

#[macro_export]
macro_rules! impl_ast_walker {
    ($struct_name:ident) => {
        impl_ast_walker!($struct_name, {}, with_defaults);
    };

    ($struct_name:ident, { $($field_name:ident: $field_type:ty),* $(,)? }) => {
        impl_ast_walker!($struct_name, { $($field_name: $field_type),* }, with_defaults);
    };

    ($struct_name:ident, { $($field_name:ident: $field_type:ty),* $(,)? }, no_defaults) => {
        impl_ast_walker!($struct_name, { $($field_name: $field_type),* }, no_defaults, {});
    };

    ($struct_name:ident, { $($field_name:ident: $field_type:ty),* $(,)? }, custom, { $($custom_impl:item)* }) => {
        impl_ast_walker!($struct_name, { $($field_name: $field_type),* }, custom, { $($custom_impl)* });
    };

    ($struct_name:ident, { $($field_name:ident: $field_type:ty),* }, with_defaults) => {
        #[derive(Debug, Clone)]
        pub struct $struct_name<'reports> {
            pub symbol_table: SymbolTable,
            pub reports: Reports<'reports>,
            pub processed_file: Option<SourceFileId>,
            pub sources: Sources,
            $(pub $field_name: $field_type,)*
        }

        impl<'reports> $struct_name<'reports> {
            pub fn new(
                table: &SymbolTable,
                reports: &Reports<'reports>,
                sources: &Sources,
            ) -> Self {
                Self {
                    symbol_table: table.clone(),
                    reports: reports.clone(),
                    processed_file: None,
                    sources: sources.clone(),
                    $($field_name: Default::default(),)*
                }
            }

            pub fn new_no_defaults(
                table: &SymbolTable,
                reports: &Reports<'reports>,
                sources: &Sources,
                $($field_name: $field_type,)*
            ) -> Self {
                Self {
                    symbol_table: table.clone(),
                    reports: reports.clone(),
                    processed_file: None,
                    sources: sources.clone(),
                    $($field_name,)*
                }
            }
        }

        impl<'reports> WalkerContext<'reports> for $struct_name<'reports> {
            fn symbol_table(&self) -> &SymbolTable {
                &self.symbol_table
            }

            fn symbol_table_mut(&mut self) -> &mut SymbolTable {
                &mut self.symbol_table
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

            fn sources(&self) -> &Sources {
                &self.sources
            }
        }
    };

    ($struct_name:ident, { $($field_name:ident: $field_type:ty),* }, no_defaults, { $($custom_impl:item)* }) => {
        pub struct $struct_name<'reports> {
            pub symbol_table: SymbolTable,
            pub reports: Reports<'reports>,
            pub processed_file: Option<SourceFileId>,
            pub sources: Sources,
            $(pub $field_name: $field_type,)*
        }

        impl<'reports> $struct_name<'reports> {
            pub fn new_no_defaults(
                table: &SymbolTable,
                reports: &Reports<'reports>,
                sources: &Sources,
                $($field_name: $field_type,)*
            ) -> Self {
                Self {
                    symbol_table: table.clone(),
                    reports: reports.clone(),
                    processed_file: None,
                    sources: sources.clone(),
                    $($field_name,)*
                }
            }
        }

        impl_ast_walker!(@maybe_impl_walker_context, $struct_name, { $($custom_impl)* });

        $($custom_impl)*
    };

    (@maybe_impl_walker_context, $struct_name:ident, {}) => {
        impl<'reports> WalkerContext<'reports> for $struct_name<'reports> {
            fn symbol_table(&self) -> &SymbolTable {
                &self.symbol_table
            }

            fn symbol_table_mut(&mut self) -> &mut SymbolTable {
                &mut self.symbol_table
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

            fn sources(&self) -> &Sources {
                &self.sources
            }
        }
    };

    (@maybe_impl_walker_context, $struct_name:ident, { $($custom_impl:item)+ }) => {
    };
}
