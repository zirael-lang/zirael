use crate::DefId;
use crate::def::{DefKind, Definition};
use crate::errors::{DuplicateDefinition, UndefinedName, UndefinedType};
use crate::module_resolver::ModuleResolver;
use crate::resolver::Resolver;
use crate::scope::ScopeKind;
use crate::symbol::{Symbol, SymbolKind};
use zirael_diagnostics::DiagnosticCtx;
use zirael_parser::ast::ProgramNode;
use zirael_parser::ast::expressions::{Expr, ExprKind};
use zirael_parser::ast::items::{
  ConstItem, EnumItem, FunctionItem, Item, ItemKind, ModItem, StructItem,
  Visibility,
};
use zirael_parser::ast::params::Param;
use zirael_parser::ast::statements::{Block, Statement};
use zirael_parser::ast::types::Type;
use zirael_parser::module::Modules;
use zirael_parser::{
  BuiltinArg, ElseBranch, IfExpr, NodeId, StructMember, TypePath, VariantField,
  VariantPayload,
};
use zirael_source::prelude::{SourceFileId, Span};

/// Resolves names on the AST
pub struct ResolveVisitor<'a> {
  /// Per-module resolution context
  pub module_resolver: ModuleResolver<'a>,
  pub dcx: &'a DiagnosticCtx,
}

impl<'a> ResolveVisitor<'a> {
  pub fn new(
    resolver: &'a Resolver,
    dcx: &'a DiagnosticCtx,
    current_file: SourceFileId,
  ) -> Self {
    Self {
      module_resolver: ModuleResolver::new(resolver, current_file),
      dcx,
    }
  }

  /// Run the full resolution process on all modules.
  pub fn resolve_modules(
    resolver: &'a Resolver,
    modules: &Modules,
    dcx: &'a DiagnosticCtx,
  ) {
    let order = match resolver.import_graph.resolution_order() {
      Ok(order) => order,
      Err(err) => {
        dcx.emit(err);
        return;
      }
    };

    // First pass: collect all top-level definitions
    for file_id in &order {
      let Some(module) = modules.get(*file_id) else {
        continue;
      };

      let mut visitor = ResolveVisitor::new(resolver, dcx, *file_id);
      visitor.collect_definitions(&module.node);
    }

    // Second pass: resolve all references
    for file_id in &order {
      let Some(module) = modules.get(*file_id) else {
        continue;
      };

      let mut visitor = ResolveVisitor::new(resolver, dcx, *file_id);
      visitor.resolve_module(&module.node);
    }
  }

  fn current_file(&self) -> SourceFileId {
    self.module_resolver.current_file
  }

  fn collect_definitions(&mut self, node: &ProgramNode) {
    self.module_resolver.enter_scope(ScopeKind::Module, None);

    for item in &node.items {
      self.collect_item(item);
    }

    self.module_resolver.save_module_rib();
  }

  fn collect_item(&mut self, item: &Item) {
    match &item.kind {
      ItemKind::Function(func) => {
        self.define_function(func, item.visibility);
      }
      ItemKind::Struct(s) => {
        self.define_struct(s, item.visibility);
      }
      ItemKind::Enum(e) => {
        self.define_enum(e, item.visibility);
      }
      ItemKind::Const(c) => {
        self.define_const(c, item.visibility);
      }
      ItemKind::Mod(m) => {
        self.define_mod(m, item.visibility);
      }
    }
  }

  fn resolver(&self) -> &Resolver {
    self.module_resolver.resolver
  }

  fn define_value_item(
    &mut self,
    name: String,
    node_id: NodeId,
    kind: DefKind,
    span: Span,
    vis: Visibility,
  ) -> Option<DefId> {
    if let Some(existing) = self.module_resolver.lookup_value(&name) {
      if let Some(def) = self.resolver().get_definition(existing) {
        self.dcx.emit(DuplicateDefinition {
          name,
          span,
          previous: def.span,
        });
        return None;
      }
    }

    let def = Definition::new(node_id, self.current_file(), kind, span);
    let def_id = self.resolver().add_definition(def);
    self.module_resolver.define_value(name.clone(), def_id);

    if let Some(scope_id) = self.module_resolver.current_scope() {
      let symbol =
        Symbol::new(name.clone(), def_id, SymbolKind::Value, scope_id);
      self.resolver().symbols.insert(symbol);
    }

    if matches!(vis, Visibility::Public(_))
      && self.module_resolver.is_at_module_scope()
    {
      self.module_resolver.export_value(name, def_id);
    }

    Some(def_id)
  }

  fn define_type_item(
    &mut self,
    name: String,
    node_id: NodeId,
    kind: DefKind,
    span: Span,
    vis: Visibility,
  ) -> Option<DefId> {
    if let Some(existing) = self.module_resolver.lookup_type(&name) {
      if let Some(def) = self.resolver().get_definition(existing) {
        self.dcx.emit(DuplicateDefinition {
          name,
          span,
          previous: def.span,
        });
        return None;
      }
    }

    let def = Definition::new(node_id, self.current_file(), kind, span);
    let def_id = self.resolver().add_definition(def);
    self.module_resolver.define_type(name.clone(), def_id);

    if let Some(scope_id) = self.module_resolver.current_scope() {
      let symbol =
        Symbol::new(name.clone(), def_id, SymbolKind::Type, scope_id);
      self.resolver().symbols.insert(symbol);
    }

    if matches!(vis, Visibility::Public(_))
      && self.module_resolver.is_at_module_scope()
    {
      self.module_resolver.export_type(name, def_id);
    }

    Some(def_id)
  }

  fn define_function(&mut self, func: &FunctionItem, vis: Visibility) {
    self.define_value_item(
      func.name.text(),
      func.id,
      DefKind::Function,
      *func.name.span(),
      vis,
    );
  }

  fn define_struct(&mut self, s: &StructItem, vis: Visibility) {
    self.define_type_item(
      s.name.text(),
      s.id,
      DefKind::Struct,
      *s.name.span(),
      vis,
    );
  }

  fn define_enum(&mut self, e: &EnumItem, vis: Visibility) {
    self.define_type_item(
      e.name.text(),
      e.id,
      DefKind::Enum,
      *e.name.span(),
      vis,
    );
  }

  fn define_const(&mut self, c: &ConstItem, vis: Visibility) {
    self.define_value_item(
      c.name.text(),
      c.id,
      DefKind::Const,
      *c.name.span(),
      vis,
    );
  }

  fn define_mod(&mut self, m: &ModItem, vis: Visibility) {
    let name = m.name.text();
    let span = *m.name.span();

    let def = Definition::new(m.id, self.current_file(), DefKind::Module, span);
    let def_id = self.resolver().add_definition(def);

    self.module_resolver.define_value(name.clone(), def_id);
    self.module_resolver.define_type(name.clone(), def_id);

    if let Some(scope_id) = self.module_resolver.current_scope() {
      let symbol =
        Symbol::new(name.clone(), def_id, SymbolKind::Module, scope_id);
      self.resolver().symbols.insert(symbol);
    }

    if matches!(vis, Visibility::Public(_))
      && self.module_resolver.is_at_module_scope()
    {
      self.module_resolver.export_value(name.clone(), def_id);
      self.module_resolver.export_type(name, def_id);
    }
  }

  fn resolve_module(&mut self, node: &ProgramNode) {
    if !self.module_resolver.restore_module_rib() {
      self.module_resolver.enter_scope(ScopeKind::Module, None);
      for item in &node.items {
        self.collect_item(item);
      }
    }

    for item in &node.items {
      self.resolve_item(item);
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_item(&mut self, item: &Item) {
    match &item.kind {
      ItemKind::Function(func) => self.resolve_function(func),
      ItemKind::Struct(s) => self.resolve_struct(s),
      ItemKind::Enum(e) => self.resolve_enum(e),
      ItemKind::Const(c) => self.resolve_const(c),
      ItemKind::Mod(m) => self.resolve_mod(m),
    }
  }

  fn resolve_function(&mut self, func: &FunctionItem) {
    let func_def = self.module_resolver.lookup_value(&func.name.text());
    self
      .module_resolver
      .enter_scope(ScopeKind::Function, func_def);

    if let Some(generics) = &func.generics {
      for param in &generics.params {
        let name = param.name.text();
        let span = *param.name.span();
        let def = Definition::new(
          param.id,
          self.current_file(),
          DefKind::TypeParam,
          span,
        );
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_type(name, def_id);
      }
    }

    for param in &func.params {
      self.resolve_param(param);
    }

    self.resolve_type(&func.return_type);

    if let Some(body) = &func.body {
      self.resolve_block(body);
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_param(&mut self, param: &Param) {
    match param {
      Param::SelfParam(_) => {}
      Param::Regular(regular) => {
        let name = regular.name.text();
        let span = *regular.name.span();
        let def = Definition::new(
          regular.id,
          self.current_file(),
          DefKind::Param,
          span,
        );
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_value(name, def_id);
        self.resolve_type(&regular.ty);
      }
      Param::Variadic(variadic) => {
        let name = variadic.name.text();
        let span = *variadic.name.span();
        let def = Definition::new(
          variadic.id,
          self.current_file(),
          DefKind::Param,
          span,
        );
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_value(name, def_id);
        self.resolve_type(&variadic.ty);
      }
    }
  }

  fn resolve_struct(&mut self, s: &StructItem) {
    let struct_def = self.module_resolver.lookup_type(&s.name.text());
    self
      .module_resolver
      .enter_scope(ScopeKind::Struct, struct_def);

    if let Some(generics) = &s.generics {
      for param in &generics.params {
        let name = param.name.text();
        let span = *param.name.span();
        let def = Definition::new(
          param.id,
          self.current_file(),
          DefKind::TypeParam,
          span,
        );
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_type(name, def_id);
      }
    }

    for member in &s.members {
      match member {
        StructMember::Field(field) => {
          self.resolve_type(&field.ty);
        }
        StructMember::Method(method) => {
          self.module_resolver.enter_scope(ScopeKind::Function, None);

          for param in &method.params {
            self.resolve_param(param);
          }

          if let Some(ret) = &method.return_type {
            self.resolve_type(ret);
          }

          self.resolve_block(&method.body);
          self.module_resolver.leave_scope();
        }
      }
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_enum(&mut self, e: &EnumItem) {
    let enum_def = self.module_resolver.lookup_type(&e.name.text());
    self.module_resolver.enter_scope(ScopeKind::Enum, enum_def);

    if let Some(generics) = &e.generics {
      for param in &generics.params {
        let name = param.name.text();
        let span = *param.name.span();
        let def = Definition::new(
          param.id,
          self.current_file(),
          DefKind::TypeParam,
          span,
        );
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_type(name, def_id);
      }
    }

    for variant in &e.variants {
      if let Some(payload) = &variant.payload {
        match payload {
          VariantPayload::Tuple(fields) => {
            for field in fields {
              match field {
                VariantField::Named { ty, .. } => {
                  self.resolve_type(ty);
                }
                VariantField::Unnamed(ty) => {
                  self.resolve_type(ty);
                }
              }
            }
          }
          VariantPayload::Discriminant(expr) => {
            self.resolve_expr(expr);
          }
        }
      }
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_const(&mut self, c: &ConstItem) {
    self.resolve_type(&c.ty);
    self.resolve_expr(&c.value);
  }

  fn resolve_mod(&mut self, m: &ModItem) {
    self.module_resolver.enter_scope(ScopeKind::Module, None);

    for item in &m.items {
      self.collect_item(item);
    }
    for item in &m.items {
      self.resolve_item(item);
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_block(&mut self, block: &Block) {
    self.module_resolver.enter_scope(ScopeKind::Block, None);

    for stmt in &block.statements {
      self.resolve_statement(stmt);
    }

    self.module_resolver.leave_scope();
  }

  fn resolve_statement(&mut self, stmt: &Statement) {
    match stmt {
      Statement::VarDecl(var) => {
        self.resolve_expr(&var.value);

        if let Some(ty) = &var.ty {
          self.resolve_type(ty);
        }

        let name = var.name.text();
        let span = *var.name.span();
        let def =
          Definition::new(var.id, self.current_file(), DefKind::Local, span);
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_value(name, def_id);
      }
      Statement::ConstDecl(c) => {
        self.resolve_expr(&c.value);
        if let Some(ty) = &c.ty {
          self.resolve_type(ty);
        }

        let name = c.name.text();
        let span = *c.name.span();
        let def =
          Definition::new(c.id, self.current_file(), DefKind::Const, span);
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_value(name, def_id);
      }
      Statement::Expr(expr_stmt) => {
        self.resolve_expr(&expr_stmt.expr);
      }
      Statement::Block(block) => {
        self.resolve_block(block);
      }
    }
  }

  fn resolve_if_expr(&mut self, if_expr: &IfExpr) {
    self.resolve_expr(&if_expr.condition);
    self.resolve_block(&if_expr.then_block);
    if let Some(else_branch) = &if_expr.else_branch {
      match else_branch {
        ElseBranch::Block(block) => {
          self.resolve_block(block);
        }
        ElseBranch::If(else_if) => {
          self.resolve_if_expr(else_if);
        }
      }
    }
  }

  fn resolve_expr(&mut self, expr: &Expr) {
    match &expr.kind {
      ExprKind::Path(path) => {
        // single segment path is a local name lookup
        if path.root.is_none() && path.segments.len() == 1 {
          let name = path.segments[0].text();
          if let Some(def_id) = self.module_resolver.lookup_value(&name) {
            // Record the resolution
            self.resolver().symbols.record_resolution(expr.id, def_id);
          } else {
            self.dcx.emit(UndefinedName {
              name,
              span: expr.span,
            });
          }
        }
        // TODO: multi segment path resolution
      }

      ExprKind::Binary { left, right, .. } => {
        self.resolve_expr(left);
        self.resolve_expr(right);
      }

      ExprKind::Unary { operand, .. } => {
        self.resolve_expr(operand);
      }

      ExprKind::Assign { target, value, .. } => {
        self.resolve_expr(target);
        self.resolve_expr(value);
      }

      ExprKind::Ternary {
        condition,
        then_expr,
        else_expr,
      } => {
        self.resolve_expr(condition);
        self.resolve_expr(then_expr);
        self.resolve_expr(else_expr);
      }

      ExprKind::Cast { expr, target_type } => {
        self.resolve_expr(expr);
        self.resolve_type(target_type);
      }

      ExprKind::Call { callee, args } => {
        self.resolve_expr(callee);
        for arg in args {
          self.resolve_expr(&arg.value);
        }
      }

      ExprKind::Field { object, .. } => {
        self.resolve_expr(object);
      }

      ExprKind::Index { object, index } => {
        self.resolve_expr(object);
        self.resolve_expr(index);
      }

      ExprKind::AddrOf { operand, .. } => {
        self.resolve_expr(operand);
      }

      ExprKind::Struct { path, fields } => {
        self.resolve_type_path(path);
        for field in fields {
          self.resolve_expr(&field.value);
        }
      }

      ExprKind::If(if_expr) => {
        self.resolve_if_expr(if_expr);
      }

      ExprKind::Match(match_expr) => {
        self.resolve_expr(&match_expr.scrutinee);
        for arm in &match_expr.arms {
          // Pattern binding would go here
          self.resolve_expr(&arm.body);
        }
      }

      ExprKind::Block(block) => {
        self.resolve_block(block);
      }

      ExprKind::Loop(loop_expr) => {
        self.module_resolver.enter_scope(ScopeKind::Loop, None);
        self.resolve_block(&loop_expr.body);
        self.module_resolver.leave_scope();
      }

      ExprKind::While(while_expr) => {
        self.resolve_expr(&while_expr.condition);
        self.module_resolver.enter_scope(ScopeKind::Loop, None);
        self.resolve_block(&while_expr.body);
        self.module_resolver.leave_scope();
      }

      ExprKind::For(for_expr) => {
        self.resolve_expr(&for_expr.iterator);
        self.module_resolver.enter_scope(ScopeKind::Loop, None);

        let name = for_expr.binding.text();
        let span = *for_expr.binding.span();
        let def = Definition::new(
          for_expr.id,
          self.current_file(),
          DefKind::Local,
          span,
        );
        let def_id = self.resolver().add_definition(def);
        self.module_resolver.define_value(name, def_id);

        self.resolve_block(&for_expr.body);
        self.module_resolver.leave_scope();
      }

      ExprKind::Break(break_expr) => {
        if let Some(value) = &break_expr.value {
          self.resolve_expr(value);
        }
      }

      ExprKind::Return(return_expr) => {
        if let Some(value) = &return_expr.value {
          self.resolve_expr(value);
        }
      }

      ExprKind::Range(range_expr) => {
        if let Some(start) = &range_expr.start {
          self.resolve_expr(start);
        }
        if let Some(end) = &range_expr.end {
          self.resolve_expr(end);
        }
      }

      ExprKind::Tuple(exprs) | ExprKind::Array(exprs) => {
        for e in exprs {
          self.resolve_expr(e);
        }
      }

      ExprKind::Builtin { args, .. } => {
        for arg in args {
          match arg {
            BuiltinArg::Expr(e) => {
              self.resolve_expr(e);
            }
            BuiltinArg::Type(t) => {
              self.resolve_type(t);
            }
          }
        }
      }

      ExprKind::Literal(_) | ExprKind::SelfValue | ExprKind::Continue(_) => {}
    }
  }

  fn resolve_type(&mut self, ty: &Type) {
    match ty {
      Type::Path(type_path) => {
        self.resolve_type_path(type_path);
      }
      Type::Function(func_ty) => {
        for param in &func_ty.params {
          self.resolve_type(param);
        }
        self.resolve_type(&func_ty.return_type);
      }
      Type::Pointer(ptr) => {
        self.resolve_type(&ptr.inner);
      }
      Type::Optional(opt) => {
        self.resolve_type(&opt.inner);
      }
      Type::Array(arr) => {
        self.resolve_type(&arr.element);
        self.resolve_expr(&arr.size);
      }
      Type::Tuple(tuple) => {
        for elem in &tuple.elements {
          self.resolve_type(elem);
        }
      }
      Type::Primitive(_) | Type::Unit(_) | Type::Never(_) | Type::Invalid => {}
    }
  }

  fn resolve_type_path(&mut self, type_path: &TypePath) {
    let path = &type_path.path;

    // single segment
    if path.root.is_none() && path.segments.len() == 1 {
      let name = path.segments[0].text();
      if let Some(def_id) = self.module_resolver.lookup_type(&name) {
        self
          .resolver()
          .symbols
          .record_resolution(type_path.id, def_id);
      } else {
        self.dcx.emit(UndefinedType {
          name,
          span: type_path.span,
        });
      }
    }
    // TODO: multi segment paths

    if let Some(args) = &type_path.args {
      for arg in args {
        self.resolve_type(arg);
      }
    }
  }
}
