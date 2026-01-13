use crate::DefId;
use crate::def::{DefKind, Definition};
use crate::errors::{
  DuplicateDefinition, InvalidPathRoot, UndefinedName, UndefinedNameInModule,
  UndefinedType,
};
use crate::module_resolver::ModuleResolver;
use crate::resolver::Resolver;
use crate::scope::ScopeKind;
use crate::symbol::{Symbol, SymbolKind};
use zirael_parser::ast::ProgramNode;
use zirael_parser::ast::expressions::{Expr, ExprKind};
use zirael_parser::ast::items::{
  ConstItem, EnumItem, FunctionItem, Item, ItemKind, ModItem, StructItem,
  Visibility,
};
use zirael_parser::ast::params::Param;
use zirael_parser::ast::statements::{Block, Statement};
use zirael_parser::ast::types::Type;
use zirael_parser::import::ImportDecl;
use zirael_parser::module::Modules;
use zirael_parser::{
  BuiltinArg, ElseBranch, IfExpr, ImportKind, ImportSpec, NodeId, Path,
  StructMember, VariantField, VariantPayload,
};
use zirael_source::prelude::{SourceFileId, Span};
use zirael_utils::context::Context;
use zirael_utils::prelude::Identifier;

pub struct ResolveVisitor<'a> {
  pub module_resolver: ModuleResolver<'a>,
  pub ctx: &'a Context<'a>,
}

impl<'a> ResolveVisitor<'a> {
  pub fn new(
    resolver: &'a Resolver,
    ctx: &'a Context<'a>,
    current_file: SourceFileId,
  ) -> Self {
    Self {
      module_resolver: ModuleResolver::new(resolver, current_file),
      ctx,
    }
  }

  pub fn resolve_modules(
    resolver: &'a Resolver,
    modules: &Modules,
    ctx: &'a Context<'a>,
  ) {
    // collect all top-level definitions
    for module in modules.all() {
      let mut visitor =
        ResolveVisitor::new(resolver, ctx, module.source_file_id);
      visitor.collect_definitions(&module.node);
    }

    let order = match resolver.import_graph.resolution_order() {
      Ok(order) => order,
      Err(err) => {
        ctx.dcx().emit(err);
        return;
      }
    };

    // resolve all references
    for file_id in &order {
      let Some(module) = modules.get(*file_id) else {
        continue;
      };

      let mut visitor = ResolveVisitor::new(resolver, ctx, *file_id);
      visitor.resolve_module(&module.node);
    }
  }

  fn current_file(&self) -> SourceFileId {
    self.module_resolver.current_file
  }

  fn collect_definitions(&mut self, node: &ProgramNode) {
    self.module_resolver.enter_scope(ScopeKind::Module, None);

    for import in &node.imports {
      self.collect_import(import);
    }

    for item in &node.items {
      self.collect_item(item);
    }

    self.module_resolver.save_module_rib();
  }

  fn collect_import(&mut self, import: &ImportDecl) {
    let sess = self.ctx.session;

    let current = sess.dcx().sources().get_unchecked(self.current_file());
    let path = import
      .path
      .construct_file(sess.root(), current.path().clone());

    let Some(path) = path else {
      todo!("add proper diagnostic")
    };

    let importing_from = sess.dcx().sources().get_by_path(&path);

    if let Some(source_id) = importing_from {
      let src_id = *source_id.value();
      self.resolver().add_path_mapping(&import.path, src_id);
      self.resolver().add_import_edge(src_id, self.current_file());
    } else {
      panic!("fix")
    }
  }

  fn resolve_import(&mut self, import: &ImportDecl) {
    let target_file = self.resolver().lookup_file_for_path(&import.path);

    match &import.kind {
      ImportKind::Wildcard => self.resolve_wildcard_import(target_file),
      ImportKind::Items(items) => self.resolve_items_import(target_file, items),
      ImportKind::Binding(name) => {
        self.resolve_binding_import(target_file, *name, import)
      }
    }
  }

  fn resolve_binding_import(
    &mut self,
    target_file: SourceFileId,
    binding: Identifier,
    import: &ImportDecl,
  ) {
    let def =
      Definition::new(import.id, target_file, DefKind::Module, *binding.span());
    let def_id = self.resolver().add_definition(def);

    let name = binding.text();
    self.module_resolver.define_value(name.clone(), def_id);
    self.module_resolver.define_type(name.clone(), def_id);

    self.resolver().symbols.record_resolution(import.id, def_id);

    if let Some(scope_id) = self.module_resolver.current_scope() {
      let symbol = Symbol::new(name, def_id, SymbolKind::Module, scope_id);
      self.resolver().symbols.insert(symbol);
    }
  }

  fn resolve_items_import(
    &mut self,
    target_file: SourceFileId,
    specs: &Vec<ImportSpec>,
  ) {
    for spec in specs {
      let (orig_name, local_name) = {
        let orig = spec.name.text();
        let local = spec
          .alias
          .as_ref()
          .map(|a| a.text())
          .unwrap_or_else(|| orig.clone());
        (orig, local)
      };

      let mut found = false;
      if let Some(def_id) =
        self.resolver().lookup_module_value(target_file, &orig_name)
      {
        self
          .module_resolver
          .define_value(local_name.clone(), def_id);
        self.resolver().symbols.record_resolution(spec.id, def_id);
        found = true;
      }

      if let Some(def_id) =
        self.resolver().lookup_module_type(target_file, &orig_name)
      {
        self.module_resolver.define_type(local_name, def_id);
        if !found {
          self.resolver().symbols.record_resolution(spec.id, def_id);
        }
        found = true;
      }

      if !found {
        self.ctx.dcx().emit(UndefinedName {
          name: orig_name,
          span: spec.span,
        });
      }
    }
  }

  fn resolve_wildcard_import(&mut self, target_file: SourceFileId) {
    if let Some(exports) = self
      .resolver()
      .module_exports_values
      .get(&target_file)
      .map(|v| v.clone())
    {
      for entry in exports.iter() {
        let name = entry.key().clone();
        let def_id = *entry.value();
        self.module_resolver.define_value(name, def_id);
      }
    }

    if let Some(exports) = self
      .resolver()
      .module_exports_types
      .get(&target_file)
      .map(|v| v.clone())
    {
      for entry in exports.iter() {
        let name = entry.key().clone();
        let def_id = *entry.value();
        self.module_resolver.define_type(name, def_id);
      }
    }
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
        self.ctx.dcx().emit(DuplicateDefinition {
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

    self.resolver().symbols.record_resolution(node_id, def_id);

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
        self.ctx.dcx().emit(DuplicateDefinition {
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

    // Record the resolution so we can look up DefId from NodeId during HIR lowering
    self.resolver().symbols.record_resolution(node_id, def_id);

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

    self.resolver().symbols.record_resolution(m.id, def_id);

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

    self
      .module_resolver
      .enter_scope(ScopeKind::Module, Some(def_id));
    for item in &m.items {
      self.collect_inline_mod_item(item, def_id);
    }
    self.module_resolver.leave_scope();
  }

  fn collect_inline_mod_item(&mut self, item: &Item, module_def_id: DefId) {
    match &item.kind {
      ItemKind::Function(func) => {
        if let Some(def_id) = self.define_value_item(
          func.name.text(),
          func.id,
          DefKind::Function,
          *func.name.span(),
          item.visibility,
        ) {
          if matches!(item.visibility, Visibility::Public(_)) {
            self.resolver().export_inline_value(
              module_def_id,
              func.name.text(),
              def_id,
            );
          }
        }
      }
      ItemKind::Struct(s) => {
        if let Some(def_id) = self.define_type_item(
          s.name.text(),
          s.id,
          DefKind::Struct,
          *s.name.span(),
          item.visibility,
        ) {
          if matches!(item.visibility, Visibility::Public(_)) {
            self.resolver().export_inline_type(
              module_def_id,
              s.name.text(),
              def_id,
            );
          }
        }
      }
      ItemKind::Enum(e) => {
        if let Some(def_id) = self.define_type_item(
          e.name.text(),
          e.id,
          DefKind::Enum,
          *e.name.span(),
          item.visibility,
        ) {
          if matches!(item.visibility, Visibility::Public(_)) {
            self.resolver().export_inline_type(
              module_def_id,
              e.name.text(),
              def_id,
            );
          }
        }
      }
      ItemKind::Const(c) => {
        if let Some(def_id) = self.define_value_item(
          c.name.text(),
          c.id,
          DefKind::Const,
          *c.name.span(),
          item.visibility,
        ) {
          if matches!(item.visibility, Visibility::Public(_)) {
            self.resolver().export_inline_value(
              module_def_id,
              c.name.text(),
              def_id,
            );
          }
        }
      }
      ItemKind::Mod(m) => {
        self.define_mod(m, item.visibility);
        if matches!(item.visibility, Visibility::Public(_)) {
          if let Some(nested_def_id) =
            self.module_resolver.lookup_value(&m.name.text())
          {
            self.resolver().export_inline_value(
              module_def_id,
              m.name.text(),
              nested_def_id,
            );
            self.resolver().export_inline_type(
              module_def_id,
              m.name.text(),
              nested_def_id,
            );
          }
        }
      }
    }
  }

  fn resolve_module(&mut self, node: &ProgramNode) {
    if !self.module_resolver.restore_module_rib() {
      self.module_resolver.enter_scope(ScopeKind::Module, None);
      for item in &node.items {
        self.collect_item(item);
      }
    }

    for import in &node.imports {
      self.resolve_import(&import);
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
    let mod_def = self.module_resolver.lookup_value(&m.name.text());
    self.module_resolver.enter_scope(ScopeKind::Module, mod_def);

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

  fn resolve_path(&mut self, path: &Path, expr: &Expr) {
    if path.segments.is_empty() {
      return;
    }

    for segment in &path.segments {
      for arg in &segment.args {
        self.resolve_type(arg);
      }
    }

    if let Some(root) = &path.root {
      self.ctx.dcx().emit(InvalidPathRoot {
        root: root.to_string(),
        span: expr.span,
      });
      return;
    }

    if path.segments.len() == 1 {
      let name = path.segments[0].identifier.text();

      if let Some(def_id) = self.module_resolver.lookup_value(&name) {
        self.resolver().symbols.record_resolution(expr.id, def_id);
      } else {
        self.ctx.dcx().emit(UndefinedName {
          name,
          span: expr.span,
        });
      }
      return;
    }

    let first = &path.segments[0];
    let first_name = first.identifier.text();

    let Some(mut current_def) = self
      .module_resolver
      .lookup_value(&first_name)
      .or_else(|| self.module_resolver.lookup_type(&first_name))
    else {
      self.ctx.dcx().emit(UndefinedName {
        name: first_name,
        span: *first.identifier.span(),
      });
      return;
    };

    for segment in &path.segments[1..] {
      let seg_name = segment.identifier.text();
      let Some(def) = self.resolver().get_definition(current_def) else {
        return;
      };

      let module_name = path
        .segments
        .iter()
        .take_while(|s| !std::ptr::eq(*s, segment))
        .map(|s| s.identifier.text())
        .collect::<Vec<_>>()
        .join("::");

      match def.kind {
        DefKind::Module => {
          if let Some(def_id) = self
            .resolver()
            .lookup_inline_module_value(current_def, &seg_name)
            .or_else(|| {
              self
                .resolver()
                .lookup_inline_module_type(current_def, &seg_name)
            })
          {
            current_def = def_id;
          } else if let Some(def_id) = self
            .resolver()
            .lookup_module_value(def.source_file, &seg_name)
            .or_else(|| {
              self
                .resolver()
                .lookup_module_type(def.source_file, &seg_name)
            })
          {
            current_def = def_id;
          } else {
            self.ctx.dcx().emit(UndefinedNameInModule {
              name: seg_name,
              module_name,
              span: *segment.identifier.span(),
            });
            return;
          }
        }
        DefKind::Enum | DefKind::Struct => {
          break;
        }
        _ => {
          break;
        }
      }
    }

    self
      .resolver()
      .symbols
      .record_resolution(expr.id, current_def);
  }

  fn resolve_expr(&mut self, expr: &Expr) {
    match &expr.kind {
      ExprKind::Path(path) => self.resolve_path(path, expr),

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

  fn resolve_type_path(&mut self, type_path: &Path) {
    // single segment
    if type_path.root.is_none() && type_path.segments.len() == 1 {
      let name = type_path.segments[0].identifier.text();
      for arg in &type_path.segments[0].args {
        self.resolve_type(arg);
      }
      if let Some(def_id) = self.module_resolver.lookup_type(&name) {
        self
          .resolver()
          .symbols
          .record_resolution(type_path.id, def_id);
      } else {
        self.ctx.dcx().emit(UndefinedType {
          name,
          span: type_path.span,
        });
      }
    }
    // TODO: multi segment paths

    todo!()
  }
}
