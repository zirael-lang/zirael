use crate::prelude::{WalkerContext, warn};
use zirael_parser::{
  AstWalker, CallInfo, Expr, ExprKind, MatchArm, Path, PathSegment, Pattern, ScopeType, Symbol,
  SymbolId, SymbolKind, SymbolRelationNode, SymbolTable, Type, impl_ast_walker, item::Item,
};
use zirael_utils::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
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
      (Self::Function, SymbolKind::Function { .. }) => true,
      (Self::Variable, SymbolKind::Variable { .. }) => true,
      (Self::Constant, SymbolKind::Constant { .. }) => true,
      (Self::Struct, SymbolKind::Struct { .. }) => true,
      (Self::Enum, SymbolKind::Enum { .. }) => true,
      (Self::Parameter, SymbolKind::Parameter { .. }) => true,
      (Self::Type, SymbolKind::Struct { .. } | SymbolKind::Enum { .. }) => true,
      (
        Self::Value,
        SymbolKind::Variable { .. }
        | SymbolKind::Constant { .. }
        | SymbolKind::Parameter { .. }
        | SymbolKind::MatchBinding { .. }
        | SymbolKind::EnumVariant { .. },
      ) => true,
      (Self::Any, _) => true,
      _ => false,
    }
  }

  fn name(&self) -> &'static str {
    match self {
      Self::Function => "function",
      Self::Variable => "variable",
      Self::Constant => "constant",
      Self::Struct => "struct",
      Self::Enum => "enum",
      Self::Parameter => "parameter",
      Self::Type => "struct or enum",
      Self::Value => "parameter, constant, or variable",
      Self::Any => "symbol",
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
      self.report_unknown_symbol(ident, span, &expected);
      None
    }
  }

  fn report_unknown_symbol(&self, ident: &Identifier, span: Span, expected: &ExpectedSymbol) {
    let mut report = ReportBuilder::builder(
      format!("couldn't find {} named {}", expected.name(), resolve(ident).dimmed().bold()),
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
      warn!("report outside of a file: {report:#?}");
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

  fn resolve_path(&mut self, path: &mut Path, expected: ExpectedSymbol) -> Option<SymbolId> {
    if path.segments.is_empty() {
      return None;
    }

    let final_id = self.resolve_path_segments(path)?;
    self.validate_expected_symbol(final_id, expected, path.span.clone())
  }

  fn resolve_path_for_construction(&mut self, path: &mut Path) -> Option<SymbolId> {
    if path.segments.is_empty() {
      return None;
    }

    if let Some(id) = self.try_resolve_as_type(path) {
      return Some(id);
    }

    self.try_resolve_enum_variant_path(path)
  }

  fn resolve_path_segments(&mut self, path: &mut Path) -> Option<SymbolId> {
    let first_segment = &mut path.segments[0];
    let first_id = self.resolve_identifier(
      &first_segment.identifier,
      first_segment.span.clone(),
      ExpectedSymbol::Any,
    )?;
    first_segment.symbol_id = Some(first_id);

    let mut current_id = first_id;
    for segment in &mut path.segments[1..] {
      current_id = self.resolve_segment_in_context(current_id, segment)?;
    }

    Some(current_id)
  }

  fn try_resolve_as_type(&mut self, path: &mut Path) -> Option<SymbolId> {
    let first_segment = &mut path.segments[0];
    let first_id = self.resolve_identifier(
      &first_segment.identifier,
      first_segment.span.clone(),
      ExpectedSymbol::Type,
    )?;
    first_segment.symbol_id = Some(first_id);

    if path.segments.len() == 1 {
      return Some(first_id);
    }

    let mut current_id = first_id;
    for segment in &mut path.segments[1..] {
      current_id = self.resolve_segment_in_context(current_id, segment)?;
    }

    Some(current_id)
  }

  fn resolve_segment_in_context(
    &mut self,
    context_id: SymbolId,
    segment: &mut PathSegment,
  ) -> Option<SymbolId> {
    let context_symbol = self.symbol_table.get_symbol_unchecked(&context_id);

    let found_id = match &context_symbol.kind {
      SymbolKind::Struct { methods, .. } => self.find_method_by_name(methods, &segment.identifier),
      SymbolKind::Enum { methods, variants, .. } => self
        .find_method_by_name(methods, &segment.identifier)
        .or_else(|| self.find_variant_by_name(variants, &segment.identifier)),
      _ => None,
    };

    if let Some(id) = found_id {
      segment.symbol_id = Some(id);
      Some(id)
    } else {
      self.report_unknown_symbol(&segment.identifier, segment.span.clone(), &ExpectedSymbol::Any);
      None
    }
  }

  fn find_method_by_name(&self, methods: &[SymbolId], name: &Identifier) -> Option<SymbolId> {
    methods
      .iter()
      .find(|&&method_id| {
        let method_symbol = self.symbol_table.get_symbol_unchecked(&method_id);
        method_symbol.name == *name
      })
      .copied()
  }

  fn find_variant_by_name(&self, variants: &[SymbolId], name: &Identifier) -> Option<SymbolId> {
    variants
      .iter()
      .find(|&&variant_id| {
        let variant_symbol = self.symbol_table.get_symbol_unchecked(&variant_id);
        variant_symbol.name == *name
      })
      .copied()
  }

  fn validate_expected_symbol(
    &mut self,
    symbol_id: SymbolId,
    expected: ExpectedSymbol,
    span: Span,
  ) -> Option<SymbolId> {
    let symbol = self.symbol_table.get_symbol_unchecked(&symbol_id);

    if expected.matches(&symbol.kind) {
      Some(symbol_id)
    } else {
      let found_kind = article(symbol.kind.name()).dimmed().bold();
      let expected_kind = article(expected.name()).dimmed().bold();
      self.error(
        &format!("expected {expected_kind}, found {found_kind}"),
        vec![(format!("found {found_kind} here"), span)],
        vec![],
      );
      None
    }
  }

  fn try_resolve_enum_variant_path(&mut self, path: &mut Path) -> Option<SymbolId> {
    if path.segments.len() < 2 {
      return None;
    }

    let enum_segment = &mut path.segments[0];
    let enum_id = self.resolve_identifier(
      &enum_segment.identifier,
      enum_segment.span.clone(),
      ExpectedSymbol::Enum,
    )?;
    enum_segment.symbol_id = Some(enum_id);

    let enum_symbol = self.symbol_table.get_symbol_unchecked(&enum_id);
    if let SymbolKind::Enum { variants, .. } = &enum_symbol.kind {
      let variant_segment = &mut path.segments[1];
      if let Some(variant_id) = self.find_variant_by_name(variants, &variant_segment.identifier) {
        variant_segment.symbol_id = Some(variant_id);
        return Some(variant_id);
      }
    }

    None
  }

  fn visit_static_call_base(&mut self, _callee: &mut Expr) {
    let ExprKind::FieldAccess(fields) = &mut _callee.kind else {
      self.error("expected field access in static call", vec![], vec![]);
      return;
    };

    let base = &mut fields[0];
    let span = base.span.clone();
    let Some((ident, ident_sym_id)) = base.as_identifier_mut() else {
      self.error("expected identifier in field access", vec![], vec![]);
      return;
    };

    if let Some(id) = self.resolve_identifier(ident, span, ExpectedSymbol::Type) {
      *ident_sym_id = Some(id);

      self.symbol_table.new_relation(
        SymbolRelationNode::Symbol(self.current_item.unwrap()),
        SymbolRelationNode::Symbol(id),
      );
    }
  }
}

impl<'reports> AstWalker<'reports> for NameResolution<'reports> {
  fn visit_item(&mut self, _item: &mut Item) {
    self.current_item = _item.symbol_id;
  }

  fn visit_function_call(&mut self, callee: &mut Expr, _args: &mut [Expr]) {
    match &mut callee.kind {
      ExprKind::Identifier(ident, ident_sym_id) => {
        let span = callee.span.clone();
        if let Some(id) = self.resolve_identifier(ident, span, ExpectedSymbol::Function) {
          *ident_sym_id = Some(id);

          self.symbol_table.new_relation(
            SymbolRelationNode::Symbol(self.current_item.unwrap()),
            SymbolRelationNode::Symbol(id),
          );
          self.symbol_table.mark_used(id).expect("invalid symbol id");
        }
      }
      ExprKind::Path(path) => {
        if let Some(id) = self.resolve_path(path, ExpectedSymbol::Function) {
          self.symbol_table.new_relation(
            SymbolRelationNode::Symbol(self.current_item.unwrap()),
            SymbolRelationNode::Symbol(id),
          );
          self.symbol_table.mark_used(id).expect("invalid symbol id");

          if let Some(parent) = self.symbol_table.is_a_child_of_symbol(id) {
            self.symbol_table.mark_used(parent).expect("invalid symbol id");
            self.symbol_table.new_relation(
              SymbolRelationNode::Symbol(self.current_item.unwrap()),
              SymbolRelationNode::Symbol(parent),
            );
          }
        }
      }
      _ => {
        self.error("expected identifier or path in function call", vec![], vec![]);
      }
    }
  }

  fn visit_identifier(&mut self, id: &mut Identifier, sym_id: &mut Option<SymbolId>, span: Span) {
    if let Some(id) = self.resolve_identifier(id, span, ExpectedSymbol::Value) {
      self.symbol_table.mark_used(id).expect("invalid symbol id");
      *sym_id = Some(id);
    }
  }

  fn visit_struct_init(&mut self, name: &mut Expr, _fields: &mut HashMap<Identifier, Expr>) {
    match &mut name.kind {
      ExprKind::Identifier(ident, ident_sym_id) => {
        let span = name.span.clone();
        if let Some(id) = self.resolve_identifier(ident, span, ExpectedSymbol::Struct) {
          *ident_sym_id = Some(id);

          self.symbol_table.new_relation(
            SymbolRelationNode::Symbol(self.current_item.unwrap()),
            SymbolRelationNode::Symbol(id),
          );
        }
      }
      ExprKind::Path(path) => {
        if let Some(id) = self.resolve_path_for_construction(path) {
          self.symbol_table.new_relation(
            SymbolRelationNode::Symbol(self.current_item.unwrap()),
            SymbolRelationNode::Symbol(id),
          );

          let symbol = self.symbol_table.get_symbol_unchecked(&id);
          if let SymbolKind::EnumVariant { .. } = &symbol.kind {}
        }
      }
      _ => {
        self.error("expected identifier or path in struct init", vec![], vec![]);
      }
    }
  }

  fn visit_path(&mut self, path: &mut Path) {
    self.resolve_path(path, ExpectedSymbol::Value);
  }

  fn visit_field_access(&mut self, fields: &mut Vec<Expr>) {
    let base = &mut fields[0];
    self.walk_expr(base);
  }

  fn visit_type(&mut self, _ty: &mut Type) {
    if let Type::Named { name, .. } = _ty {
      if let Some(sym) = self.symbol_table.lookup_symbol(name) {
        if let SymbolKind::Struct { .. } | SymbolKind::Enum { .. } = sym.kind {
          self.symbol_table.new_relation(
            SymbolRelationNode::Symbol(self.current_item.unwrap()),
            SymbolRelationNode::Symbol(sym.id),
          );
          self.symbol_table.mark_used(sym.id).expect("invalid symbol id");
        }
      }
    }
  }

  fn visit_method_call(
    &mut self,
    _chain: &mut Vec<Expr>,
    _args: &mut Vec<Expr>,
    _info: &mut Option<CallInfo>,
  ) {
    self.visit_field_access(_chain);

    for arg in _args {
      self.visit_expr(arg);
    }
  }

  fn visit_static_call(
    &mut self,
    _callee: &mut Expr,
    _args: &mut Vec<Expr>,
    _: &mut Option<CallInfo>,
  ) {
    self.visit_static_call_base(_callee);

    for arg in _args {
      self.walk_expr(arg);
    }
  }

  fn visit_match_arm(&mut self, _arm: &mut MatchArm) {
    match &mut _arm.pattern {
      Pattern::EnumVariant { path, .. } => {
        if let Some(id) = self.resolve_path_for_construction(path) {
          let symbol = self.symbol_table.get_symbol_unchecked(&id);
          if !matches!(symbol.kind, SymbolKind::EnumVariant { .. }) {
            self.error(
              "expected enum variant in pattern",
              vec![("not an enum variant".to_string(), path.span.clone())],
              vec![],
            );
          }
        }
      }
      _ => {}
    }
  }
}
