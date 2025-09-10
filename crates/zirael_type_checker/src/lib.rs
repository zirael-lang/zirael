mod ctx;
mod enums;
mod errors;
mod expressions;
pub mod monomorphization;
mod structs;
mod substitution;
pub mod unification;

use crate::{ctx::TypeInferenceContext, monomorphization::MonomorphizationTable};
use id_arena::Arena;
use zirael_parser::{ast::monomorphized_symbol::MonomorphizedSymbol, *};
use zirael_utils::prelude::*;

impl_ast_walker!(TypeInference, {
    ctx: TypeInferenceContext,
    mono_table: MonomorphizationTable,
    mono_arena: Arena<()>,
    current_struct_generics: HashMap<Identifier, Type>,
    current_item: Option<SymbolId>,
});

impl<'reports> TypeInference<'reports> {
  pub fn try_monomorphize_named_type(&mut self, ty: &mut Type) {
    match ty {
      Type::Named { name, generics } => {
        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
          if let SymbolKind::Struct { generics: generic_params, .. }
          | SymbolKind::Enum { generics: generic_params, .. } = &symbol.kind
          {
            let all_concrete =
              generics.iter().all(|g| !matches!(g, Type::Variable { .. } | Type::Inferred));
            if all_concrete && !generic_params.is_empty() {
              let mut generic_map = HashMap::new();
              for (g, ty) in generic_params.iter().zip(generics.iter()) {
                generic_map.insert(g.name, ty.clone());
              }
              let monomorphized_id =
                self.record_monomorphization_with_id(symbol.id, &generic_map, None);

              *ty = Type::MonomorphizedSymbol(MonomorphizedSymbol {
                id: monomorphized_id,
                display_ty: Box::new(ty.clone()),
              });
            }
          }
        }
      }
      Type::Reference(inner) => {
        self.try_monomorphize_named_type(inner);
      }
      _ => {}
    }
  }

  pub fn eq(&self, left: &Type, right: &Type) -> bool {
    self.structural_eq(left, right)
  }

  fn structural_eq(&self, left: &Type, right: &Type) -> bool {
    match (left, right) {
      (Type::String, Type::String)
      | (Type::Char, Type::Char)
      | (Type::Int, Type::Int)
      | (Type::Float, Type::Float)
      | (Type::Bool, Type::Bool)
      | (Type::Void, Type::Void)
      | (Type::Never, Type::Never) => true,

      (Type::Never, _) | (_, Type::Never) => true,

      (Type::Pointer(a), Type::Pointer(b)) | (Type::Reference(a), Type::Reference(b)) => {
        self.structural_eq(a, b)
      }

      (Type::Array(a_ty, a_len), Type::Array(b_ty, b_len)) => {
        self.structural_eq(a_ty, b_ty) && a_len == b_len
      }

      (
        Type::Function { params: a_params, return_type: a_ret },
        Type::Function { params: b_params, return_type: b_ret },
      ) => {
        a_params.len() == b_params.len()
          && a_params.iter().zip(b_params).all(|(a, b)| self.structural_eq(a, b))
          && self.structural_eq(a_ret, b_ret)
      }

      (
        Type::Named { name: a_name, generics: a_generics },
        Type::Named { name: b_name, generics: b_generics },
      ) => {
        a_name == b_name
          && a_generics.len() == b_generics.len()
          && a_generics.iter().zip(b_generics).all(|(a, b)| self.structural_eq(a, b))
      }

      (Type::Variable { name: a_name, .. }, Type::Variable { name: b_name, .. }) => {
        a_name == b_name
      }

      (Type::Inferred, Type::Inferred) | (Type::Error, _) | (_, Type::Error) => true,

      (Type::MonomorphizedSymbol(sym), Type::Named { .. }) => {
        self.structural_eq(&sym.display_ty, right)
      }

      (Type::Named { .. }, Type::MonomorphizedSymbol(sym)) => {
        self.structural_eq(left, &sym.display_ty)
      }

      (Type::MonomorphizedSymbol(sym), Type::MonomorphizedSymbol(sym2)) => {
        if let (Some(mono1), Some(mono2)) =
          (self.mono_table.get_entry(sym.id), self.mono_table.get_entry(sym2.id))
        {
          if mono1.original_id != mono2.original_id {
            return false;
          }

          let concrete_types1 = mono1.concrete_types.clone();
          let concrete_types2 = mono2.concrete_types.clone();

          for (name, value) in &concrete_types1 {
            let Some(mono2_value) = concrete_types2.get(name) else { return false };
            if !self.eq(value, mono2_value) {
              return false;
            }
          }

          true
        } else {
          false
        }
      }

      _ => false,
    }
  }

  fn get_self_type_for_method(&mut self, func: &Function) -> Option<Type> {
    if let Some(func_symbol) = self.symbol_table.lookup_symbol(&func.name) {
      if let Some(struct_symbol_id) = self.symbol_table.is_a_child_of_symbol(func_symbol.id) {
        let struct_symbol = self.symbol_table.get_symbol_unchecked(&struct_symbol_id);

        return match &struct_symbol.kind {
          SymbolKind::Struct { generics, .. } | SymbolKind::Enum { generics, .. } => {
            let generic_names = generics
              .iter()
              .map(|g| Type::Named { name: g.name.clone(), generics: vec![] })
              .collect::<Vec<_>>();

            Some(Type::Named { name: struct_symbol.name, generics: generic_names })
          }
          SymbolKind::TypeExtension { ty, .. } => {
            if ty.is_primitive() {
              Some(ty.clone())
            } else {
              self.error(
                "right now type extensions can only be used on primitive types",
                vec![("type extension here".to_string(), func.span.clone())],
                vec![],
              );
              None
            }
          }
          _ => {
            warn!("expected struct symbol, got {:?}", struct_symbol.kind);
            None
          }
        };
      }
    }
    None
  }

  fn get_generic_type_vars(
    &mut self,
    generics: &Vec<GenericParameter>,
  ) -> HashMap<Identifier, Type> {
    if !generics.is_empty() {
      generics
        .iter()
        .map(|g| (g.name.clone(), self.ctx.fresh_type_var(Some(g.name.clone()))))
        .collect::<HashMap<_, _>>()
    } else {
      HashMap::new()
    }
  }

  fn get_generics_for_method(&mut self, func: &Function) -> Option<HashMap<Identifier, Type>> {
    if let Some(func_symbol) = self.symbol_table.lookup_symbol(&func.name) {
      if let Some(struct_symbol_id) = self.symbol_table.is_a_child_of_symbol(func_symbol.id) {
        let struct_symbol = self.symbol_table.get_symbol_unchecked(&struct_symbol_id);

        if let SymbolKind::Struct { generics, .. } | SymbolKind::Enum { generics, .. } =
          &struct_symbol.kind
        {
          if !generics.is_empty() {
            return Some(
              generics
                .iter()
                .map(|g| (g.name.clone(), self.ctx.fresh_type_var(Some(g.name.clone()))))
                .collect(),
            );
          }
        } else {
          warn!("expected struct symbol, got {:?}", struct_symbol.kind);
        }
      }
    }
    None
  }

  fn merge_generic_maps(
    &self,
    struct_generics: &HashMap<Identifier, Type>,
    method_generics: &HashMap<Identifier, Type>,
  ) -> HashMap<Identifier, Type> {
    let mut merged = struct_generics.clone();
    merged.extend(method_generics.clone());
    merged
  }

  fn resolve_self_parameter_type_with_generics(
    &mut self,
    param: &Parameter,
    struct_type: &mut Option<Type>,
    struct_generics: &HashMap<Identifier, Type>,
  ) -> Type {
    match struct_type {
      Some(struct_ty) => {
        self.substitute_type_with_map(struct_ty, struct_generics);

        match &param.ty {
          Type::Reference(_inner) => Type::Reference(Box::new(struct_ty.clone())),
          _ => struct_ty.clone(),
        }
      }
      None => param.ty.clone(),
    }
  }
}

impl<'reports> AstWalker<'reports> for TypeInference<'reports> {
  fn walk_expr(&mut self, expr: &mut Expr) {
    self.infer_expr(expr);
    self.try_monomorphize_named_type(&mut expr.ty);
  }

  fn visit_var_decl(&mut self, _var_decl: &mut VarDecl) {
    self.infer_variable(_var_decl);
  }

  fn visit_item(&mut self, _item: &mut Item) {
    if let Some(id) = _item.symbol_id {
      debug!("Setting current_item to {:?} for item: {:?}", id, _item.name);
      self.current_item = Some(id);
    } else {
      debug!("Item has no symbol_id: {:?}", _item.name);
    }
  }

  fn walk_function(&mut self, func: &mut Function) {
    self.push_scope(ScopeType::Function(func.id));

    self.walk_function_modifiers(&mut func.modifiers);
    self.walk_function_signature(&mut func.signature);

    let method_generic_type_vars = self.get_generic_type_vars(&func.signature.generics);

    let struct_generic_type_vars = self.get_generics_for_method(func).unwrap_or_else(HashMap::new);

    let all_generic_type_vars =
      self.merge_generic_maps(&struct_generic_type_vars, &method_generic_type_vars);

    let struct_type = &mut self.get_self_type_for_method(func);

    if !func.signature.is_static()
      && let Some(param) = func.signature.parameters.get_mut(0)
    {
      param.ty = self.resolve_self_parameter_type_with_generics(
        &param,
        struct_type,
        &struct_generic_type_vars,
      );
    };

    for param in &mut func.signature.parameters {
      if let Some(param_id) = param.symbol_id {
        self.substitute_type_with_map(&mut param.ty, &all_generic_type_vars);
        self.ctx.add_variable(param_id, param.ty.clone());
      }
    }

    if let Some(body) = &mut func.body {
      self.ctx.set_function_return_type(func.signature.return_type.clone());

      let body_ty = self.infer_expr_with_expected(body, Some(&func.signature.return_type));

      self.ctx.clear_function_return_type();

      self.substitute_type_with_map(&mut func.signature.return_type, &all_generic_type_vars);

      if !self.eq(&body_ty, &func.signature.return_type) {
        self.return_type_mismatch(&func.signature.return_type, &body_ty, func.span.clone());
      }
    }

    self.pop_scope();
  }

  fn walk_struct_declaration(&mut self, _struct: &mut StructDeclaration) {
    self.push_scope(ScopeType::Struct(_struct.id));

    let struct_generic_type_vars = self.get_generic_type_vars(&_struct.generics);

    self.current_struct_generics = struct_generic_type_vars.clone();

    for field in &mut _struct.fields {
      if !struct_generic_type_vars.is_empty() {
        self.substitute_type_with_map(&mut field.ty, &struct_generic_type_vars);
      }
      self.walk_struct_field(field);
    }

    for item in &mut _struct.methods {
      self.walk_item(item);
    }

    self.current_struct_generics.clear();
    self.pop_scope();
  }

  fn walk_enum_declaration(&mut self, _enum: &mut EnumDeclaration) {
    self.push_scope(ScopeType::Enum(_enum.id));
    let enum_generic_type_vars = self.get_generic_type_vars(&_enum.generics);
    self.current_struct_generics = enum_generic_type_vars.clone();

    for variant in &mut _enum.variants {
      if let EnumVariantData::Struct(ref mut fields) = variant.data {
        for field in fields {
          if !enum_generic_type_vars.is_empty() {
            self.substitute_type_with_map(&mut field.ty, &enum_generic_type_vars);
          }
          self.walk_struct_field(field);
        }
      }
    }

    for item in &mut _enum.methods {
      self.walk_item(item);
    }

    self.current_struct_generics.clear();
    self.pop_scope();
  }

  fn walk_type_extension(&mut self, _ty_ext: &mut TypeExtension) {
    self.push_scope(ScopeType::TypeExtension(_ty_ext.id));

    for item in &mut _ty_ext.items {
      self.walk_item(item);
    }

    self.pop_scope();
  }

  fn walk_struct_field(&mut self, field: &mut StructField) {
    self.try_monomorphize_named_type(&mut field.ty);

    self.visit_struct_field(field);
    self.walk_type(&mut field.ty);

    for attr in &mut field.attributes {
      self.walk_attribute(attr);
    }
  }

  fn visit_type(&mut self, ty: &mut Type) {
    match ty {
      Type::Named { name, .. } => {
        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
          if let Some(item) = self.current_item {
            debug!("Adding relation: {:?} -> {:?} (name: {})", item, symbol.id, resolve(name));
            self.symbol_table.new_relation(
              SymbolRelationNode::Symbol(item),
              SymbolRelationNode::Symbol(symbol.canonical_symbol),
            )
          } else {
            debug!("No current_item set when visiting type: {}", resolve(name));
          }
        } else {
          if !self.ctx.is_generic_parameter(*name) {
            let report = ReportBuilder::builder(
              &format!("couldn't find struct or enum named {}", resolve(name).dimmed().bold()),
              ReportKind::Error,
            )
            .label("not found", name.span().clone());

            self.reports.add(self.processed_file.unwrap(), report);
          }
        }
      }
      _ => {}
    }
  }
}
