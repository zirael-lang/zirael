use crate::{TypeInference, monomorphization::MonomorphizationData};
use std::{collections::HashMap, vec};
use zirael_parser::{
  AstWalker, CallInfo, EnumVariantData, Expr, ExprKind, GenericParameter, Path, StructField,
  SymbolId, SymbolKind, Type, ast::monomorphized_symbol::MonomorphizedSymbol,
};
use zirael_utils::ident_table::{Identifier, resolve};

#[derive(Debug, Clone)]
struct StructInfo {
  fields: Vec<StructField>,
  generics: Vec<GenericParameter>,
  name: Identifier,
  parent_enum_id: Option<SymbolId>,
}

impl<'reports> TypeInference<'reports> {
  pub fn infer_struct_init(
    &mut self,
    name_expr: &Expr,
    fields: &mut HashMap<Identifier, Expr>,
    call_info: &mut Option<CallInfo>,
  ) -> Type {
    let struct_sym_id = match self.resolve_struct_symbol(name_expr) {
      Ok(id) => id,
      Err(ty) => return ty,
    };

    let struct_info = match self.get_struct_info(struct_sym_id, name_expr) {
      Ok(info) => info,
      Err(ty) => return ty,
    };

    self.validate_required_fields(&struct_info.fields, fields, name_expr);

    self.setup_generics(&struct_info.generics);

    let (field_types, generic_mapping) =
      self.infer_field_types(fields, &struct_info.fields, &struct_info.generics);

    self.create_final_type(
      struct_info,
      field_types,
      generic_mapping,
      fields,
      struct_sym_id,
      call_info,
    )
  }

  fn resolve_struct_symbol(&mut self, name_expr: &Expr) -> Result<SymbolId, Type> {
    match &name_expr.kind {
      ExprKind::Identifier(_, Some(sym_id)) => Ok(*sym_id),
      ExprKind::Path(path) => self.resolve_path_symbol(path, name_expr),
      _ => {
        self.error(
          "expected identifier or path in struct initialization",
          vec![("here".to_string(), name_expr.span.clone())],
          vec![],
        );
        Err(Type::Error)
      }
    }
  }

  fn resolve_path_symbol(&mut self, path: &Path, name_expr: &Expr) -> Result<SymbolId, Type> {
    if let Some(last_segment) = path.segments.last() {
      if let Some(sym_id) = last_segment.symbol_id {
        Ok(sym_id)
      } else {
        self.error(
          "unresolved path in struct initialization",
          vec![("here".to_string(), name_expr.span.clone())],
          vec![],
        );
        Err(Type::Error)
      }
    } else {
      self.error(
        "empty path in struct initialization",
        vec![("here".to_string(), name_expr.span.clone())],
        vec![],
      );
      Err(Type::Error)
    }
  }

  fn get_struct_info(
    &mut self,
    struct_sym_id: SymbolId,
    name_expr: &Expr,
  ) -> Result<StructInfo, Type> {
    let symbol = self.symbol_table.get_symbol_unchecked(&struct_sym_id);

    match &symbol.kind {
      SymbolKind::Struct { fields, generics, .. } => Ok(StructInfo {
        fields: fields.clone(),
        generics: generics.clone(),
        name: symbol.name,
        parent_enum_id: None,
      }),
      SymbolKind::EnumVariant { parent_enum, data } => {
        self.handle_enum_variant(*parent_enum, data, symbol.name, name_expr)
      }
      _ => {
        self.error(
          "expected struct or enum variant for initialization",
          vec![("here".to_string(), name_expr.span.clone())],
          vec![],
        );
        Err(Type::Error)
      }
    }
  }

  fn handle_enum_variant(
    &mut self,
    parent_enum: SymbolId,
    data: &EnumVariantData,
    variant_name: Identifier,
    name_expr: &Expr,
  ) -> Result<StructInfo, Type> {
    let parent_symbol = self.symbol_table.get_symbol_unchecked(&parent_enum);

    if let SymbolKind::Enum { generics, .. } = &parent_symbol.kind {
      if let EnumVariantData::Struct(variant_fields) = data {
        Ok(StructInfo {
          fields: variant_fields.clone(),
          generics: generics.clone(),
          name: variant_name,
          parent_enum_id: Some(parent_enum),
        })
      } else {
        self.error(
          "cannot initialize non-struct enum variant with struct syntax",
          vec![("here".to_string(), name_expr.span.clone())],
          vec![],
        );
        Err(Type::Error)
      }
    } else {
      self.error(
        "invalid parent enum for variant",
        vec![("here".to_string(), name_expr.span.clone())],
        vec![],
      );
      Err(Type::Error)
    }
  }

  fn validate_required_fields(
    &mut self,
    struct_fields: &[StructField],
    fields: &HashMap<Identifier, Expr>,
    name_expr: &Expr,
  ) {
    for field in struct_fields.iter() {
      if !fields.contains_key(&field.name) {
        let field_name = resolve(&field.name);
        self.error(
          &format!("missing field `{}`", field_name),
          vec![(format!("missing field `{}`", field_name), name_expr.span.clone())],
          vec![],
        );
      }
    }
  }

  fn setup_generics(&mut self, generics: &[GenericParameter]) {
    if !generics.is_empty() {
      for generic in generics.iter() {
        if !self.ctx.is_generic_parameter(generic.name) {
          let _type_var = self.ctx.fresh_type_var(Some(generic.name));
        }
      }
    }
  }

  fn infer_field_types(
    &mut self,
    fields: &mut HashMap<Identifier, Expr>,
    struct_fields: &[StructField],
    generics: &[GenericParameter],
  ) -> (Vec<(Identifier, Type)>, HashMap<Identifier, Type>) {
    let mut field_types = Vec::new();
    let mut generic_mapping = HashMap::new();

    for (field_name, field_expr) in fields.iter_mut() {
      if let Some(field) = struct_fields.iter().find(|f| &f.name == field_name) {
        let expr_type = self.infer_expr(field_expr);

        field_types.push((*field_name, expr_type.clone()));

        if !generics.is_empty() {
          self.infer_generic_types(&field.ty, &expr_type, &mut generic_mapping);
        }

        if generics.is_empty() && !self.eq(&field.ty, &expr_type) {
          self.type_mismatch(&field.ty, &expr_type, field_expr.span.clone());
        }
      } else {
        let field_name_str = resolve(field_name);
        self.error(
          &format!("unknown field `{}`", field_name_str),
          vec![(format!("unknown field `{}`", field_name_str), field_expr.span.clone())],
          vec![],
        );
      }
    }

    for (_, field_type) in field_types.iter_mut() {
      self.try_monomorphize_named_type(field_type);
    }

    (field_types, generic_mapping)
  }

  fn create_final_type(
    &mut self,
    struct_info: StructInfo,
    field_types: Vec<(Identifier, Type)>,
    generic_mapping: HashMap<Identifier, Type>,
    fields: &HashMap<Identifier, Expr>,
    struct_sym_id: SymbolId,
    call_info: &mut Option<CallInfo>,
  ) -> Type {
    if struct_info.generics.is_empty() {
      self.create_non_generic_type(struct_info)
    } else {
      self.create_generic_type(
        struct_info,
        field_types,
        generic_mapping,
        fields,
        struct_sym_id,
        call_info,
      )
    }
  }

  fn create_non_generic_type(&self, struct_info: StructInfo) -> Type {
    let name = if let Some(parent_enum_id) = struct_info.parent_enum_id {
      let parent_symbol = self.symbol_table.get_symbol_unchecked(&parent_enum_id);
      parent_symbol.name
    } else {
      struct_info.name
    };

    Type::Named { name, generics: vec![] }
  }

  fn create_generic_type(
    &mut self,
    struct_info: StructInfo,
    field_types: Vec<(Identifier, Type)>,
    generic_mapping: HashMap<Identifier, Type>,
    fields: &HashMap<Identifier, Expr>,
    struct_sym_id: SymbolId,
    call_info: &mut Option<CallInfo>,
  ) -> Type {
    let concrete_generics = self.resolve_concrete_generics(&struct_info.generics, &generic_mapping);

    if self.all_generics_concrete(&concrete_generics) {
      self.create_monomorphized_type(
        struct_info,
        field_types,
        concrete_generics,
        fields,
        struct_sym_id,
        call_info,
      )
    } else {
      self.create_partial_generic_type(struct_info, concrete_generics)
    }
  }

  fn resolve_concrete_generics(
    &self,
    generics: &[GenericParameter],
    generic_mapping: &HashMap<Identifier, Type>,
  ) -> Vec<Type> {
    generics
      .iter()
      .map(|g| {
        generic_mapping.get(&g.name).cloned().expect("generic mapping should contain all generics")
      })
      .collect()
  }

  fn all_generics_concrete(&self, concrete_generics: &[Type]) -> bool {
    concrete_generics.iter().all(|ty| !matches!(ty, Type::Variable { .. }))
  }

  fn create_monomorphized_type(
    &mut self,
    struct_info: StructInfo,
    field_types: Vec<(Identifier, Type)>,
    concrete_generics: Vec<Type>,
    fields: &HashMap<Identifier, Expr>,
    struct_sym_id: SymbolId,
    call_info: &mut Option<CallInfo>,
  ) -> Type {
    let monomorphized_fields = self.create_monomorphized_fields(&struct_info, &concrete_generics);

    self.validate_monomorphized_field_types(&field_types, &monomorphized_fields, fields);

    let generic_map = self.create_generic_map(&struct_info.generics, &concrete_generics);

    let monomorphized_id = self.record_monomorphization_with_id(
      struct_sym_id,
      &generic_map,
      Some(MonomorphizationData::Fields(monomorphized_fields)),
    );

    *call_info = Some(CallInfo {
      original_symbol: struct_sym_id,
      monomorphized_id: Some(monomorphized_id),
      concrete_types: generic_map,
    });

    let display_type = self.create_display_type(&struct_info, concrete_generics);

    Type::MonomorphizedSymbol(MonomorphizedSymbol {
      id: monomorphized_id,
      display_ty: Box::new(display_type),
    })
  }

  fn create_monomorphized_fields(
    &mut self,
    struct_info: &StructInfo,
    concrete_generics: &[Type],
  ) -> Vec<StructField> {
    let mut monomorphized_fields = struct_info.fields.clone();

    for field in monomorphized_fields.iter_mut() {
      self.substitute_generic_params(&mut field.ty, &struct_info.generics, concrete_generics);
    }

    monomorphized_fields
  }

  fn validate_monomorphized_field_types(
    &mut self,
    field_types: &[(Identifier, Type)],
    monomorphized_fields: &[StructField],
    fields: &HashMap<Identifier, Expr>,
  ) {
    let mut type_mismatches = Vec::new();

    for (field_name, expr_type) in field_types {
      if let Some(field) = monomorphized_fields.iter().find(|f| &f.name == field_name) {
        if !self.eq(&field.ty, expr_type) {
          if let Some(field_expr) = fields.get(field_name) {
            type_mismatches.push((field.ty.clone(), expr_type.clone(), field_expr.span.clone()));
          }
        }
      }
    }

    for (expected_ty, found_ty, span) in type_mismatches {
      self.type_mismatch(&expected_ty, &found_ty, span);
    }
  }

  fn create_generic_map(
    &self,
    generics: &[GenericParameter],
    concrete_generics: &[Type],
  ) -> HashMap<Identifier, Type> {
    let mut generic_map = HashMap::new();
    for (g, ty) in generics.iter().zip(concrete_generics.iter()) {
      generic_map.insert(g.name, ty.clone());
    }
    generic_map
  }

  fn create_display_type(&self, struct_info: &StructInfo, concrete_generics: Vec<Type>) -> Type {
    let name = if let Some(parent_enum_id) = struct_info.parent_enum_id {
      let parent_symbol = self.symbol_table.get_symbol_unchecked(&parent_enum_id);
      parent_symbol.name
    } else {
      struct_info.name
    };

    Type::Named { name, generics: concrete_generics }
  }

  fn create_partial_generic_type(
    &self,
    struct_info: StructInfo,
    concrete_generics: Vec<Type>,
  ) -> Type {
    let name = if let Some(parent_enum_id) = struct_info.parent_enum_id {
      let parent_symbol = self.symbol_table.get_symbol_unchecked(&parent_enum_id);
      parent_symbol.name
    } else {
      struct_info.name
    };

    Type::Named { name, generics: concrete_generics }
  }
}
