use crate::TypeInference;
use crate::symbol_table::{
  GenericEnumData, GenericStructField, GenericSymbol, MonomorphizedStructField,
  MonomorphizedSymbol, MonomorphizedSymbolBase, MonomorphizedSymbolKind, TyId,
};
use std::collections::HashMap;
use zirael_parser::{
  CallInfo, EnumDeclaration, EnumVariantData, Expr, GenericParameter, MonomorphizationId, SymbolId,
  SymbolKind, Type,
};
use zirael_utils::{
  ident_table::Identifier,
  prelude::{Span, resolve},
};

impl<'reports> TypeInference<'reports> {
  pub fn infer_enum_variant_init(
    &mut self,
    variant_id: &SymbolId,
    name_span: &Span,
    fields: &mut HashMap<Identifier, Expr>,
    call_info: &mut Option<CallInfo>,
  ) -> Type {
    let (variant_fields, enum_generics, enum_name, enum_sym_id) =
      match self.extract_enum_variant_info(*variant_id) {
        Some(info) => info,
        None => return Type::Error,
      };

    if !self.validate_enum_variant_fields(&variant_fields, fields, name_span) {
      return Type::Error;
    }

    self.infer_field_expressions(fields);

    let generic_mapping =
      match self.resolve_enum_generics(&enum_generics, &variant_fields, fields, name_span) {
        Ok(mapping) => mapping,
        Err(()) => return Type::Error,
      };

    let concrete_generics = self.create_concrete_generics(&enum_generics, &generic_mapping);

    if !self.validate_enum_field_types(&variant_fields, fields, &enum_generics, &concrete_generics)
    {
      return Type::Error;
    }

    println!("{:?}", generic_mapping);
    let monomorphized_id = if !enum_generics.is_empty() && !generic_mapping.is_empty() {
      Some(self.create_monomorphized_enum_variant(
        *variant_id,
        enum_sym_id,
        enum_name,
        &variant_fields,
        &enum_generics,
        &concrete_generics,
        &generic_mapping,
      ))
    } else {
      None
    };

    *call_info = Some(CallInfo {
      original_symbol: *variant_id,
      monomorphized_id,
      concrete_types: generic_mapping,
    });
    
    if let Some(mono_id) = monomorphized_id {
      Type::MonomorphizedSymbol(mono_id)
    } else {
      Type::Named { name: enum_name, generics: concrete_generics }
    }
  }

  fn extract_enum_variant_info(
    &mut self,
    variant_id: SymbolId,
  ) -> Option<(Vec<GenericStructField>, Vec<GenericParameter>, Identifier, SymbolId)> {
    let variant_symbol = self.symbol_table.get_symbol(variant_id).ok()?;

    if let SymbolKind::EnumVariant { parent_enum, data, .. } = &variant_symbol.kind {
      let enum_symbol = self.symbol_table.get_symbol(*parent_enum).ok()?;

      if let SymbolKind::Enum { generics, .. } = &enum_symbol.kind {
        let variant_fields = match data {
          EnumVariantData::Unit => vec![],
          EnumVariantData::Struct(fields) => fields
            .iter()
            .map(|f| {
              let ty = match &f.ty {
                Type::Named { name, generics: type_generics } if type_generics.is_empty() => {
                  if generics.iter().any(|g| g.name == *name) {
                    self.ctx.fresh_type_var(Some(*name))
                  } else {
                    f.ty.clone()
                  }
                }
                _ => f.ty.clone(),
              };
              GenericStructField { name: f.name, ty: self.sym_table.intern_type(ty) }
            })
            .collect(),
        };
        return Some((variant_fields, generics.clone(), enum_symbol.name, *parent_enum));
      }
    }

    None
  }

  fn validate_enum_variant_fields(
    &mut self,
    variant_fields: &[GenericStructField],
    fields: &HashMap<Identifier, Expr>,
    name_span: &Span,
  ) -> bool {
    let mut valid = true;

    if variant_fields.is_empty() && !fields.is_empty() {
      self.error(
        "unit variant cannot have fields",
        vec![("unit variant".to_string(), name_span.clone())],
        vec![],
      );
      return false;
    }

    for field in variant_fields {
      if !fields.contains_key(&field.name) {
        self.error(
          &format!("missing field `{}`", resolve(&field.name)),
          vec![(format!("missing field `{}`", resolve(&field.name)), name_span.clone())],
          vec![],
        );
        valid = false;
      }
    }

    for (field_name, field_expr) in fields {
      if !variant_fields.iter().any(|f| &f.name == field_name) {
        self.error(
          &format!("unknown field `{}`", resolve(field_name)),
          vec![(format!("unknown field `{}`", resolve(field_name)), field_expr.span.clone())],
          vec![],
        );
        valid = false;
      }
    }

    valid
  }

  fn infer_field_expressions(&mut self, fields: &mut HashMap<Identifier, Expr>) {
    for (_, field_expr) in fields.iter_mut() {
      self.infer_expr(field_expr);
    }
  }

  fn resolve_enum_generics(
    &mut self,
    enum_generics: &[GenericParameter],
    variant_fields: &[GenericStructField],
    fields: &HashMap<Identifier, Expr>,
    _name_span: &Span,
  ) -> Result<HashMap<Identifier, TyId>, ()> {
    if enum_generics.is_empty() {
      return Ok(HashMap::new());
    }

    for generic in enum_generics {
      if !self.ctx.is_generic_parameter(generic.name) {
        let _ = self.ctx.fresh_type_var(Some(generic.name));
      }
    }

    let mut generic_mapping = HashMap::new();

    for (field_name, field_expr) in fields {
      if let Some(field_def) = variant_fields.iter().find(|f| &f.name == field_name) {
        let field_type = Type::Id(field_def.ty);
        self.infer_generic_types(&field_type, &field_expr.ty, &mut generic_mapping);
      }
    }

    Ok(generic_mapping)
  }

  fn create_concrete_generics(
    &self,
    enum_generics: &[GenericParameter],
    generic_mapping: &HashMap<Identifier, TyId>,
  ) -> Vec<Type> {
    enum_generics
      .iter()
      .map(|g| generic_mapping.get(&g.name).map(|id| Type::Id(*id)).unwrap_or(Type::Inferred))
      .collect()
  }

  fn validate_enum_field_types(
    &mut self,
    variant_fields: &[GenericStructField],
    fields: &HashMap<Identifier, Expr>,
    enum_generics: &[GenericParameter],
    _concrete_generics: &[Type],
  ) -> bool {
    if enum_generics.is_empty() {
      for (field_name, field_expr) in fields {
        if let Some(field_def) = variant_fields.iter().find(|f| &f.name == field_name) {
          let field_type = Type::Id(field_def.ty);
          if !self.eq(&field_type, &field_expr.ty) {
            self.type_mismatch(&field_type, &field_expr.ty, field_expr.span.clone());
            return false;
          }
        }
      }
    }
    true
  }

  fn create_monomorphized_enum_variant(
    &mut self,
    variant_id: SymbolId,
    enum_sym_id: SymbolId,
    enum_name: Identifier,
    variant_fields: &[GenericStructField],
    _enum_generics: &[GenericParameter],
    _concrete_generics: &[Type],
    generic_mapping: &HashMap<Identifier, TyId>,
  ) -> MonomorphizationId {
    let mono_fields: Vec<MonomorphizedStructField> = variant_fields
      .iter()
      .map(|field| MonomorphizedStructField {
        name: field.name,
        concrete_ty: field.ty,
        is_public: true,
      })
      .collect();

    let mono_symbol = MonomorphizedSymbol {
      base: MonomorphizedSymbolBase {
        original_symbol_id: variant_id,
        name: enum_name,
        concrete_types: generic_mapping.clone(),
      },
      kind: MonomorphizedSymbolKind::EnumVariant {
        mono_id: self.sym_table.mono_ids.alloc(()),
        parent_enum: enum_sym_id,
        fields: mono_fields,
      },
    };

    self.sym_table.add_monomorphized_symbol(mono_symbol)
  }

  pub fn add_enum_variants(&mut self, _enum: &mut EnumDeclaration, enum_sym_id: SymbolId) {
    for (_, variant) in _enum.variants.iter().enumerate() {
      let mut has_generics = false;

      let data = match &variant.data {
        EnumVariantData::Unit => GenericEnumData::Unit,
        EnumVariantData::Struct(fields) => {
          let mut converted = vec![];
          for f in fields {
            if self.has_generics(&f.ty) {
              has_generics = true;
            }

            let ty = self.sym_table.intern_type(f.ty.clone());
            converted.push(GenericStructField { name: f.name, ty });
          }

          GenericEnumData::Struct(converted)
        }
      };

      let variant_symbol_id = variant.symbol_id.unwrap();
      let variant_sym =
        self.symbol_table.get_symbol(variant_symbol_id).expect("variant symbol must exist");
      let variant_generic = GenericSymbol::enum_variant(
        variant_symbol_id,
        variant.name,
        enum_sym_id,
        data,
        variant_sym.is_used,
        variant.span,
        has_generics,
      );

      let generic_id = self.sym_table.generic_symbols.alloc(variant_generic);
      self.sym_table.symbol_to_generic.insert(variant_symbol_id, generic_id);
      self.sym_table.generic_to_symbol.insert(generic_id, variant_symbol_id);
    }
  }

  pub fn substitute_enum_variants(
    &mut self,
    _enum: &mut EnumDeclaration,
    enum_generic_type_vars: &HashMap<Identifier, TyId>,
  ) {
    for variant in &mut _enum.variants {
      if let EnumVariantData::Struct(ref mut fields) = variant.data {
        for field in fields {
          if !enum_generic_type_vars.is_empty() {
            self.substitute_type_with_map(&mut field.ty, &enum_generic_type_vars);
          }
        }
      }
    }
  }
}
