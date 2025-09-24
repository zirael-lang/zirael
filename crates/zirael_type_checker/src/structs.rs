use crate::symbol_table::TyId;
use crate::{MonomorphizedStructField, MonomorphizedSymbol, TypeInference};
use std::collections::HashMap;
use zirael_parser::{
  AstWalker, CallInfo, EnumVariantData, Expr, ExprKind, FunctionSignature, GenericParameter, Path,
  StructField, SymbolId, SymbolKind, Type,
};
use zirael_utils::ident_table::{Identifier, resolve};
use zirael_utils::prelude::Span;

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
  type_annotations: &mut Vec<Type>,
  _expected_type: Option<&Type>,
  ) -> Type {
    let struct_sym_id = match self.resolve_struct_symbol(name_expr) {
      Some(id) => id,
      None => return Type::Error,
    };

    let sym = match self.symbol_table().get_symbol(struct_sym_id) {
      Ok(symbol) => symbol,
      Err(_) => return Type::Error,
    };

    if let SymbolKind::EnumVariant { parent_enum: _, data } = &sym.kind {
      match data {
        EnumVariantData::Unit => {
          if !fields.is_empty() {
            self.error(
              "unit variant cannot have fields",
              vec![("unit variant".to_string(), name_expr.span.clone())],
              vec![],
            );
            return Type::Error;
          }
          return Type::Symbol(struct_sym_id);
        }
        EnumVariantData::Struct(_) => {
          return self.infer_enum_variant_init(&struct_sym_id, &name_expr.span, fields, call_info);
        }
      }
    }

    let struct_info = match self.extract_struct_info(&sym.kind, struct_sym_id, name_expr) {
      Some(info) => info,
      None => return Type::Error,
    };

    if !self.validate_required_fields(&struct_info.fields, fields, name_expr) {
      return Type::Error;
    }

    if !self.validate_type_annotations(type_annotations, &struct_info.generics, &name_expr.span) {
      return Type::Error;
    }
    self.infer_struct_types(fields);

    let generic_mapping =
      match self.resolve_struct_generics(&struct_info, fields, type_annotations, &name_expr.span) {
        Ok(mapping) => mapping,
        Err(()) => return Type::Error,
      };
    println!("Generic mapping: {:?}", generic_mapping);

    // let concrete_fields = self.create_concrete_fields(&struct_info.fields, &generic_mapping);
    //
    // println!("{:#?}", concrete_fields);
    // if !self.validate_field_types(fields, &concrete_fields) {
    //   return Type::Error;
    // }

    let monomorphized_id = if !struct_info.generics.is_empty() && !generic_mapping.is_empty() {
      // let mono_fields =
      //   self.create_monomorphized_fields(fields, &concrete_fields, &generic_mapping);
      //
      // Some(self.sym_table.add_monomorphized_symbol(MonomorphizedSymbol::struct_def(
      //   struct_sym_id,
      //   struct_info.name,
      //   "TODO".to_string(),
      //   mono_fields,
      //   generic_mapping.clone(),
      //   None,
      // )))
      None
    } else {
      None
    };

    *call_info = Some(CallInfo {
      original_symbol: struct_sym_id,
      monomorphized_id,
      concrete_types: generic_mapping.clone(),
    });

    Type::Int
    // self.create_struct_type(&struct_info, &generic_mapping)
  }

  fn infer_struct_types(&mut self, fields: &mut HashMap<Identifier, Expr>) {
    for (_, field) in fields.iter_mut() {
      self.infer_expr(field);
      self.visit_type(&mut field.ty);
    }
  }

  fn resolve_struct_generics(
    &mut self,
    struct_info: &StructInfo,
    fields: &HashMap<Identifier, Expr>,
    type_annotations: &Vec<Type>,
    call_span: &Span,
  ) -> Result<HashMap<Identifier, TyId>, ()> {
    let mut generic_mapping: HashMap<Identifier, TyId> = HashMap::new();

    if !type_annotations.is_empty() {
      generic_mapping =
        self.create_mapping_from_annotations(&struct_info.generics, type_annotations);
    } else if !struct_info.generics.is_empty() {
      let original_generics = self.ctx.generic_params.clone();
      for param in &struct_info.generics {
        self.ctx.generic_params.insert(param.name, self.ctx.next_type_var_id);
        self.ctx.next_type_var_id += 1;
      }

      let expected_types: Vec<Type> = struct_info.fields.iter().map(|f| f.ty.clone()).collect();
      let actual_types: Vec<Type> = struct_info
        .fields
        .iter()
        .filter_map(|f| fields.get(&f.name).map(|expr| expr.ty.clone()))
        .collect();

      generic_mapping = self.infer_generic_mappings(&struct_info.generics, &expected_types, &actual_types);

      self.ctx.generic_params = original_generics;
    }

    let unresolved = self.get_unresolved_generics(&struct_info.generics, &generic_mapping);
    if !unresolved.is_empty() {
      let struct_name = resolve(&struct_info.name);

      self.report_unresolved_generics(
        &unresolved,
        &struct_name,
        call_span,
        !type_annotations.is_empty(),
      );
      return Err(());
    }

    Ok(generic_mapping)
  }

  fn resolve_struct_symbol(&mut self, name_expr: &Expr) -> Option<SymbolId> {
    match &name_expr.kind {
      ExprKind::Identifier(_, Some(sym_id)) => Some(*sym_id),
      ExprKind::Path(path) => path.segments.last().and_then(|seg| seg.symbol_id),
      _ => {
        self.error(
          "expected identifier or path in struct initialization",
          vec![("here".to_string(), name_expr.span.clone())],
          vec![],
        );
        None
      }
    }
  }

  fn extract_struct_info(
    &mut self,
    symbol_kind: &SymbolKind,
    struct_sym_id: SymbolId,
    name_expr: &Expr,
  ) -> Option<StructInfo> {
  let symbol = self.symbol_table().get_symbol(struct_sym_id).expect("symbol must exist");

    match symbol_kind {
      SymbolKind::Struct { fields, generics, .. } => Some(StructInfo {
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
        None
      }
    }
  }

  fn handle_enum_variant(
    &mut self,
    parent_enum: SymbolId,
    data: &EnumVariantData,
    variant_name: Identifier,
    name_expr: &Expr,
  ) -> Option<StructInfo> {
  let parent_symbol = self.symbol_table().get_symbol(parent_enum).expect("enum must exist");

    match &parent_symbol.kind {
      SymbolKind::Enum { generics, .. } => match data {
        EnumVariantData::Struct(variant_fields) => Some(StructInfo {
          fields: variant_fields.clone(),
          generics: generics.clone(),
          name: variant_name,
          parent_enum_id: Some(parent_enum),
        }),
        _ => {
          self.error(
            "cannot initialize non-struct enum variant with struct syntax",
            vec![("here".to_string(), name_expr.span.clone())],
            vec![],
          );
          None
        }
      },
      _ => {
        self.error(
          "invalid parent enum for variant",
          vec![("here".to_string(), name_expr.span.clone())],
          vec![],
        );
        None
      }
    }
  }

  fn validate_required_fields(
    &mut self,
    struct_fields: &[StructField],
    fields: &HashMap<Identifier, Expr>,
    name_expr: &Expr,
  ) -> bool {
    let mut valid = true;

    for field in struct_fields {
      if !fields.contains_key(&field.name) {
        let field_name = resolve(&field.name);
        self.error(
          &format!("missing field `{}`", field_name),
          vec![(format!("missing field `{}`", field_name), name_expr.span.clone())],
          vec![],
        );
        valid = false;
      }
    }

    valid
  }
}
