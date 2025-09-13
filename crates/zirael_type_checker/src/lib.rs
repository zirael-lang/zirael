mod ctx;
//mod enums;
mod errors;
mod expressions;
mod generic_inference;
mod method_utils;
//mod structs;
mod substitution;
mod symbol_table;
mod type_operations;

use crate::ctx::TypeInferenceContext;
use crate::symbol_table::{
  GenericFunction, GenericStruct, GenericStructField, MonoSymbolTable, TyId,
};
use id_arena::Arena;
use zirael_parser::*;
use zirael_utils::prelude::*;

impl_ast_walker!(TypeInference, {
    ctx: TypeInferenceContext,
    mono_arena: Arena<()>,
    current_struct_generics: HashMap<Identifier, TyId>,
    current_item: Option<SymbolId>,
    sym_table: MonoSymbolTable
});

impl<'reports> TypeInference<'reports> {
  pub fn eq(&self, left: &Type, right: &Type) -> bool {
    self.types_equal(left, right)
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

      (Type::Variable { id: id_a, .. }, Type::Variable { id: id_b, .. }) => id_a == id_b,

      (Type::Inferred, Type::Inferred) | (Type::Error, _) | (_, Type::Error) => true,

      (Type::MonomorphizedSymbol(sym), Type::MonomorphizedSymbol(sym2)) => {
        let Some(mono1) = self.sym_table.get_monomorphized_symbol(*sym) else { return false };
        let Some(mono2) = self.sym_table.get_monomorphized_symbol(*sym2) else { return false };

        if mono1.original_symbol_id() != mono2.original_symbol_id() {
          return false;
        }

        let concrete_types1 = mono1.concrete_types();
        let concrete_types2 = mono2.concrete_types();

        for (name, value) in concrete_types1 {
          let Some(mono2_value) = concrete_types2.get(name) else { return false };
          let value_ty = &self.sym_table[*value];
          let mono2_value_ty = &self.sym_table[*mono2_value];

          if value_ty != mono2_value_ty {
            return false;
          }
        }

        true
      }

      _ => false,
    }
  }
}

impl<'reports> AstWalker<'reports> for TypeInference<'reports> {
  fn walk_expr(&mut self, expr: &mut Expr) {
    self.infer_expr(expr);
    self.visit_type(&mut expr.ty);
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

    let method_generic_type_vars = self.create_generic_mapping(&func.signature.generics);
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

    let body_ty = if let Some(body) = &mut func.body {
      self.ctx.set_function_return_type(func.signature.return_type.clone());

      let body_ty = self.infer_expr_with_expected(body, Some(&func.signature.return_type));

      self.ctx.clear_function_return_type();

      self.substitute_type_with_map(&mut func.signature.return_type, &all_generic_type_vars);

      if !self.eq(&body_ty, &func.signature.return_type) {
        self.return_type_mismatch(&func.signature.return_type, &body_ty, func.span.clone());
      }

      body_ty
    } else {
      Type::Void
    };

    let sym_id = self.current_item.unwrap();
    let generic_function = GenericFunction::new(sym_id, func.name, func.signature.clone())
      .with_generics(func.signature.generics.clone())
      .with_body_type(self.sym_table.intern_type(body_ty));
    self.sym_table.add_generic_function(sym_id, generic_function);

    self.ctx.clear_generics();
    self.pop_scope();
  }

  fn walk_struct_declaration(&mut self, _struct: &mut StructDeclaration) {
    self.push_scope(ScopeType::Struct(_struct.id));

    let struct_generic_type_vars = self.create_generic_mapping(&_struct.generics);

    self.current_struct_generics = struct_generic_type_vars.clone();

    let mut fields = vec![];
    for field in &mut _struct.fields {
      if !struct_generic_type_vars.is_empty() {
        self.substitute_type_with_map(&mut field.ty, &struct_generic_type_vars);
      }
      self.walk_struct_field(field);

      fields.push(GenericStructField {
        name: field.name,
        ty: self.sym_table.intern_type(field.ty.clone()),
      });
    }

    for item in &mut _struct.methods {
      self.walk_item(item);
    }

    let sym_id = self.current_item.unwrap();
    let generic_struct = GenericStruct::new(sym_id, _struct.name)
      .with_generics(_struct.generics.clone())
      .with_fields(fields);

    self.sym_table.add_generic_struct(sym_id, generic_struct);

    self.current_struct_generics.clear();
    self.ctx.clear_generics();
    self.pop_scope();
  }

  fn walk_enum_declaration(&mut self, _enum: &mut EnumDeclaration) {
    self.push_scope(ScopeType::Enum(_enum.id));
    let enum_generic_type_vars = self.create_generic_mapping(&_enum.generics);
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
    self.ctx.clear_generics();
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
    self.visit_type(&mut field.ty);
    self.visit_struct_field(field);
    self.walk_type(&mut field.ty);

    for attr in field.attributes.iter_mut() {
      self.walk_attribute(attr);
    }
  }

  fn visit_type(&mut self, ty: &mut Type) {
    self.try_to_symbol(ty);
  }
}
