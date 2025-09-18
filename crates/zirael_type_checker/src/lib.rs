mod ctx;
//mod enums;
mod errors;
mod expressions;
mod generic_inference;
mod method_utils;
//mod structs;
mod eq;
mod substitution;
mod symbol_table;
mod type_operations;

use crate::ctx::TypeInferenceContext;
pub use crate::symbol_table::{
  GenericFunction, GenericStruct, GenericStructField, MonoSymbolTable, TyId,
};
use id_arena::Arena;
use zirael_parser::*;
use zirael_utils::prelude::*;

pub use symbol_table::*;

impl_ast_walker!(TypeInference, {
    ctx: TypeInferenceContext,
    mono_arena: Arena<()>,
    current_struct_generics: HashMap<Identifier, TyId>,
    current_item: Option<SymbolId>,
    sym_table: MonoSymbolTable
});

impl<'reports> AstWalker<'reports> for TypeInference<'reports> {
  fn walk_expr(&mut self, expr: &mut Expr) {
    self.infer_expr(expr);
    self.visit_type(&mut expr.ty);
  }

  fn visit_var_decl(&mut self, var_decl: &mut VarDecl) {
    let ty = self.sym_table.intern_type(var_decl.ty.clone());
    let sym = self.symbol_table.get_symbol_unchecked(&var_decl.symbol_id.unwrap());
    self.sym_table.add_generic_value(
      var_decl.name,
      var_decl.symbol_id.unwrap(),
      ty,
      sym.is_used,
      var_decl.span,
    );

    self.infer_variable(var_decl);
  }

  fn visit_parameter(&mut self, param: &mut Parameter) {
    let ty = self.sym_table.intern_type(param.ty.clone());
    let sym = self.symbol_table.get_symbol_unchecked(&param.symbol_id.unwrap());
    self.sym_table.add_generic_value(
      param.name,
      param.symbol_id.unwrap(),
      ty,
      sym.is_used,
      param.span,
    );
  }

  fn visit_item(&mut self, _item: &mut Item) {
    if let Some(id) = _item.symbol_id {
      debug!("Setting current_item to {:?} for item: {:?}", id, _item.name);
      self.current_item = Some(id);
    } else {
      debug!("Item has no symbol_id: {:?}", _item.name);
    }
  }

  fn walk_function(&mut self, func: &mut Function, sym_id: SymbolId) {
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

    for param in &mut func.signature.parameters {
      self.visit_parameter(param);
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
    let sym = self.symbol_table.get_symbol_unchecked(&sym_id);
    self.sym_table.add_generic_function(sym_id, generic_function, sym.is_used, func.span);

    self.ctx.clear_generics();
    self.pop_scope();
  }

  fn walk_struct_declaration(&mut self, _struct: &mut StructDeclaration, sym_id: SymbolId) {
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

    let generic_struct = GenericStruct::new(sym_id, _struct.name)
      .with_generics(_struct.generics.clone())
      .with_fields(fields);

    let sym = self.symbol_table.get_symbol_unchecked(&sym_id);
    self.sym_table.add_generic_struct(sym_id, generic_struct, sym.is_used, _struct.span);

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
