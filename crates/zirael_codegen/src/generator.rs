use crate::codegen::Codegen;
use anyhow::Result;
use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
use zirael_hir::hir::expr::{FieldSymbol, HirExpr, HirExprKind, HirMatchArm, HirPattern, HirStmt};
use zirael_hir::hir::{
  HirEnum, HirFunction, HirItem, HirItemKind, HirModule, HirParam, HirStruct, HirTypeExtension,
};
use zirael_parser::ty::{Ty, TyId};
use zirael_parser::{
  BinaryOp, EnumVariantData, Literal, OriginalSymbolId, SymbolId, SymbolTable, Type,
};
use zirael_type_checker::MonoSymbolTable;
use zirael_utils::prelude::CompilationInfo;
use zirael_utils::sources::Sources;

#[derive(Debug)]
pub struct CodeGenerator<'a> {
  pub hir_modules: Vec<HirModule>,
  pub used_externals: HashSet<String>,
  pub mono_symbol_table: &'a MonoSymbolTable,
  pub symbol_table: &'a SymbolTable,
  pub sources: &'a Sources,
  pub order: Vec<OriginalSymbolId>,
  pub compilation_info: &'a CompilationInfo,
  pub generated_symbols: HashSet<String>,
  pub main_function: Option<SymbolId>,
}

impl<'a> CodeGenerator<'a> {
  pub fn new(
    hir: Vec<HirModule>,
    used_externals: HashSet<String>,
    mono_symbol_table: &'a MonoSymbolTable,
    symbol_table: &'a SymbolTable,
    sources: &'a Sources,
    order: Vec<OriginalSymbolId>,
    compilation_info: &'a CompilationInfo,
    main_function: Option<SymbolId>,
  ) -> Self {
    Self {
      hir_modules: hir,
      used_externals,
      mono_symbol_table,
      symbol_table,
      sources,
      order,
      compilation_info,
      generated_symbols: HashSet::new(),
      main_function,
    }
  }

  pub fn generate_c_code(&mut self) -> Result<PathBuf> {
    fs_err::create_dir_all(&self.compilation_info.write_to)?;

    let header_path =
      self.compilation_info.write_to.join(format!("{}.h", self.compilation_info.name));
    let source_path =
      self.compilation_info.write_to.join(format!("{}.c", self.compilation_info.name));

    let mut header_gen = Codegen::new();
    let mut source_gen = Codegen::new();

    self.generate_header(&mut header_gen);
    self.generate_source(&mut source_gen, &header_path);

    fs_err::write(&header_path, header_gen.to_string())?;
    fs_err::write(&source_path, source_gen.to_string())?;

    Ok(source_path)
  }

  fn generate_header(&mut self, codegen: &mut Codegen) {
    let guard_name = format!("{}_H", self.compilation_info.name.to_uppercase());
    codegen.line(&format!("#ifndef {}", guard_name));
    codegen.line(&format!("#define {}", guard_name));
    codegen.empty_line();

    codegen.line("#include <stdint.h>");
    codegen.line("#include <stdbool.h>");
    codegen.line("#include <stdlib.h>");
    codegen.empty_line();

    self.generate_forward_declarations(codegen);
    self.generate_type_definitions(codegen);
    self.generate_function_declarations(codegen);

    codegen.empty_line();
    codegen.line(&format!("#endif // {}", guard_name));
  }

  fn generate_source(&mut self, codegen: &mut Codegen, header_path: &PathBuf) {
    let header_name = header_path.file_name().and_then(|n| n.to_str()).unwrap_or("unknown.h");
    codegen.line(&format!("#include \"{}\"", header_name));
    codegen.empty_line();

    for external in &self.used_externals {
      codegen.line(&format!("#include <{}>", external));
    }
    if !self.used_externals.is_empty() {
      codegen.empty_line();
    }

    self.generate_function_implementations(codegen);

    if let Some(main_id) = self.main_function {
      if let Some(mangled_name) = self.get_mangled_name(&OriginalSymbolId::Symbol(main_id)) {
        codegen.line("int main() {");
        codegen.indent();
        codegen.line(&format!("{}();", mangled_name));
        codegen.line("return 0;");
        codegen.dedent();
        codegen.line("}");
      }
    }
  }

  fn generate_forward_declarations(&mut self, codegen: &mut Codegen) {
    for module in &self.hir_modules {
      for item in &module.items {
        if let Some(mangled_name) = self.get_mangled_name(&item.original_item_id) {
          if self.generated_symbols.insert(mangled_name.clone()) {
            match &item.kind {
              HirItemKind::Struct(_) => {
                codegen.line(&format!("typedef struct {} {};", mangled_name, mangled_name));
              }
              HirItemKind::Enum(_) => {
                codegen.line(&format!("typedef enum {} {};", mangled_name, mangled_name));
              }
              _ => {}
            }
          }
        }
      }
    }
    codegen.empty_line();
  }

  fn generate_type_definitions(&mut self, codegen: &mut Codegen) {
    let modules = std::mem::take(&mut self.hir_modules);
    for module in &modules {
      for item in &module.items {
        match &item.kind {
          HirItemKind::Struct(struct_def) => {
            self.generate_struct_definition(codegen, struct_def, &item.original_item_id);
          }
          HirItemKind::Enum(enum_def) => {
            self.generate_enum_definition(codegen, enum_def, &item.original_item_id);
          }
          _ => {}
        }
      }
    }
    self.hir_modules = modules;
  }

  fn generate_function_declarations(&mut self, codegen: &mut Codegen) {
    let modules = std::mem::take(&mut self.hir_modules);
    for module in &modules {
      for item in &module.items {
        self.process_item_for_function_declarations(codegen, item);
      }
    }
    self.hir_modules = modules;

    codegen.empty_line();
  }

  fn process_item_for_function_declarations(&mut self, codegen: &mut Codegen, item: &HirItem) {
    match &item.kind {
      HirItemKind::Function(func) => {
        self.generate_function_declaration(codegen, func, &item.original_item_id);
      }
      HirItemKind::Struct(struct_def) => {
        self.generate_method_declarations(codegen, &struct_def.methods);
      }
      HirItemKind::Enum(enum_def) => {
        self.generate_method_declarations(codegen, &enum_def.methods);
      }
      HirItemKind::TypeExtension(ext) => {
        self.generate_method_declarations(codegen, &ext.methods);
      }
    }
  }

  fn generate_method_declarations(&mut self, codegen: &mut Codegen, methods: &[HirItem]) {
    for method in methods {
      if let HirItemKind::Function(func) = &method.kind {
        self.generate_function_declaration(codegen, func, &method.original_item_id);
      }
    }
  }

  fn generate_function_implementations(&mut self, codegen: &mut Codegen) {
    let modules = std::mem::take(&mut self.hir_modules);
    for module in &modules {
      for item in &module.items {
        self.process_item_for_function_implementations(codegen, item);
      }
    }
    self.hir_modules = modules;
  }

  fn process_item_for_function_implementations(&mut self, codegen: &mut Codegen, item: &HirItem) {
    match &item.kind {
      HirItemKind::Function(func) => {
        if !func.is_extern {
          self.generate_function_implementation(codegen, func, &item.original_item_id);
        }
      }
      HirItemKind::Struct(struct_def) => {
        self.generate_method_implementations(codegen, &struct_def.methods);
      }
      HirItemKind::Enum(enum_def) => {
        self.generate_method_implementations(codegen, &enum_def.methods);
      }
      HirItemKind::TypeExtension(ext) => {
        self.generate_method_implementations(codegen, &ext.methods);
      }
    }
  }

  fn generate_method_implementations(&mut self, codegen: &mut Codegen, methods: &[HirItem]) {
    for method in methods {
      if let HirItemKind::Function(func) = &method.kind {
        if !func.is_extern {
          self.generate_function_implementation(codegen, func, &method.original_item_id);
        }
      }
    }
  }

  fn generate_struct_definition(
    &mut self,
    codegen: &mut Codegen,
    struct_def: &HirStruct,
    original_id: &OriginalSymbolId,
  ) {
    if let Some(mangled_name) = self.get_mangled_name(original_id) {
      codegen.line(&format!("struct {} {{", mangled_name));
      codegen.indent();

      for field in &struct_def.fields {
        codegen.line(&format!("{} {};", "TODOOOOOOOOOOOOOOOOO", field.name));
      }

      codegen.dedent();
      codegen.line("};");
      codegen.empty_line();
    }
  }

  fn generate_enum_definition(
    &mut self,
    codegen: &mut Codegen,
    enum_def: &HirEnum,
    original_id: &OriginalSymbolId,
  ) {
    if let Some(mangled_name) = self.get_mangled_name(original_id) {
      codegen.line(&format!("enum {} {{", mangled_name));
      codegen.indent();

      for (i, variant) in enum_def.variants.iter().enumerate() {}

      codegen.dedent();
      codegen.line("};");
      codegen.empty_line();
    }
  }

  fn generate_function_declaration(
    &mut self,
    codegen: &mut Codegen,
    func: &HirFunction,
    original_id: &OriginalSymbolId,
  ) {
    if let Some(mangled_name) = self.get_mangled_name(original_id) {
      if !self.generated_symbols.insert(mangled_name.clone()) {
        return;
      }

      let return_type = self.resolve_type_id(&func.signature.return_type);
      let params = self.build_parameter_list(&func.signature.parameters);

      codegen.line(&format!("{} {}({});", return_type, mangled_name, params));
    }
  }

  pub fn build_parameter_list(&mut self, parameters: &[HirParam]) -> String {
    if parameters.is_empty() {
      return "void".to_string();
    }

    let mut params = Vec::new();
    for param in parameters {
      if param.is_variadic {
        params.push("...".to_string());
      } else {
        let param_type = self.resolve_type_id(&param.ty);
        let param_name = self
          .get_mangled_name(&OriginalSymbolId::Symbol(param.symbol_id))
          .unwrap_or_else(|| "param".to_string());
        params.push(format!("{} {}", param_type, param_name));
      }
    }

    params.join(", ")
  }

  pub fn needs_return_statement(&self, return_type: &TyId) -> bool {
    !matches!(self.mono_symbol_table.resolve(*return_type), Ty::Void | Ty::Never)
  }

  pub fn get_mangled_name(&self, original_id: &OriginalSymbolId) -> Option<String> {
    let mangled_name = self.mono_symbol_table.mangled_names.get(original_id);
    mangled_name.map(|m| {
      if m.module_path.is_empty() {
        m.base.clone()
      } else {
        format!("{}_{}", m.module_path, m.base)
      }
    })
  }

  pub fn resolve_type_id(&self, ty_id: &TyId) -> String {
    let ty = self.mono_symbol_table.resolve(*ty_id);
    self.generate_ty(&ty)
  }

  fn generate_ty(&self, ty: &Ty) -> String {
    match ty {
      Ty::Void => "void".to_string(),
      Ty::Bool => "bool".to_string(),
      Ty::Int => "int64_t".to_string(),
      Ty::Uint => "uint64_t".to_string(),
      Ty::Float => "double".to_string(),
      Ty::Char => "char".to_string(),
      Ty::String => "char*".to_string(),
      Ty::Never => "void".to_string(),

      Ty::Pointer(inner) => {
        let inner_type = self.generate_ty(inner);
        format!("{}*", inner_type)
      }

      Ty::Reference(inner) => {
        let inner_type = self.generate_ty(inner);
        format!("{}*", inner_type)
      }

      Ty::Array(inner) => {
        let inner_type = self.generate_ty(inner);
        format!("{}*", inner_type)
      }

      Ty::Function(params, return_type) => {
        let return_c_type = self.generate_ty(return_type);
        let param_types: Vec<String> = params.iter().map(|p| self.generate_ty(p)).collect();
        let params_str =
          if param_types.is_empty() { "void".to_string() } else { param_types.join(", ") };
        format!("{}(*)({})", return_c_type, params_str)
      }

      Ty::Symbol(id) => {
        if let Some(mangled) = self.get_mangled_name(id) {
          mangled
        } else {
          format!("/* UnknownSymbol {:?} */ void*", id)
        }
      }

      _ => "void*".to_string(),
    }
  }
}
