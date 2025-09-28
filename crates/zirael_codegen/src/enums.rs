use crate::codegen::Codegen;
use crate::generator::CodeGenerator;
use zirael_hir::hir::{HirEnum, HirEnumVariantData, HirStructField, HirVariant};
use zirael_parser::{OriginalSymbolId, SymbolId};
use zirael_type_checker::{
  GenericEnumData, GenericStructField, GenericSymbol, GenericSymbolKind, MonomorphizedStructField,
  MonomorphizedSymbolKind,
};
use zirael_utils::ident_table::resolve;
use zirael_utils::prelude::Identifier;

impl<'a> CodeGenerator<'a> {
  pub fn generate_enum_definition(
    &mut self,
    codegen: &mut Codegen,
    enum_def: &HirEnum,
    original_id: &OriginalSymbolId,
  ) {
    let Some(mangled_name) = self.get_mangled_name(original_id) else { return };

    self.generate_enum_tags(codegen, &mangled_name, enum_def);
    self.generate_enum_struct(codegen, &mangled_name, enum_def);
  }

  fn generate_enum_tags(&mut self, codegen: &mut Codegen, mangled_name: &str, enum_def: &HirEnum) {
    codegen.line("typedef enum {");
    codegen.indent();

    for variant in &enum_def.variants {
      codegen.line(&format!(
        "{}_TAG,",
        self.get_mangled_name(&variant.symbol_id).unwrap_or("ERROR".to_string())
      ));
    }

    codegen.dedent();
    codegen.line(&format!("}} {}_tags;", mangled_name));
    codegen.line("");
  }

  fn generate_enum_struct(
    &mut self,
    codegen: &mut Codegen,
    mangled_name: &str,
    enum_def: &HirEnum,
  ) {
    codegen.line("typedef struct {");
    codegen.indent();
    codegen.line(&format!("{}_tags tag;", mangled_name));
    codegen.line("union {");
    codegen.indent();

    for variant in &enum_def.variants {
      self.generate_variant_union_member(codegen, variant);
    }

    codegen.dedent();
    codegen.line("} data;");
    codegen.dedent();
    codegen.line(&format!("}} {};", mangled_name));
    codegen.line("");
  }

  fn generate_variant_union_member(&mut self, codegen: &mut Codegen, variant_sym: &HirVariant) {
    let name = self.get_mangled_name(&variant_sym.symbol_id).unwrap_or("ERROR".to_string());
    match &variant_sym.data {
      HirEnumVariantData::Struct(fields) => {
        codegen.line("struct {");
        codegen.indent();
        for field in fields {
          let field_type = self.resolve_type_id(&field.ty);
          codegen.line(&format!("{} {};", field_type, field.name));
        }
        codegen.dedent();
        codegen.line(&format!("}} {};", name));
      }
      HirEnumVariantData::Unit => {
        codegen.line(&format!("struct {{}} {};", name));
      }
    }
  }

  pub fn generate_enum_variant_implementations(
    &mut self,
    codegen: &mut Codegen,
    enum_def: &HirEnum,
    original_id: &OriginalSymbolId,
  ) {
    let Some(mangled_name) = self.get_mangled_name(original_id) else { return };

    for variant in &enum_def.variants {
      self.generate_variant_implementations(codegen, &mangled_name, variant);
    }
  }

  fn generate_variant_implementations(
    &mut self,
    codegen: &mut Codegen,
    enum_mangled_name: &str,
    variant: &HirVariant,
  ) {
    let variant_name = self.get_mangled_name(&variant.symbol_id).unwrap_or("ERROR".to_string());

    match &variant.data {
      HirEnumVariantData::Unit => {
        self.write_unit_variant_implementation(codegen, enum_mangled_name, &variant_name);
      }
      HirEnumVariantData::Struct(fields) => {
        self.write_struct_variant_implementation(codegen, enum_mangled_name, &variant_name, fields);
      }
    }
  }

  fn write_variant_implementation(
    &mut self,
    codegen: &mut Codegen,
    enum_name: &str,
    variant_name: &str,
    fields: &[MonomorphizedStructField],
  ) {
    if fields.is_empty() {
      self.write_unit_variant_implementation(codegen, enum_name, variant_name);
    } else {
      let params: Vec<String> = fields
        .iter()
        .map(|field| {
          let field_type = self.resolve_type_id(&field.concrete_ty);
          format!("{} {}", field_type, field.name)
        })
        .collect();

      codegen.line(&format!("{} {}({}) {{", enum_name, variant_name, params.join(", ")));
      codegen.indent();
      codegen.line(&format!("{} result;", enum_name));
      codegen.line(&format!("result.tag = {}_TAG;", variant_name));

      for field in fields {
        codegen.line(&format!("result.data.{}.{} = {};", variant_name, field.name, field.name));
      }

      codegen.line("return result;");
      codegen.dedent();
      codegen.line("}");
      codegen.line("");
    }
  }

  fn write_unit_variant_implementation(
    &mut self,
    codegen: &mut Codegen,
    enum_name: &str,
    variant_name: &str,
  ) {
    codegen.line(&format!("{} {}(void) {{", enum_name, variant_name));
    codegen.indent();
    codegen.line(&format!("{} result;", enum_name));
    codegen.line(&format!("result.tag = {}_TAG;", variant_name));
    codegen.line("return result;");
    codegen.dedent();
    codegen.line("}");
    codegen.line("");
  }

  fn write_struct_variant_implementation(
    &mut self,
    codegen: &mut Codegen,
    enum_name: &str,
    variant_name: &str,
    fields: &Vec<HirStructField>,
  ) {
    let params: Vec<String> = fields
      .iter()
      .enumerate()
      .map(|(i, field)| {
        let field_type = self.resolve_type_id(&field.ty);
        format!("{} field_{}", field_type, i)
      })
      .collect();

    let param_list = if params.is_empty() { "void".to_string() } else { params.join(", ") };

    codegen.line(&format!("{} {}({}) {{", enum_name, variant_name, param_list));
    codegen.indent();
    codegen.line(&format!("{} result;", enum_name));
    codegen.line(&format!("result.tag = {}_TAG;", variant_name));

    for (i, field) in fields.iter().enumerate() {
      codegen.line(&format!("result.data.{}.{} = field_{};", variant_name, field.name, i));
    }

    codegen.line("return result;");
    codegen.dedent();
    codegen.line("}");
    codegen.line("");
  }
}
