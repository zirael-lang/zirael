use crate::codegen::Codegen;
use crate::generator::CodeGenerator;
use zirael_hir::hir::HirEnum;
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

    let variants =
      self.mono_symbol_table.get_enum_variants_by_parent(original_id.as_symbol().unwrap());
    println!("{:?}", variants);

    self.generate_enum_tags(codegen, &mangled_name, enum_def);
    self.generate_enum_struct(codegen, &mangled_name, enum_def);
  }

  fn generate_enum_tags(&mut self, codegen: &mut Codegen, mangled_name: &str, enum_def: &HirEnum) {
    codegen.line("typedef enum {");
    codegen.indent();

    for variant in &enum_def.variants {
      if let Some(variant_sym) = self.mono_symbol_table.get_generic_symbol(variant.symbol_id) {
        codegen.line(&format!("{}_TAG_{},", mangled_name, variant_sym.name()));
      }
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
      if let Some(variant_sym) = self.mono_symbol_table.get_generic_symbol(variant.symbol_id) {
        self.generate_variant_union_member(codegen, variant_sym);
      }
    }

    codegen.dedent();
    codegen.line("} data;");
    codegen.dedent();
    codegen.line(&format!("}} {};", mangled_name));
    codegen.line("");
  }

  fn generate_variant_union_member(&mut self, codegen: &mut Codegen, variant_sym: &GenericSymbol) {
    if let GenericSymbolKind::EnumVariant { data, .. } = &variant_sym.kind {
      match data {
        GenericEnumData::Struct(fields) => {
          codegen.line("struct {");
          codegen.indent();
          for field in fields {
            let field_type = self.resolve_type_id(&field.ty);
            codegen.line(&format!("{} {};", field_type, field.name));
          }
          codegen.dedent();
          codegen.line(&format!("}} {};", variant_sym.name()));
        }
        GenericEnumData::Unit => {
          codegen.line(&format!("struct {{}} {};", variant_sym.name()));
        }
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
      self.generate_monomorphized_implementations(codegen, &mangled_name, &variant.symbol_id);
      self.generate_generic_implementation_if_needed(codegen, &mangled_name, &variant.symbol_id);
    }
  }

  fn generate_monomorphized_implementations(
    &mut self,
    codegen: &mut Codegen,
    enum_mangled_name: &str,
    variant_symbol_id: &SymbolId,
  ) {
    let Some(mono_variants) = self.mono_symbol_table.symbol_to_mono_variants.get(variant_symbol_id)
    else {
      return;
    };

    for &mono_id in mono_variants {
      let Some(mono_variant) = self.mono_symbol_table.get_monomorphized_symbol(mono_id) else {
        continue;
      };
      let Some(variant_mangled_name) =
        self.get_mangled_name(&OriginalSymbolId::Monomorphization(mono_id))
      else {
        continue;
      };
      let Some(variant_sym) = self.mono_symbol_table.get_generic_symbol(*variant_symbol_id) else {
        continue;
      };

      if let MonomorphizedSymbolKind::EnumVariant { fields, .. } = &mono_variant.kind {
        self.write_variant_implementation(
          codegen,
          enum_mangled_name,
          &variant_mangled_name,
          &resolve(variant_sym.name()),
          fields,
        );
      }
    }
  }

  fn generate_generic_implementation_if_needed(
    &mut self,
    codegen: &mut Codegen,
    enum_mangled_name: &str,
    variant_symbol_id: &SymbolId,
  ) {
    let generic_variant = self.mono_symbol_table.get_generic_symbol(*variant_symbol_id);
    let Some(GenericSymbolKind::EnumVariant { has_generics, .. }) =
      generic_variant.map(|s| &s.kind)
    else {
      return;
    };

    if *has_generics {
      return;
    }

    let Some(variant_sym) = self.mono_symbol_table.get_generic_symbol(*variant_symbol_id) else {
      return;
    };
    let variant_name = format!("{}_{}", enum_mangled_name, variant_sym.name());

    if let GenericSymbolKind::EnumVariant { data, .. } = &variant_sym.kind {
      match data {
        GenericEnumData::Unit => {
          self.write_unit_variant_implementation(
            codegen,
            enum_mangled_name,
            &variant_name,
            &resolve(variant_sym.name()),
          );
        }
        GenericEnumData::Struct(fields) => {
          self.write_struct_variant_implementation(
            codegen,
            enum_mangled_name,
            &variant_name,
            &resolve(variant_sym.name()),
            fields,
          );
        }
      }
    }
  }

  fn write_variant_implementation(
    &mut self,
    codegen: &mut Codegen,
    enum_name: &str,
    variant_name: &str,
    variant_sym_name: &str,
    fields: &[MonomorphizedStructField],
  ) {
    if fields.is_empty() {
      self.write_unit_variant_implementation(codegen, enum_name, variant_name, variant_sym_name);
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
      codegen.line(&format!("result.tag = {}_TAG_{};", enum_name, variant_sym_name));

      for field in fields {
        codegen.line(&format!("result.data.{}.{} = {};", variant_sym_name, field.name, field.name));
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
    variant_sym_name: &str,
  ) {
    codegen.line(&format!("{} {}(void) {{", enum_name, variant_name));
    codegen.indent();
    codegen.line(&format!("{} result;", enum_name));
    codegen.line(&format!("result.tag = {}_TAG_{};", enum_name, variant_sym_name));
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
    variant_sym_name: &str,
    fields: &[GenericStructField],
  ) {
    let params: Vec<String> = fields
      .iter()
      .enumerate()
      .map(|(i, field)| {
        let field_type = self.resolve_type_id(&field.ty);
        println!("{:?}", self.mono_symbol_table[field.ty]);
        format!("{} field_{}", field_type, i)
      })
      .collect();

    let param_list = if params.is_empty() { "void".to_string() } else { params.join(", ") };

    codegen.line(&format!("{} {}({}) {{", enum_name, variant_name, param_list));
    codegen.indent();
    codegen.line(&format!("{} result;", enum_name));
    codegen.line(&format!("result.tag = {}_TAG_{};", enum_name, variant_sym_name));

    for (i, field) in fields.iter().enumerate() {
      codegen.line(&format!("result.data.{}.{} = field_{};", variant_sym_name, field.name, i));
    }

    codegen.line("return result;");
    codegen.dedent();
    codegen.line("}");
    codegen.line("");
  }
}
