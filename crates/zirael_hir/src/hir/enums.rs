use crate::hir::lowering::AstLowering;
use crate::hir::{HirEnum, HirEnumVariantData, HirStructField, HirVariant};
use zirael_parser::{
  EnumDeclaration, EnumVariant, EnumVariantData, MonomorphizationId, OriginalSymbolId, SymbolId,
};
use zirael_type_checker::{GenericSymbolKind, MonomorphizedSymbolKind};
use zirael_utils::prelude::warn;

impl<'reports, 'table> AstLowering<'reports, 'table> {
  pub fn lower_enum(&mut self, enum_def: &mut EnumDeclaration, symbol_id: SymbolId) -> HirEnum {
    let variants =
      enum_def.variants.iter().flat_map(|variant| self.lower_enum_variant(variant)).collect();

    HirEnum {
      id: enum_def.id,
      symbol_id,
      methods: self.lower_methods(&mut enum_def.methods),
      variants,
    }
  }

  fn lower_enum_variant(&mut self, variant: &EnumVariant) -> Vec<HirVariant> {
    let variant_id = variant.symbol_id.unwrap();

    if self.is_non_generic_variant(variant_id) {
      return vec![HirVariant {
        symbol_id: OriginalSymbolId::Symbol(variant.symbol_id.unwrap()),
        data: self.lower_variant_data(&variant.data),
      }];
    }

    self.process_monomorphized_variants(variant_id)
  }

  fn is_non_generic_variant(&self, variant_id: SymbolId) -> bool {
    self.symbol_table
            .get_generic_symbol(variant_id)
            .map(|sym| matches!(&sym.kind, GenericSymbolKind::EnumVariant { has_generics, .. } if !has_generics))
            .unwrap_or(false)
  }

  fn process_monomorphized_variants(&mut self, variant_id: SymbolId) -> Vec<HirVariant> {
    let Some(mono_variants) = self.symbol_table.get_mono_variants(variant_id) else {
      return vec![];
    };

    mono_variants.iter().filter_map(|&mono_id| self.create_hir_variant_from_mono(mono_id)).collect()
  }

  fn create_hir_variant_from_mono(&self, mono_id: MonomorphizationId) -> Option<HirVariant> {
    let symbol = self.symbol_table.get_monomorphized_symbol(mono_id)?;

    let MonomorphizedSymbolKind::EnumVariant { fields, .. } = &symbol.kind else {
      warn!("Expected EnumVariant kind, found {:?}", symbol.kind);
      return None;
    };

    let data = if fields.is_empty() {
      HirEnumVariantData::Unit
    } else {
      HirEnumVariantData::Struct(
        fields.iter().map(|f| HirStructField { name: f.name, ty: f.concrete_ty }).collect(),
      )
    };

    Some(HirVariant { symbol_id: OriginalSymbolId::Monomorphization(mono_id), data })
  }

  fn lower_variant_data(&mut self, data: &EnumVariantData) -> HirEnumVariantData {
    match data {
      EnumVariantData::Struct(fields) => {
        let hir_fields = fields
          .iter()
          .map(|f| HirStructField { name: f.name, ty: self.symbol_table.intern_type(f.ty.clone()) })
          .collect();
        HirEnumVariantData::Struct(hir_fields)
      }
      _ => HirEnumVariantData::Unit,
    }
  }
}
