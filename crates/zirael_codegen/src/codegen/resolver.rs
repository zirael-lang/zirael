use crate::codegen::{Codegen, Gen};
use crate::ir::{IrEnum, IrItem, IrItemKind, IrModule, IrVariant};
use zirael_parser::{EnumVariantData, SymbolRelationNode};

pub struct OrderResolver;

impl OrderResolver {
  pub fn resolve_order(
    modules: &[IrModule],
    mut order: Vec<SymbolRelationNode>,
  ) -> Vec<SymbolRelationNode> {
    if order.is_empty() {
      let module_items: Vec<_> = modules.iter().flat_map(|m| &m.items).collect();
      order = module_items.iter().map(|i| SymbolRelationNode::Symbol(i.sym_id)).collect();
    }
    order
  }

  pub fn process_enum_monomorphization(modules: &mut [IrModule]) {
    let mono_items: Vec<_> = modules.iter().flat_map(|m| &m.mono_items).cloned().collect();

    for module in modules {
      for item in &mut module.items {
        if let IrItemKind::Enum(ref mut enum_data) = item.kind {
          Self::merge_enum_variants(enum_data, &mono_items);
        }
      }
    }
  }

  fn merge_enum_variants(enum_data: &mut IrEnum, mono_items: &[IrItem]) {
    let mut mono_variants = vec![];
    let mut remove_indices = vec![];

    for (i, variant) in enum_data.variants.iter().enumerate() {
      let variant_mono_items: Vec<_> = mono_items
        .iter()
        .filter_map(|m| {
          if let IrItemKind::EnumVariant(ref ir_variant) = m.kind {
            if variant.symbol_id == ir_variant.symbol_id { Some(ir_variant.clone()) } else { None }
          } else {
            None
          }
        })
        .collect();

      if !variant_mono_items.is_empty() {
        mono_variants.extend(variant_mono_items);
        remove_indices.push(i);
      }
    }

    if !mono_variants.is_empty() && !remove_indices.is_empty() {
      remove_indices.sort_by(|a, b| b.cmp(a));
      for i in remove_indices {
        enum_data.variants.remove(i);
      }
      enum_data.variants.extend(mono_variants);
    }
  }

  pub fn generate_remaining_mono_items(
    implementation: &mut Codegen,
    mono_items: &[IrItem],
    order: &[SymbolRelationNode],
  ) {
    for mono_item in mono_items {
      if let Some(mono_id) = mono_item.mono_id {
        let mono_node = SymbolRelationNode::Monomorphization(mono_id);
        if !order.contains(&mono_node) {
          mono_item.generate(implementation);
        }
      }
    }
  }
}
