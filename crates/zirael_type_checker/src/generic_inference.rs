use crate::TypeInference;
use std::collections::HashMap;
use zirael_parser::{AstWalker, GenericParameter, Type};
use zirael_utils::prelude::{Identifier, Span, resolve};
use crate::symbol_table::TyId;

impl<'reports> TypeInference<'reports> {
  pub fn infer_generic_mappings(
    &mut self,
    generic_params: &[GenericParameter],
    param_types: &[Type],
    arg_types: &[Type],
  ) -> HashMap<Identifier, TyId> {
    let mut mapping = HashMap::new();

    for (param_ty, arg_ty) in param_types.iter().zip(arg_types.iter()) {
      self.infer_generic_types(param_ty, arg_ty, &mut mapping);
    }

    mapping
  }

  pub fn all_generics_resolved(
    &self,
    generics: &[GenericParameter],
    mapping: &HashMap<Identifier, Type>,
  ) -> bool {
    generics.iter().all(|g| mapping.get(&g.name).map_or(false, |ty| self.is_concrete_type(ty)))
  }

  pub fn get_unresolved_generics<'a>(
    &self,
    generics: &'a [GenericParameter],
    mapping: &HashMap<Identifier, TyId>,
  ) -> Vec<&'a GenericParameter> {
    generics.iter().filter(|g| !mapping.contains_key(&g.name)).collect()
  }

  /// Validate that type annotations match generic parameter count
  pub fn validate_type_annotations(
    &mut self,
    type_annotations: &[Type],
    generics: &[GenericParameter],
    call_span: &Span,
  ) -> bool {
    if !type_annotations.is_empty() && type_annotations.len() != generics.len() {
      self.simple_error(
        &format!(
          "wrong number of type annotations: expected {}, found {}",
          generics.len(),
          type_annotations.len()
        ),
        "in this call",
        call_span.clone(),
      );
      false
    } else {
      true
    }
  }

  pub fn create_mapping_from_annotations(
    &mut self,
    generics: &[GenericParameter],
    type_annotations: &[Type],
  ) -> HashMap<Identifier, TyId> {
    let mut mapping = HashMap::new();

    for (generic, annotation) in generics.iter().zip(type_annotations.iter()) {
      let mut resolved_annotation = annotation.clone();
      self.visit_type(&mut resolved_annotation);
      mapping.insert(generic.name, self.sym_table.intern_type(resolved_annotation));
    }

    mapping
  }

  pub fn report_unresolved_generics(
    &mut self,
    unresolved: &[&GenericParameter],
    function_name: &str,
    call_span: &Span,
    has_annotations: bool,
  ) {
    let generics_list = unresolved.iter().map(|g| resolve(&g.name)).collect::<Vec<_>>().join(", ");

    let suggestion = if !has_annotations {
      format!(
        "consider providing explicit type annotations, e.g., '{}<{}>({})'",
        function_name,
        unresolved.iter().map(|g| resolve(&g.name)).collect::<Vec<_>>().join(", "),
        "..."
      )
    } else {
      "the provided type annotations are insufficient to resolve all generic parameters".to_string()
    };

    self.error_with_suggestions(
      &format!(
        "cannot infer type parameter{} {} for function {}",
        if unresolved.len() > 1 { "s" } else { "" },
        generics_list,
        function_name
      ),
      vec![("in this call".to_string(), call_span.clone())],
      vec![suggestion],
    );
  }

  pub fn apply_generic_substitutions(
    &mut self,
    param_types: &mut [Type],
    mapping: &HashMap<Identifier, TyId>,
  ) {
    for param_ty in param_types.iter_mut() {
      self.substitute_type_with_map(param_ty, mapping);
    }
  }
}
