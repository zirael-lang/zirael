use crate::TypeInference;
use std::collections::HashMap;
use zirael_parser::{
  CallInfo, Expr, ExprKind, Path, Type, SymbolKind,
};
use zirael_utils::prelude::{Identifier, resolve};
use crate::monomorphization::MonomorphizationData;

impl<'reports> TypeInference<'reports> {
  pub fn infer_call(
    &mut self,
    callee: &mut Expr,
    args: &mut Vec<Expr>,
    call_info: &mut Option<CallInfo>,
    type_annotations: &mut Vec<Type>,
  ) -> Type {
    let sym_id = match self.extract_symbol_from_callee(callee) {
      Some(id) => id,
      None => return Type::Error,
    };

    let sym = self.symbol_table().get_symbol_unchecked(&sym_id);
    let signature = match self.extract_function_signature(&sym.kind, &callee.span) {
      Some(sig) => sig,
      None => return Type::Error,
    };

    if !self.validate_argument_count(&signature, args, &callee.span) {
      return Type::Error;
    }

    if !self.validate_type_annotations(type_annotations, &signature.generics, &callee.span) {
      return Type::Error;
    }

    self.infer_argument_types(args);

    let generic_mapping = match self.resolve_generics(&signature, args, type_annotations, &callee.span) {
      Ok(mapping) => mapping,
      Err(()) => return Type::Error,
    };

    let concrete_signature = self.create_concrete_signature(signature.clone(), &generic_mapping);
    let param_types_to_check = self.get_parameter_types_to_check(&concrete_signature);
    let param_types = self.extract_parameter_types(param_types_to_check);
    
    if !self.validate_argument_types(args, &param_types) {
      return Type::Error;
    }

    let monomorphized_id = self.create_monomorphization_if_needed(
      sym_id,
      &signature,
      &generic_mapping,
      &concrete_signature,
    );

    *call_info = Some(CallInfo {
      original_symbol: sym_id,
      monomorphized_id,
      concrete_types: generic_mapping,
    });

    concrete_signature.return_type
  }

  fn extract_symbol_from_callee(&self, callee: &Expr) -> Option<zirael_parser::SymbolId> {
    match &callee.kind {
      ExprKind::Identifier(_, Some(sym_id)) => Some(*sym_id),
      ExprKind::Path(Path { segments, .. }) => {
        segments.last().and_then(|seg| seg.symbol_id)
      }
      _ => None,
    }
  }

  fn extract_function_signature(
    &mut self,
    symbol_kind: &SymbolKind,
    call_span: &zirael_utils::prelude::Span,
  ) -> Option<zirael_parser::FunctionSignature> {
    match symbol_kind {
      SymbolKind::Function { signature, .. } => Some(signature.clone()),
      _ => {
        self.validate_callable_symbol(symbol_kind, call_span);
        None
      }
    }
  }

  fn resolve_generics(
    &mut self,
    signature: &zirael_parser::FunctionSignature,
    args: &[Expr],
    type_annotations: &[Type],
    call_span: &zirael_utils::prelude::Span,
  ) -> Result<HashMap<Identifier, Type>, ()> {
    let mut generic_mapping = HashMap::new();

    if !type_annotations.is_empty() {
      generic_mapping = self.create_mapping_from_annotations(&signature.generics, type_annotations);
    } else if !signature.generics.is_empty() {
      let param_types_to_check = self.get_parameter_types_to_check(signature);
      let param_types = self.extract_parameter_types(param_types_to_check);
      let arg_types: Vec<Type> = args.iter().map(|arg| arg.ty.clone()).collect();
      
      generic_mapping = self.infer_generic_mappings(&signature.generics, &param_types, &arg_types);
    }

    let unresolved = self.get_unresolved_generics(&signature.generics, &generic_mapping);
    if !unresolved.is_empty() {
      let function_name = resolve(&self.symbol_table().get_symbol_unchecked(
        &signature.parameters.first().unwrap().symbol_id.unwrap() // This needs fixing
      ).name);
      
      self.report_unresolved_generics(
        &unresolved,
        &function_name,
        call_span,
        !type_annotations.is_empty(),
      );
      return Err(());
    }

    Ok(generic_mapping)
  }

  fn create_concrete_signature(
    &mut self,
    mut signature: zirael_parser::FunctionSignature,
    generic_mapping: &HashMap<Identifier, Type>,
  ) -> zirael_parser::FunctionSignature {
    for param in &mut signature.parameters {
      self.substitute_type_with_map(&mut param.ty, generic_mapping);
    }

    self.substitute_type_with_map(&mut signature.return_type, generic_mapping);

    signature
  }

  fn create_monomorphization_if_needed(
    &mut self,
    symbol_id: zirael_parser::SymbolId,
    original_signature: &zirael_parser::FunctionSignature,
    generic_mapping: &HashMap<Identifier, Type>,
    concrete_signature: &zirael_parser::FunctionSignature,
  ) -> Option<zirael_parser::MonomorphizationId> {
    if !original_signature.generics.is_empty() && !generic_mapping.is_empty() {
      Some(self.record_monomorphization_with_id(
        symbol_id,
        generic_mapping,
        Some(MonomorphizationData::Signature(concrete_signature.clone())),
      ))
    } else {
      None
    }
  }
}
