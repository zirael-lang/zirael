use crate::TypeInference;
use crate::symbol_table::{MonomorphizedFunction, MonomorphizedSymbol, TyId};
use std::collections::HashMap;
use zirael_parser::{
  CallInfo, Expr, ExprKind, FunctionSignature, MonomorphizationId, Path, SymbolId, SymbolKind, Type,
};
use zirael_utils::prelude::{Identifier, Span, resolve};

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

    let sym = match self.symbol_table().get_symbol(sym_id) {
      Ok(symbol) => symbol,
      Err(_) => return Type::Error,
    };
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

    let generic_mapping =
      match self.resolve_generics(&signature, args, type_annotations, &callee.span) {
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
      sym.name,
      &signature,
      &generic_mapping,
      &concrete_signature,
    );

    *call_info =
      Some(CallInfo { original_symbol: sym_id, monomorphized_id, concrete_types: generic_mapping });

    concrete_signature.return_type
  }

  fn extract_symbol_from_callee(&self, callee: &Expr) -> Option<SymbolId> {
    match &callee.kind {
      ExprKind::Identifier(_, Some(sym_id)) => Some(*sym_id),
      ExprKind::Path(Path { segments, .. }) => segments.last().and_then(|seg| seg.symbol_id),
      _ => None,
    }
  }

  fn extract_function_signature(
    &mut self,
    symbol_kind: &SymbolKind,
    call_span: &zirael_utils::prelude::Span,
  ) -> Option<FunctionSignature> {
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
    signature: &FunctionSignature,
    args: &[Expr],
    type_annotations: &[Type],
    call_span: &Span,
  ) -> Result<HashMap<Identifier, TyId>, ()> {
    let mut generic_mapping: HashMap<Identifier, TyId> = HashMap::new();

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
      let function_name = if let Some(param) = signature.parameters.first()
        && let Some(symbol_id) = param.symbol_id
        && let Ok(symbol) = self.symbol_table().get_symbol(symbol_id)
      {
        resolve(&symbol.name)
      } else {
        "unknown".to_string()
      };

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
    mut signature: FunctionSignature,
    generic_mapping: &HashMap<Identifier, TyId>,
  ) -> FunctionSignature {
    for param in &mut signature.parameters {
      self.substitute_type_with_map(&mut param.ty, generic_mapping);
    }

    self.substitute_type_with_map(&mut signature.return_type, generic_mapping);

    signature
  }

  fn create_monomorphization_if_needed(
    &mut self,
    symbol_id: SymbolId,
    name: Identifier,
    original_signature: &FunctionSignature,
    generic_mapping: &HashMap<Identifier, TyId>,
    concrete_signature: &FunctionSignature,
  ) -> Option<MonomorphizationId> {
    if !original_signature.generics.is_empty() && !generic_mapping.is_empty() {
      let mono =
        MonomorphizedFunction::new(symbol_id, name, "TODO".to_string(), concrete_signature.clone())
          .with_concrete_types(generic_mapping.clone());

      Some(self.sym_table.add_monomorphized_symbol(MonomorphizedSymbol::Function(mono)))
    } else {
      None
    }
  }
}

pub struct FunctionMonomorphizer {}
