use crate::TypeInference;
use zirael_parser::{AstWalker, Expr, FunctionSignature, Parameter, SymbolKind, Type};
use zirael_utils::prelude::Span;

impl<'reports> TypeInference<'reports> {
  pub fn validate_argument_count(
    &mut self,
    signature: &FunctionSignature,
    args: &[Expr],
    call_span: &Span,
  ) -> bool {
    let expected_count = if signature.is_static() {
      signature.parameters.len()
    } else {
      signature.parameters.len() - 1
    };

    if args.len() != expected_count {
      self.simple_error(
        &format!("wrong number of arguments: expected {}, found {}", expected_count, args.len()),
        "in this call",
        call_span.clone(),
      );
      false
    } else {
      true
    }
  }

  pub fn validate_argument_types(&mut self, args: &[Expr], param_types: &[Type]) -> bool {
    let mut all_valid = true;

    for (i, (arg, param_type)) in args.iter().zip(param_types.iter()).enumerate() {
      if !self.eq(&arg.ty, param_type) {
        self.type_mismatch_with_context(
          param_type,
          &arg.ty,
          arg.span.clone(),
          &format!("argument {}", i + 1),
        );
        all_valid = false;
      }
    }

    all_valid
  }

  pub fn get_parameter_types_to_check<'a>(
    &self,
    signature: &'a FunctionSignature,
  ) -> &'a [Parameter] {
    if signature.is_static() { &signature.parameters[..] } else { &signature.parameters[1..] }
  }

  pub fn extract_parameter_types(&self, params: &[Parameter]) -> Vec<Type> {
    params.iter().map(|p| p.ty.clone()).collect()
  }

  pub fn infer_argument_types(&mut self, args: &mut [Expr]) {
    for arg in args.iter_mut() {
      self.infer_expr(arg);
      self.visit_type(&mut arg.ty);
    }
  }

  pub fn validate_callable_symbol(&mut self, symbol_kind: &SymbolKind, call_span: &Span) -> bool {
    match symbol_kind {
      SymbolKind::Function { .. } => true,
      _ => {
        self.simple_error(
          &format!("cannot call non-function type: {}", symbol_kind.name()),
          "in this call",
          call_span.clone(),
        );
        false
      }
    }
  }
}
