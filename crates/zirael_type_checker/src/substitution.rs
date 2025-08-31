use crate::TypeInference;
use std::collections::HashMap;
use zirael_parser::{GenericParameter, Type};
use zirael_utils::prelude::Identifier;

impl<'reports> TypeInference<'reports> {
  pub fn substitute_generic_params(
    &mut self,
    ty: &mut Type,
    params: &Vec<GenericParameter>,
    generics: &[Type],
  ) {
    let mut param_map = HashMap::new();
    for (param, concrete) in params.iter().zip(generics.iter()) {
      param_map.insert(param.name, concrete.clone());
    }

    self.substitute_type_with_map(ty, &param_map);
  }

  pub fn substitute_type_with_map(&mut self, ty: &mut Type, param_map: &HashMap<Identifier, Type>) {
    if param_map.is_empty() {
      return;
    }

    match ty {
      Type::Named { name, generics } if generics.is_empty() => {
        if let Some(concrete) = param_map.get(name) {
          *ty = concrete.clone();
        }
      }
      Type::Named { name: _, generics } => {
        for generic in generics.iter_mut() {
          self.substitute_type_with_map(generic, param_map);
        }
      }
      Type::Pointer(inner) => {
        self.substitute_type_with_map(inner, param_map);
      }
      Type::Reference(inner) => {
        self.substitute_type_with_map(inner, param_map);
      }
      Type::Array(inner, _size) => {
        self.substitute_type_with_map(inner, param_map);
      }
      Type::Function { params, return_type } => {
        for param in params.iter_mut() {
          self.substitute_type_with_map(param, param_map);
        }
        self.substitute_type_with_map(return_type, param_map);
      }
      _ => {}
    }

    self.try_monomorphize_named_type(ty);
  }
}
