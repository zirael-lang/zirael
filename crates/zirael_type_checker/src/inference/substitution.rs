use crate::TypeInference;
use std::collections::HashMap;
use zirael_parser::{SymbolKind, Type};
use zirael_utils::prelude::Identifier;

impl<'reports> TypeInference<'reports> {
    pub fn substitute_generic_params(
        &self,
        ty: &Type,
        struct_kind: &SymbolKind,
        generics: &[Type],
    ) -> Type {
        if let SymbolKind::Struct { generics: generic_params, .. } = struct_kind {
            let mut param_map = HashMap::new();
            for (param, concrete) in generic_params.iter().zip(generics.iter()) {
                param_map.insert(param.name, concrete.clone());
            }

            return self.substitute_type_with_map(ty, &param_map);
        }

        ty.clone()
    }

    pub fn substitute_type_with_map(
        &self,
        ty: &Type,
        param_map: &HashMap<Identifier, Type>,
    ) -> Type {
        match ty {
            Type::Named { name, generics } if generics.is_empty() => {
                if let Some(concrete) = param_map.get(name) {
                    return concrete.clone();
                }
                ty.clone()
            }
            Type::Named { name, generics } => Type::Named {
                name: *name,
                generics: generics
                    .iter()
                    .map(|g| self.substitute_type_with_map(g, param_map))
                    .collect(),
            },
            Type::Pointer(inner) => {
                Type::Pointer(Box::new(self.substitute_type_with_map(inner, param_map)))
            }
            Type::Reference(inner) => {
                Type::Reference(Box::new(self.substitute_type_with_map(inner, param_map)))
            }
            Type::Array(inner, size) => {
                Type::Array(Box::new(self.substitute_type_with_map(inner, param_map)), *size)
            }
            Type::Function { params, return_type } => Type::Function {
                params: params
                    .iter()
                    .map(|p| self.substitute_type_with_map(p, param_map))
                    .collect(),
                return_type: Box::new(self.substitute_type_with_map(return_type, param_map)),
            },
            _ => ty.clone(),
        }
    }
}
