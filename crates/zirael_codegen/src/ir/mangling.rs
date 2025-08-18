use crate::ir::HirLowering;
use std::hash::{DefaultHasher, Hash as _, Hasher as _};
use zirael_parser::{SymbolId, Type};
use zirael_utils::{
    ident_table::resolve,
    prelude::{Mode, strip_same_root, warn},
};

impl<'reports> HirLowering<'reports> {
    pub fn mangle_symbol(&self, sym_id: SymbolId) -> String {
        if self.mode == Mode::Debug {
            self.symbol_table.get_c_identifier(sym_id).unwrap()
        } else {
            let symbol = self.symbol_table.get_symbol_unchecked(&sym_id);
            let canonical_id = symbol.canonical_symbol;

            if let Some(mangled) = self.symbol_table.get_mangled_name(canonical_id) {
                return mangled;
            }

            let canonical_symbol = self.symbol_table.get_symbol_unchecked(&canonical_id);
            let symbol_name = resolve(&canonical_symbol.name);

            let symbol_file = self.symbol_table.get_symbol_module(canonical_symbol.scope).unwrap();
            let file_path = self.sources.get_unchecked(symbol_file).path();
            let base_path = strip_same_root(file_path, self.root.clone()).with_extension("");

            let mut result = String::from("_ZN");
            result.push_str(&format!("{}{}", symbol_name.len(), symbol_name));

            for component in base_path.components() {
                if let Some(component_str) = component.as_os_str().to_str() {
                    result.push_str(&format!("{}{}", component_str.len(), component_str));
                }
            }

            let mut hasher = DefaultHasher::new();
            symbol_name.hash(&mut hasher);
            let hash = format!("{:x}", hasher.finish());
            result.push('h');
            result.push_str(&hash);
            result.push('E');

            self.symbol_table.add_mangled_name(canonical_id, result.clone());
            result
        }
    }

    pub fn mangle_monomorphized_symbol(
        &mut self,
        original_sym_id: SymbolId,
        type_arguments: &[Type],
    ) -> String {
        if self.mode == Mode::Debug {
            // In debug mode, use readable names
            let base_name = self.symbol_table.get_c_identifier(original_sym_id).unwrap();
            let type_suffix = type_arguments
                .iter()
                .map(|ty| self.mangle_type_for_name(ty))
                .collect::<Vec<_>>()
                .join("_");

            if type_suffix.is_empty() { base_name } else { format!("{base_name}__{type_suffix}") }
        } else {
            let symbol = self.symbol_table.get_symbol_unchecked(&original_sym_id);
            let canonical_id = symbol.canonical_symbol;
            let canonical_symbol = self.symbol_table.get_symbol_unchecked(&canonical_id);
            let symbol_name = resolve(&canonical_symbol.name);

            let symbol_file = self.symbol_table.get_symbol_module(canonical_symbol.scope).unwrap();
            let file_path = self.sources.get_unchecked(symbol_file).path();
            let base_path = strip_same_root(file_path, self.root.clone()).with_extension("");

            let mut result = String::from("_ZN");
            result.push_str(&format!("{}{}", symbol_name.len(), symbol_name));

            for component in base_path.components() {
                if let Some(component_str) = component.as_os_str().to_str() {
                    result.push_str(&format!("{}{}", component_str.len(), component_str));
                }
            }

            if !type_arguments.is_empty() {
                result.push('I');
                for ty in type_arguments {
                    self.mangle_type_into_string(ty, &mut result);
                }
                result.push('E');
            }

            let mut hasher = DefaultHasher::new();
            symbol_name.hash(&mut hasher);
            for ty in type_arguments {
                self.hash_type(ty, &mut hasher);
            }
            let hash = format!("{:x}", hasher.finish());
            result.push('h');
            result.push_str(&hash);
            result.push('E');

            result
        }
    }

    fn mangle_type_for_name(&mut self, ty: &Type) -> String {
        match ty {
            Type::String => "string".to_owned(),
            Type::Char => "char".to_owned(),
            Type::Int => "int".to_owned(),
            Type::Uint => "uint".to_owned(),
            Type::Float => "float".to_owned(),
            Type::Bool => "bool".to_owned(),
            Type::Void => "void".to_owned(),
            Type::Pointer(inner) => format!("ptr{}", self.mangle_type_for_name(inner)),
            Type::Reference(inner) => format!("ref{}", self.mangle_type_for_name(inner)),
            Type::Array(inner, Some(size)) => {
                format!("arr{}_{}", size, self.mangle_type_for_name(inner))
            }
            Type::Array(inner, None) => format!("slice{}", self.mangle_type_for_name(inner)),
            Type::Named { name, generics } => {
                if generics.is_empty() {
                    resolve(name)
                } else {
                    let generic_parts = generics
                        .iter()
                        .map(|g| self.mangle_type_for_name(g))
                        .collect::<Vec<_>>()
                        .join("_");
                    format!("{}_{}", resolve(name), generic_parts)
                }
            }
            Type::Function { params, return_type } => {
                let param_names = params
                    .iter()
                    .map(|p| self.mangle_type_for_name(p))
                    .collect::<Vec<_>>()
                    .join("_");
                format!("fn_{}__ret_{}", param_names, self.mangle_type_for_name(return_type))
            }
            Type::MonomorphizedSymbol(mono) => {
                let mono = self.handle_monomorphized_symbol(mono, false);
                self.mangle_type_for_name(&mono)
            }
            _ => {
                warn!("unknown type for mangling: {:?}", ty);
                "unknown".to_owned()
            }
        }
    }

    fn mangle_type_into_string(&self, ty: &Type, result: &mut String) {
        match ty {
            Type::Int => result.push('i'),
            Type::Uint => result.push('j'),
            Type::Float => result.push('f'),
            Type::Bool => result.push('b'),
            Type::Char => result.push('c'),
            Type::Void => result.push('v'),
            Type::String => {
                let name = "String";
                result.push_str(&format!("{}{}", name.len(), name));
            }
            Type::Pointer(inner) => {
                result.push('P');
                self.mangle_type_into_string(inner, result);
            }
            Type::Reference(inner) => {
                result.push('R');
                self.mangle_type_into_string(inner, result);
            }
            Type::Array(inner, Some(size)) => {
                result.push('A');
                result.push_str(&size.to_string());
                result.push('_');
                self.mangle_type_into_string(inner, result);
            }
            Type::Array(inner, None) => {
                result.push('A');
                result.push('_');
                self.mangle_type_into_string(inner, result);
            }
            Type::Named { name, generics } => {
                let name = resolve(name);
                result.push_str(&format!("{}{}", name.len(), name));
                if !generics.is_empty() {
                    result.push('I');
                    for generic in generics {
                        self.mangle_type_into_string(generic, result);
                    }
                    result.push('E');
                }
            }
            Type::Function { params, return_type } => {
                result.push('F');
                self.mangle_type_into_string(return_type, result);
                for param in params {
                    self.mangle_type_into_string(param, result);
                }
                result.push('E');
            }
            _ => {
                let fallback = "Unknown";
                result.push_str(&format!("{}{}", fallback.len(), fallback));
            }
        }
    }
}
