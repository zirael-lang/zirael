use crate::ir::HirLowering;
use std::hash::{DefaultHasher, Hash as _, Hasher as _};
use zirael_parser::{
    MonomorphizationId, Symbol, SymbolId, SymbolKind, Type, Type::MonomorphizedSymbol,
};
use zirael_utils::{
    ident_table::resolve,
    prelude::{Mode, strip_same_root, warn},
};

impl<'reports> HirLowering<'reports> {
    pub fn mangle_symbol(&mut self, sym_id: SymbolId) -> String {
        if self.mode == Mode::Debug {
            self.mangle_debug_symbol(sym_id, None, &[])
        } else {
            self.mangle_release_symbol(sym_id, None, &[])
        }
    }

    pub fn get_sym_name(&mut self, sym: &Symbol, mono_id: Option<MonomorphizationId>) -> String {
        let base = resolve(&sym.name);

        if let Some(parent_struct) = self.symbol_table.is_a_method(sym.id) {
            let parent_struct = self.symbol_table.get_symbol_unchecked(&parent_struct);

            if let SymbolKind::TypeExtension { ty, .. } = parent_struct.kind {
                return format!("ext_{}_{}", self.mangle_type_for_name(&ty), base);
            }

            let parent_struct_name = resolve(&parent_struct.name);
            format!("{}_{}", parent_struct_name, base)
        } else {
            base
        }
    }

    pub fn mangle_monomorphized_symbol(
        &mut self,
        original_sym_id: SymbolId,
        mono_id: MonomorphizationId,
        type_arguments: &[Type],
    ) -> String {
        if self.mode == Mode::Debug {
            self.mangle_debug_symbol(original_sym_id, Some(mono_id), type_arguments)
        } else {
            self.mangle_release_symbol(original_sym_id, Some(mono_id), type_arguments)
        }
    }

    fn mangle_debug_symbol(
        &mut self,
        sym_id: SymbolId,
        mono_id: Option<MonomorphizationId>,
        type_arguments: &[Type],
    ) -> String {
        let symbol = self.symbol_table.get_symbol_unchecked(&sym_id);
        let canonical_symbol = self.symbol_table.get_symbol_unchecked(&symbol.canonical_symbol);

        let base_name = format!("zirael_{}", self.get_sym_name(&canonical_symbol, mono_id));

        if type_arguments.is_empty() {
            base_name
        } else {
            let type_suffix = type_arguments
                .iter()
                .map(|ty| self.mangle_type_for_name(ty))
                .collect::<Vec<_>>()
                .join("_");
            format!("{}__{}", base_name, type_suffix)
        }
    }

    fn mangle_release_symbol(
        &mut self,
        sym_id: SymbolId,
        mono_id: Option<MonomorphizationId>,
        type_arguments: &[Type],
    ) -> String {
        let symbol = self.symbol_table.get_symbol_unchecked(&sym_id);
        let canonical_id = symbol.canonical_symbol;

        let cache_key = self.compute_cache_key(canonical_id, type_arguments);
        if let Some(cached) = self.get_cached_mangled_name(canonical_id, &cache_key) {
            return cached;
        }

        let canonical_symbol = self.symbol_table.get_symbol_unchecked(&canonical_id);
        let result = self.build_mangled_name(&canonical_symbol, mono_id, type_arguments);

        self.cache_mangled_name(canonical_id, cache_key, result.clone());
        result
    }

    fn build_mangled_name(
        &mut self,
        symbol: &Symbol,
        mono_id: Option<MonomorphizationId>,
        type_arguments: &[Type],
    ) -> String {
        let symbol_name = self.get_sym_name(symbol, mono_id);
        let symbol_file = self.symbol_table.get_symbol_module(symbol.scope).unwrap();
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

        let hash = self.compute_symbol_hash(&symbol_name, type_arguments);
        result.push('h');
        result.push_str(&hash);
        result.push('E');

        result
    }

    fn compute_cache_key(&mut self, symbol_id: SymbolId, type_arguments: &[Type]) -> String {
        if type_arguments.is_empty() {
            format!("sym_{}", symbol_id.index())
        } else {
            let type_hash = {
                let mut hasher = DefaultHasher::new();
                for ty in type_arguments {
                    self.hash_type(ty, &mut hasher);
                }
                hasher.finish()
            };
            format!("sym_{}_{:x}", symbol_id.index(), type_hash)
        }
    }

    fn get_cached_mangled_name(&self, symbol_id: SymbolId, _cache_key: &str) -> Option<String> {
        self.symbol_table.get_mangled_name(symbol_id)
    }

    fn cache_mangled_name(&self, symbol_id: SymbolId, _cache_key: String, mangled_name: String) {
        self.symbol_table.add_mangled_name(symbol_id, mangled_name);
    }

    fn compute_symbol_hash(&mut self, symbol_name: &str, type_arguments: &[Type]) -> String {
        let mut hasher = DefaultHasher::new();
        symbol_name.hash(&mut hasher);
        for ty in type_arguments {
            self.hash_type(ty, &mut hasher);
        }
        format!("{:x}", hasher.finish())
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
