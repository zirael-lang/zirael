use crate::ir::{
    HirLowering, IrBlock, IrExpr, IrExprKind, IrField, IrFunction, IrItem, IrItemKind, IrModule,
    IrParam, IrStmt, IrStruct,
};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    hash::{DefaultHasher, Hash as _},
};
use zirael_parser::{
    Function, FunctionSignature, GenericParameter, MonomorphizationId, Symbol, SymbolId,
    SymbolKind, SymbolRelationNode, Type, monomorphized_symbol::MonomorphizedSymbol,
};
use zirael_type_checker::{MonomorphizationData, MonomorphizationEntry};
use zirael_utils::prelude::{Identifier, get_or_intern, resolve};

impl<'reports> HirLowering<'reports> {
    pub fn process_monomorphization_entries(&mut self, module: &mut IrModule) {
        // TODO: find a way to do that without cloning the entries
        let entries: Vec<_> = self.mono_table.entries.clone().into_iter().collect();

        let (struct_entries, func_entries): (Vec<_>, Vec<_>) =
            entries.into_iter().partition(|(_, entry)| {
                let symbol = self.symbol_table.get_symbol_unchecked(&entry.original_id);
                matches!(symbol.kind, SymbolKind::Struct { .. })
            });

        for (id, entry) in struct_entries {
            if let Some(monomorphized_item) = self.create_monomorphized_item(&id, &entry, module) {
                module.mono_items.push(monomorphized_item);
            }
        }

        for (id, entry) in func_entries {
            if let Some(monomorphized_item) = self.create_monomorphized_item(&id, &entry, module) {
                module.mono_items.push(monomorphized_item);
            }
        }
    }

    fn get_type_arguments(
        &mut self,
        generics: &Vec<GenericParameter>,
        concrete_types: &HashMap<Identifier, Type>,
    ) -> Option<Vec<Type>> {
        let all_generics_mapped = generics.iter().all(|g| concrete_types.contains_key(&g.name));

        if !all_generics_mapped {
            return None;
        }

        Some(
            generics
                .iter()
                .map(|param| concrete_types.get(&param.name).cloned())
                .collect::<Option<Vec<_>>>()?,
        )
    }

    fn create_monomorphized_item(
        &mut self,
        id: &MonomorphizationId,
        entry: &MonomorphizationEntry,
        module: &IrModule,
    ) -> Option<IrItem> {
        let original_id = entry.original_id;
        let concrete_types = &entry.concrete_types;

        let original_item = module.items.iter().find(|item| item.sym_id == original_id)?;
        let original_symbol = self.symbol_table.get_symbol_unchecked(&original_id);

        self.current_mono_id = Some(id.clone());
        match (&original_symbol.kind, &original_item.kind) {
            (SymbolKind::Function { signature: sig, .. }, IrItemKind::Function(func)) => {
                let signature = if let Some(data) = &entry.data
                    && let MonomorphizationData::Signature(signature) = data
                {
                    signature
                } else {
                    sig
                };

                let mangled_name = self.get_monomorphized_name(*id);
                let monomorphized_function = self.monomorphize_function(
                    mangled_name.clone(),
                    signature,
                    func,
                    concrete_types,
                );

                Some(IrItem {
                    name: mangled_name,
                    kind: IrItemKind::Function(monomorphized_function),
                    sym_id: original_id,
                    mono_id: Some(id.clone()),
                })
            }
            (SymbolKind::Struct { generics, .. }, IrItemKind::Struct(struct_def)) => {
                let Some(type_arguments) = self.get_type_arguments(generics, concrete_types) else {
                    return None;
                };

                let mangled_name =
                    self.mangle_monomorphized_symbol(original_id, *id, &type_arguments);
                let monomorphized_struct =
                    self.monomorphize_struct(mangled_name.clone(), struct_def, concrete_types);

                Some(IrItem {
                    name: mangled_name,
                    kind: IrItemKind::Struct(monomorphized_struct),
                    sym_id: original_id,
                    mono_id: Some(id.clone()),
                })
            }
            _ => None,
        }
    }

    fn monomorphize_struct(
        &mut self,
        name: String,
        struct_def: &IrStruct,
        type_map: &HashMap<Identifier, Type>,
    ) -> IrStruct {
        let fields = struct_def
            .fields
            .iter()
            .map(|field| {
                let substituted_ty = self.substitute_type(&field.ty, type_map);
                IrField { name: field.name.clone(), ty: substituted_ty }
            })
            .collect_vec();
        IrStruct { name, fields }
    }

    fn monomorphize_function(
        &mut self,
        name: String,
        signature: &FunctionSignature,
        original: &IrFunction,
        type_map: &HashMap<Identifier, Type>,
    ) -> IrFunction {
        let parameters = signature
            .parameters
            .iter()
            .map(|param| {
                let substituted_ty = self.substitute_type(&param.ty, type_map);
                IrParam { name: param.name.clone().to_string(), ty: substituted_ty }
            })
            .collect();

        let mut return_type = self.substitute_type(&signature.return_type, type_map);
        return_type = self.lower_type(return_type.clone());

        let body = original.body.as_ref().map(|body| self.monomorphize_block(body, type_map));

        IrFunction {
            name,
            parameters,
            return_type,
            body,
            is_async: original.is_async,
            is_const: original.is_const,
            is_extern: original.is_extern,
            abi: original.abi.clone(),
        }
    }

    fn monomorphize_block(
        &mut self,
        original: &IrBlock,
        type_map: &HashMap<Identifier, Type>,
    ) -> IrBlock {
        let stmts =
            original.stmts.iter().map(|stmt| self.monomorphize_stmt(stmt, type_map)).collect();

        IrBlock { stmts }
    }

    fn monomorphize_stmt(
        &mut self,
        original: &IrStmt,
        type_map: &HashMap<Identifier, Type>,
    ) -> IrStmt {
        match original {
            IrStmt::Var(name, init) => {
                IrStmt::Var(name.clone(), self.monomorphize_expr(init, type_map))
            }
            IrStmt::Expr(expr) => IrStmt::Expr(self.monomorphize_expr(expr, type_map)),
            IrStmt::Return(expr) => {
                if let Some(expr) = expr {
                    IrStmt::Return(Some(self.monomorphize_expr(expr, type_map)))
                } else {
                    IrStmt::Return(None)
                }
            }
        }
    }

    pub fn monomorphize_expr(
        &mut self,
        original: &IrExpr,
        type_map: &HashMap<Identifier, Type>,
    ) -> IrExpr {
        let ty = self.substitute_type(&original.ty, type_map);

        let kind = match &original.kind {
            IrExprKind::Symbol(sym) => IrExprKind::Symbol(sym.clone()),

            IrExprKind::Block(block) => IrExprKind::Block(self.monomorphize_block(block, type_map)),

            IrExprKind::Literal(lit) => IrExprKind::Literal(lit.clone()),

            IrExprKind::Call(func, args) => {
                let mono_args =
                    args.iter().map(|arg| self.monomorphize_expr(arg, type_map)).collect();

                IrExprKind::Call(func.clone(), mono_args)
            }

            IrExprKind::CCall(func, args) => {
                let mono_args =
                    args.iter().map(|arg| self.monomorphize_expr(arg, type_map)).collect();

                IrExprKind::CCall(func.clone(), mono_args)
            }

            IrExprKind::Assign(left, right) => IrExprKind::Assign(
                Box::new(self.monomorphize_expr(left, type_map)),
                Box::new(self.monomorphize_expr(right, type_map)),
            ),

            IrExprKind::Unary(op, expr) => {
                IrExprKind::Unary(op.clone(), Box::new(self.monomorphize_expr(expr, type_map)))
            }

            IrExprKind::Binary(left, op, right) => IrExprKind::Binary(
                Box::new(self.monomorphize_expr(left, type_map)),
                op.clone(),
                Box::new(self.monomorphize_expr(right, type_map)),
            ),

            IrExprKind::StructInit(struct_name, fields) => {
                let mut new_fields = HashMap::new();
                for (field_name, field_expr) in fields {
                    let new_field_expr = self.monomorphize_expr(field_expr, type_map);
                    new_fields.insert(field_name.clone(), new_field_expr);
                }

                IrExprKind::StructInit(struct_name.clone(), new_fields)
            }

            IrExprKind::Type(ty) => IrExprKind::Type(self.substitute_type(ty, type_map)),

            IrExprKind::FieldAccess(fields) => {
                let mono_fields = fields.iter().map(|f| f.clone()).collect();
                IrExprKind::FieldAccess(mono_fields)
            }
        };

        IrExpr { ty, kind }
    }

    fn substitute_type(&mut self, ty: &Type, type_map: &HashMap<Identifier, Type>) -> Type {
        self.substitute_type_with_visited(ty, type_map, &mut HashSet::new())
    }

    fn substitute_type_with_visited(
        &mut self,
        ty: &Type,
        type_map: &HashMap<Identifier, Type>,
        visited: &mut HashSet<Identifier>,
    ) -> Type {
        let new_ty = match ty {
            Type::TypeVariable { id: _, name } => {
                if let Some(concrete_type) = type_map.get(name) {
                    concrete_type.clone()
                } else {
                    ty.clone()
                }
            }

            Type::Pointer(inner) => {
                Type::Pointer(Box::new(self.substitute_type_with_visited(inner, type_map, visited)))
            }

            Type::Reference(inner) => Type::Reference(Box::new(
                self.substitute_type_with_visited(inner, type_map, visited),
            )),

            Type::Array(inner, size) => Type::Array(
                Box::new(self.substitute_type_with_visited(inner, type_map, visited)),
                *size,
            ),

            Type::Named { name, generics } => {
                if type_map.contains_key(name) {
                    if visited.contains(name) {
                        return ty.clone();
                    }

                    visited.insert(*name);
                    let result = self.substitute_type_with_visited(
                        type_map.get(name).unwrap(),
                        type_map,
                        visited,
                    );
                    visited.remove(name);
                    return result;
                }

                if generics.is_empty() {
                    ty.clone()
                } else {
                    let substituted_generics = generics
                        .iter()
                        .map(|g| self.substitute_type_with_visited(g, type_map, visited))
                        .collect();

                    Type::Named { name: *name, generics: substituted_generics }
                }
            }

            Type::Function { params, return_type } => {
                let substituted_params = params
                    .iter()
                    .map(|p| self.substitute_type_with_visited(p, type_map, visited))
                    .collect();

                let substituted_return =
                    Box::new(self.substitute_type_with_visited(return_type, type_map, visited));

                Type::Function { params: substituted_params, return_type: substituted_return }
            }

            Type::MonomorphizedSymbol(sym) => self.handle_monomorphized_symbol(sym, true),

            _ => ty.clone(),
        };

        self.lower_type(new_ty)
    }

    pub fn handle_monomorphized_symbol(
        &mut self,
        sym: &MonomorphizedSymbol,
        add_struct: bool,
    ) -> Type {
        let name = self.get_monomorphized_name(sym.id);
        let sym_id = self.mono_table.entries[&sym.id].original_id;
        let original_symbol = self.symbol_table.get_symbol_unchecked(&sym_id);
        self.new_relation(SymbolRelationNode::Monomorphization(sym.id.clone()));

        if let SymbolKind::Struct { .. } = &original_symbol.kind {
            Type::Named {
                name: get_or_intern(&if add_struct { format!("struct {name}") } else { name }),
                generics: vec![],
            }
        } else {
            Type::Named { name: get_or_intern(&name), generics: vec![] }
        }
    }

    pub(crate) fn hash_type(&mut self, ty: &Type, hasher: &mut DefaultHasher) {
        match ty {
            Type::String => "string".hash(hasher),
            Type::Char => "char".hash(hasher),
            Type::Int => "int".hash(hasher),
            Type::Uint => "uint".hash(hasher),
            Type::Float => "float".hash(hasher),
            Type::Bool => "bool".hash(hasher),
            Type::Void => "void".hash(hasher),

            Type::Pointer(inner) => {
                "ptr".hash(hasher);
                self.hash_type(inner, hasher);
            }

            Type::Reference(inner) => {
                "ref".hash(hasher);
                self.hash_type(inner, hasher);
            }

            Type::Array(inner, size) => {
                "array".hash(hasher);
                if let Some(s) = size {
                    s.hash(hasher);
                }
                self.hash_type(inner, hasher);
            }

            Type::Named { name, generics } => {
                resolve(name).hash(hasher);
                for g in generics {
                    self.hash_type(g, hasher);
                }
            }

            Type::Function { params, return_type } => {
                "function".hash(hasher);
                for p in params {
                    self.hash_type(p, hasher);
                }
                self.hash_type(return_type, hasher);
            }

            Type::TypeVariable { id, name } => {
                id.hash(hasher);
                resolve(name).hash(hasher);
            }

            _ => "unknown".hash(hasher),
        }
    }

    pub fn get_generics_for_symbol(&self, symbol: &Symbol) -> Vec<GenericParameter> {
        if let SymbolKind::Function { signature, .. } = &symbol.kind {
            let mut generics = vec![];

            if let Some(parent_struct) = self.symbol_table.is_a_method(symbol.canonical_symbol) {
                let parent_struct = self.symbol_table.get_symbol_unchecked(&parent_struct);
                if let SymbolKind::Struct { generics: gens, .. } = &parent_struct.kind {
                    generics.extend(gens.clone());
                }
            }
            generics.extend(signature.generics.clone());

            generics
        } else if let SymbolKind::Struct { generics, .. } = &symbol.kind {
            generics.clone()
        } else {
            unreachable!()
        }
    }

    pub fn get_monomorphized_name(&mut self, mono_id: MonomorphizationId) -> String {
        if let Some(entry) = self.mono_table.entries.get(&mono_id) {
            let original_symbol = self.symbol_table.get_symbol_unchecked(&entry.original_id);
            let generics = self.get_generics_for_symbol(&original_symbol);

            let type_arguments: Vec<Type> = generics
                .iter()
                .filter_map(|param| entry.concrete_types.get(&param.name))
                .cloned()
                .collect();

            return self.mangle_monomorphized_symbol(entry.original_id, mono_id, &type_arguments);
        }

        panic!("Monomorphization ID {mono_id:?} not found")
    }
}
