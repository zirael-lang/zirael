use crate::ir::{
    HirLowering, IrBlock, IrExpr, IrExprKind, IrFunction, IrItem, IrItemKind, IrModule, IrParam,
    IrStmt,
};
use std::{
    collections::HashMap,
    hash::{DefaultHasher, Hash},
};
use zirael_parser::{MonomorphizationId, SymbolId, SymbolKind, Type};
use zirael_type_checker::MonomorphizationEntry;
use zirael_utils::prelude::{Identifier, resolve, warn};

impl<'reports> HirLowering<'reports> {
    pub fn process_monomorphization_entries(&mut self, module: &mut IrModule) {
        for (_, entry) in &self.mono_table.entries {
            if let Some(monomorphized_item) = self.create_monomorphized_item(entry, module) {
                module.mono_items.push(monomorphized_item);
            }
        }
    }

    fn create_monomorphized_item(
        &self,
        entry: &MonomorphizationEntry,
        module: &IrModule,
    ) -> Option<IrItem> {
        let original_id = entry.original_id;
        let concrete_types = &entry.concrete_types;

        let original_item = module.items.iter().find(|item| item.sym_id == original_id)?;

        match &original_item.kind {
            IrItemKind::Function(func) => {
                let original_symbol = self.symbol_table.get_symbol_unchecked(&original_id);

                if let SymbolKind::Function { signature, .. } = &original_symbol.kind {
                    let all_generics_mapped =
                        signature.generics.iter().all(|g| concrete_types.contains_key(&g.name));

                    if !all_generics_mapped {
                        return None;
                    }

                    let type_arguments = signature
                        .generics
                        .iter()
                        .filter_map(|param| concrete_types.get(&param.name))
                        .cloned()
                        .collect::<Vec<_>>();

                    let mangled_name =
                        self.mangle_monomorphized_function(original_id, &type_arguments);

                    let monomorphized_function = self.monomorphize_function(func, concrete_types);

                    Some(IrItem {
                        name: mangled_name,
                        kind: IrItemKind::Function(monomorphized_function),
                        sym_id: original_id,
                    })
                } else {
                    None
                }
            }
            _ => {
                warn!("Unsupported item kind for monomorphization: {:?}", original_item.kind);
                None
            }
        }
    }

    fn monomorphize_function(
        &self,
        original: &IrFunction,
        type_map: &HashMap<Identifier, Type>,
    ) -> IrFunction {
        let parameters = original
            .parameters
            .iter()
            .map(|param| IrParam {
                name: param.name.clone(),
                ty: self.substitute_type(&param.ty, type_map),
            })
            .collect();

        let return_type = self.substitute_type(&original.return_type, type_map);

        let body = if let Some(body) = &original.body {
            Some(self.monomorphize_block(body, type_map))
        } else {
            None
        };

        IrFunction {
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
        &self,
        original: &IrBlock,
        type_map: &HashMap<Identifier, Type>,
    ) -> IrBlock {
        let stmts =
            original.stmts.iter().map(|stmt| self.monomorphize_stmt(stmt, type_map)).collect();

        IrBlock { stmts }
    }

    fn monomorphize_stmt(&self, original: &IrStmt, type_map: &HashMap<Identifier, Type>) -> IrStmt {
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

    fn monomorphize_expr(&self, original: &IrExpr, type_map: &HashMap<Identifier, Type>) -> IrExpr {
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

            IrExprKind::Type(ty) => IrExprKind::Type(self.substitute_type(ty, type_map)),
        };

        IrExpr { ty, kind }
    }

    fn substitute_type(&self, ty: &Type, type_map: &HashMap<Identifier, Type>) -> Type {
        match ty {
            Type::TypeVariable { id: _, name } => {
                if let Some(concrete_type) = type_map.get(name) {
                    concrete_type.clone()
                } else {
                    ty.clone()
                }
            }

            Type::Pointer(inner) => Type::Pointer(Box::new(self.substitute_type(inner, type_map))),

            Type::Reference(inner) => {
                Type::Reference(Box::new(self.substitute_type(inner, type_map)))
            }

            Type::Array(inner, size) => {
                Type::Array(Box::new(self.substitute_type(inner, type_map)), *size)
            }

            Type::Named { name, generics } => {
                if type_map.contains_key(name) {
                    return self.substitute_type(type_map.get(name).unwrap(), type_map);
                }

                if generics.is_empty() {
                    ty.clone()
                } else {
                    let substituted_generics =
                        generics.iter().map(|g| self.substitute_type(g, type_map)).collect();

                    Type::Named { name: *name, generics: substituted_generics }
                }
            }

            Type::Function { params, return_type } => {
                let substituted_params =
                    params.iter().map(|p| self.substitute_type(p, type_map)).collect();

                let substituted_return = Box::new(self.substitute_type(return_type, type_map));

                Type::Function { params: substituted_params, return_type: substituted_return }
            }

            _ => ty.clone(),
        }
    }

    pub(crate) fn hash_type(&self, ty: &Type, hasher: &mut DefaultHasher) {
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

    pub(crate) fn get_monomorphized_function_name(&self, mono_id: MonomorphizationId) -> String {
        if let Some(entry) = self.mono_table.entries.get(&mono_id) {
            let original_symbol = self.symbol_table.get_symbol_unchecked(&entry.original_id);
            if let SymbolKind::Function { signature, .. } = &original_symbol.kind {
                let type_arguments: Vec<Type> = signature
                    .generics
                    .iter()
                    .filter_map(|param| entry.concrete_types.get(&param.name))
                    .cloned()
                    .collect();

                return self.mangle_monomorphized_function(entry.original_id, &type_arguments);
            }
        }

        panic!("Monomorphization ID {:?} not found in mono_id_map", mono_id)
    }
}
