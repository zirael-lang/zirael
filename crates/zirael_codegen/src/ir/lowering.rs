use crate::ir::{
    IrBlock, IrExpr, IrExprKind, IrField, IrFunction, IrItem, IrItemKind, IrModule, IrParam,
    IrStmt, IrStruct, IrTypeExtension,
};
use itertools::Itertools;
use std::{collections::HashMap, path::PathBuf, vec};
use zirael_hir::hir::{
    HirBody, HirFunction, HirItem, HirItemKind, HirModule, HirStruct, HirTypeExtension,
    expr::{FieldSymbol, HirExpr, HirExprKind, HirStmt},
};
use zirael_parser::{
    AstId, DropStackEntry, MonomorphizationId, Scope, ScopeType, StructField, Symbol, SymbolId,
    SymbolKind, SymbolRelationNode, SymbolTable, Type, UnaryOp,
    monomorphized_symbol::MonomorphizedSymbol,
};
use zirael_type_checker::MonomorphizationTable;
use zirael_utils::prelude::{
    Colorize as _, Mode, ReportBuilder, ReportKind, Reports, SourceFileId, Sources, debug,
    default_ident, get_or_intern, resolve, warn,
};

pub fn lower_hir_to_ir<'reports>(
    hir_modules: &mut Vec<HirModule>,
    symbol_table: &SymbolTable,
    mono_table: MonomorphizationTable,
    reports: &Reports<'reports>,
    sources: &Sources,
    mode: Mode,
    root: PathBuf,
) -> Vec<IrModule> {
    let mut lowering = HirLowering::new(symbol_table, mono_table, reports, sources, mode, root);
    lowering.lower_modules(hir_modules)
}

pub struct HirLowering<'reports> {
    pub symbol_table: SymbolTable,
    pub mono_table: MonomorphizationTable,
    processed_file: Option<SourceFileId>,
    pub sources: Sources,
    pub mode: Mode,
    pub root: PathBuf,
    pub reports: Reports<'reports>,
    pub current_mono_id: Option<MonomorphizationId>,
    pub current_symbol_id: Option<SymbolId>,
    pub current_items: Vec<IrItem>,
}

impl<'reports> HirLowering<'reports> {
    pub fn new(
        symbol_table: &SymbolTable,
        mono_table: MonomorphizationTable,
        reports: &Reports<'reports>,
        sources: &Sources,
        mode: Mode,
        root: PathBuf,
    ) -> Self {
        Self {
            symbol_table: symbol_table.clone(),
            mono_table,
            processed_file: None,
            sources: sources.clone(),
            reports: reports.clone(),
            mode,
            root,
            current_mono_id: None,
            current_symbol_id: None,
            current_items: vec![],
        }
    }

    pub fn lower_modules(&mut self, lexed_modules: &mut Vec<HirModule>) -> Vec<IrModule> {
        lexed_modules.iter_mut().map(|module| self.lower_module(module)).collect::<Vec<_>>()
    }

    fn push_scope(&mut self, scope_type: ScopeType) {
        let _ = self.symbol_table.push_scope(scope_type);
    }

    fn pop_scope(&mut self) -> Vec<DropStackEntry> {
        let pop = self.symbol_table.pop_scope().unwrap();
        self.symbol_table.get_scope_unchecked(pop).drop_stack
    }

    pub fn lower_type(&mut self, ty: Type) -> Type {
        match ty {
            Type::Reference(ty) => Type::Reference(Box::new(self.lower_type(*ty))),
            Type::MonomorphizedSymbol(sym) => self.handle_monomorphized_symbol(&sym, true),
            Type::Named { name, generics } if generics.is_empty() => {
                let symbol = self.symbol_table.lookup_symbol(&name);

                if let Some(symbol) = symbol {
                    let mangled = self.mangle_symbol(symbol.id);
                    let string = if let SymbolKind::Struct { .. } = symbol.kind {
                        format!("struct {}", mangled)
                    } else {
                        mangled
                    };
                    Type::Named { name: get_or_intern(&string), generics }
                } else {
                    Type::Named { name, generics }
                }
            }
            _ => ty,
        }
    }

    fn lower_module(&mut self, lexed_module: &mut HirModule) -> IrModule {
        let mut ir_module = IrModule { items: vec![], mono_items: vec![] };

        self.push_scope(ScopeType::Module(lexed_module.id));
        self.processed_file = Some(lexed_module.id);

        for item in &mut lexed_module.items.values_mut() {
            if let Some(ir_item) = self.lower_item(item) {
                ir_module.items.push(ir_item);
            }
        }
        ir_module.items.extend(self.current_items.drain(..));
        self.pop_scope();

        if !self.mono_table.entries.is_empty() {
            self.process_monomorphization_entries(&mut ir_module);
        }

        ir_module
    }

    fn lower_item(&mut self, item: &mut HirItem) -> Option<IrItem> {
        let sym = self.symbol_table.get_symbol_unchecked(&item.symbol_id);
        let name = self.mangle_symbol(item.symbol_id);

        self.current_symbol_id = Some(item.symbol_id);
        match &mut item.kind {
            HirItemKind::Function(func) => {
                let ir_function = self.lower_function(name.clone(), func);

                Some(IrItem {
                    name,
                    kind: IrItemKind::Function(ir_function),
                    sym_id: item.symbol_id,
                    mono_id: None,
                })
            }
            HirItemKind::Struct(hir_struct) => {
                let fields = if let SymbolKind::Struct { fields, .. } = sym.kind {
                    fields.clone()
                } else {
                    unreachable!()
                };

                let ir_struct = self.lower_struct(name.clone(), hir_struct, fields);
                Some(IrItem {
                    name,
                    kind: IrItemKind::Struct(ir_struct),
                    sym_id: item.symbol_id,
                    mono_id: None,
                })
            }
            HirItemKind::TypeExtension(ext) => {
                self.lower_type_extension(ext);
                None
            }
            _ => todo!(),
        }
    }

    fn lower_type_extension(&mut self, ext: &mut HirTypeExtension) {
        self.push_scope(ScopeType::TypeExtension(ext.id));

        let ir_items = ext.methods.iter_mut().map(|func| self.lower_item(func)).collect_vec();

        for item in ir_items {
            if let Some(item) = item {
                self.current_items.push(item);
            } else {
                warn!("failed to lower method")
            }
        }

        self.pop_scope();
    }

    fn lower_struct(
        &mut self,
        name: String,
        hir_struct: &mut HirStruct,
        fields: Vec<StructField>,
    ) -> IrStruct {
        self.push_scope(ScopeType::Struct(hir_struct.id));

        for item in hir_struct.methods.iter_mut() {
            if let Some(item) = self.lower_item(item) {
                self.current_items.push(item);
            } else {
                warn!("failed to lower method")
            }
        }

        self.pop_scope();
        IrStruct {
            fields: fields
                .iter()
                .map(|field| IrField {
                    name: resolve(&field.name),
                    ty: self.lower_type(field.ty.clone()),
                })
                .collect_vec(),
            name,
        }
    }

    fn lower_function(&mut self, name: String, func: &mut HirFunction) -> IrFunction {
        self.push_scope(ScopeType::Function(func.id));
        let parameters = func
            .signature
            .parameters
            .iter()
            .map(|p| IrParam {
                name: self.mangle_symbol(p.symbol_id),
                ty: self.lower_type(p.ty.clone()),
            })
            .collect::<Vec<_>>();

        let mut body = self.lower_body(func.body.clone());
        let entries = self.pop_scope();
        if let Some(body) = &mut body {
            self.add_drop(entries, &mut body.stmts);
        }

        IrFunction {
            name,
            parameters,
            return_type: self.lower_type(func.signature.return_type.clone()),
            body,
            is_extern: func.is_extern,
            is_const: func.is_const,
            is_async: func.is_async,
            abi: func.abi.clone(),
        }
    }

    fn lower_body(&mut self, body: Option<HirBody>) -> Option<IrBlock> {
        let body = body?;

        if let HirExprKind::Block(stmts) = body.root_expr.kind {
            Some(self.lower_block(stmts, body.root_expr.id))
        } else {
            unreachable!()
        }
    }

    fn heap_variable(&mut self, name: String, ty: Type) -> IrStmt {
        let lowered_ty = self.lower_type(ty.clone());
        IrStmt::Var(
            name,
            IrExpr::new(
                self.lower_type(Type::Pointer(Box::new(ty.clone()))),
                IrExprKind::CCall(
                    "malloc".to_owned(),
                    vec![IrExpr::new(
                        self.lower_type(Type::Int),
                        IrExprKind::CCall(
                            "sizeof".to_owned(),
                            vec![IrExpr::new(lowered_ty.clone(), IrExprKind::Type(lowered_ty))],
                        ),
                    )],
                ),
            ),
        )
    }

    fn after_heap_assigment(&mut self, name: String, expr: IrExpr) -> IrStmt {
        IrStmt::Expr(IrExpr::new(
            self.lower_type(expr.ty.clone()),
            IrExprKind::Assign(
                Box::new(IrExpr::new(
                    self.lower_type(Type::Inferred),
                    IrExprKind::Unary(
                        UnaryOp::Deref,
                        Box::new(IrExpr::new(
                            self.lower_type(expr.ty.clone()),
                            IrExprKind::Symbol(name.clone()),
                        )),
                    ),
                )),
                Box::new(expr),
            ),
        ))
    }

    fn warn(&mut self, report: ReportBuilder<'reports>) {
        self.reports.add(self.processed_file.unwrap(), report);
    }

    fn lower_block(&mut self, block: Vec<HirStmt>, id: AstId) -> IrBlock {
        self.push_scope(ScopeType::Block(id));
        let mut ir_block = vec![];
        for stmt in block {
            let ir_stmt = match stmt {
                HirStmt::Var { symbol_id, init } => {
                    let symbol = self.symbol_table.get_symbol_unchecked(&symbol_id);
                    let SymbolKind::Variable { is_heap, .. } = symbol.kind else { unreachable!() };

                    let var_name = resolve(&symbol.name);
                    if !symbol.is_used && !var_name.starts_with('_') {
                        debug!("eliminating unused variable: {var_name}");

                        self.warn(
                            ReportBuilder::builder(
                                format!("unused variable: {}", var_name.dimmed().bold()),
                                ReportKind::Warning,
                            )
                            .label("declared here", symbol.source_location.unwrap()),
                        );

                        continue;
                    }

                    let name = self.mangle_symbol(symbol_id);
                    if is_heap {
                        ir_block.push(self.heap_variable(name.clone(), init.ty.clone()));

                        let expr = if let HirExprKind::Unary { op, operand } = init.kind.clone() {
                            if op == UnaryOp::Box { *operand } else { init }
                        } else {
                            init
                        };
                        let ir_expr = self.lower_expr(expr);

                        ir_block.push(self.after_heap_assigment(name, ir_expr));
                    } else {
                        ir_block.push(IrStmt::Var(name, self.lower_expr(init)));
                    }

                    continue;
                }
                HirStmt::Return(expr) => {
                    let expr = expr.map(|expr| self.lower_expr(expr));

                    IrStmt::Return(expr)
                }
                HirStmt::Expr(expr) => IrStmt::Expr(self.lower_expr(expr)),
            };

            ir_block.push(ir_stmt);
        }

        let entries = self.pop_scope();
        self.add_drop(entries, &mut ir_block);

        IrBlock::new(ir_block)
    }

    pub(crate) fn new_relation(&mut self, node: SymbolRelationNode) {
        let referrer = if let Some(mono) = self.current_mono_id {
            SymbolRelationNode::Monomorphization(mono)
        } else if let Some(sym_id) = self.current_symbol_id {
            SymbolRelationNode::Symbol(sym_id)
        } else {
            unreachable!()
        };

        self.symbol_table.new_relation(referrer, node);
    }

    fn lower_expr(&mut self, expr: HirExpr) -> IrExpr {
        let kind = match expr.kind {
            HirExprKind::Block(stms) => IrExprKind::Block(self.lower_block(stms, expr.id)),
            HirExprKind::Literal(literal) => IrExprKind::Literal(literal),
            HirExprKind::Call { callee, args, call_info } => {
                let identifier = if let Some(call_info) = call_info {
                    if let Some(mono_id) = call_info.monomorphized_id {
                        self.new_relation(SymbolRelationNode::Monomorphization(mono_id));
                        self.get_monomorphized_name(mono_id)
                    } else {
                        self.new_relation(SymbolRelationNode::Symbol(call_info.original_symbol));
                        self.mangle_symbol(call_info.original_symbol)
                    }
                } else if let HirExprKind::Symbol(id) = callee.kind {
                    self.new_relation(SymbolRelationNode::Symbol(id));
                    self.mangle_symbol(id)
                } else {
                    unreachable!("Invalid call expression structure")
                };

                let args = args.iter().map(|arg| self.lower_expr(arg.clone())).collect::<Vec<_>>();

                IrExprKind::Call(identifier, args)
            }
            HirExprKind::StructInit { name, fields, call_info } => {
                let constructor_identifier = if let Some(call_info) = &call_info {
                    if let Some(mono_id) = call_info.monomorphized_id {
                        self.get_monomorphized_name(mono_id)
                    } else {
                        self.mangle_symbol(call_info.original_symbol)
                    }
                } else if let HirExprKind::Symbol(id) = name.kind {
                    let mangled = self.mangle_symbol(id);
                    mangled
                } else {
                    unreachable!("Invalid struct init expression structure")
                };

                let mut new_fields = HashMap::new();
                for (ident, expr) in fields {
                    new_fields.insert(resolve(&ident), self.lower_expr(expr.clone()));
                }

                IrExprKind::StructInit(constructor_identifier, new_fields)
            }
            HirExprKind::Assign { lhs, rhs } => {
                let lhs = self.lower_expr(*lhs);
                let rhs = self.lower_expr(*rhs);

                IrExprKind::Assign(Box::new(lhs), Box::new(rhs))
            }
            HirExprKind::Unary { op, operand } => {
                IrExprKind::Unary(op, Box::new(self.lower_expr(*operand)))
            }
            HirExprKind::Symbol(id) => IrExprKind::Symbol(self.mangle_symbol(id)),
            HirExprKind::Binary { left, right, op } => IrExprKind::Binary(
                Box::new(self.lower_expr(*left)),
                op,
                Box::new(self.lower_expr(*right)),
            ),
            HirExprKind::FieldAccess { base_field, main_access, fields } => {
                let mut f = vec![
                    match base_field {
                        FieldSymbol::Symbol(sym_id) => IrExpr::new(
                            self.lower_type(Type::Inferred),
                            IrExprKind::Symbol(self.mangle_symbol(sym_id)),
                        ),
                        FieldSymbol::Expr(expr) => self.lower_expr(*expr),
                    },
                    IrExpr::sym(main_access.to_string()),
                ];

                for (i, (field, access)) in fields.iter().enumerate() {
                    f.push(IrExpr::sym(resolve(&field)));

                    if i != fields.len() - 1 {
                        f.push(IrExpr::sym(access.to_string()));
                    }
                }

                IrExprKind::FieldAccess(f)
            }
            _ => IrExprKind::Symbol(String::new()),
        };

        let mut expr_type = self.lower_type(expr.ty);

        let mut fixed_kind = kind;
        if let IrExprKind::StructInit(constructor_name, fields) = &fixed_kind {
            if let Type::Named { name: struct_name, generics } = &expr_type {
                if !generics.is_empty()
                    && generics.iter().any(|g| matches!(g, Type::TypeVariable { .. }))
                {
                    for (mono_id, entry) in &self.mono_table.entries {
                        let original_symbol =
                            self.symbol_table.get_symbol_unchecked(&entry.original_id);
                        if let SymbolKind::Struct { .. } = original_symbol.kind {
                            if original_symbol.name == *struct_name {
                                let mono_name = self.get_monomorphized_name(*mono_id);
                                let new_type = Type::Named {
                                    name: get_or_intern(&format!("struct {}", mono_name)),
                                    generics: vec![],
                                };
                                expr_type = new_type;

                                fixed_kind = IrExprKind::StructInit(mono_name, fields.clone());
                                break;
                            }
                        }
                    }
                }
            }
        }

        IrExpr { ty: expr_type, kind: fixed_kind }
    }
}
