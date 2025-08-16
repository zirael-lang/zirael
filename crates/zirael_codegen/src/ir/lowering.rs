use crate::ir::{
    IrBlock, IrExpr, IrExprKind, IrFunction, IrItem, IrItemKind, IrModule, IrParam, IrStmt,
};
use std::{
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    path::PathBuf,
};
use zirael_hir::hir::{
    HirBody, HirFunction, HirItem, HirItemKind, HirModule,
    expr::{HirExpr, HirExprKind, HirStmt},
    lowering::AstLowering,
};
use zirael_parser::{
    AstId, DropStackEntry, ItemKind, LexedModule, MonomorphizationId, ScopeId, ScopeType, SymbolId,
    SymbolKind, SymbolTable, Type, UnaryOp, item::Item,
};
use zirael_type_checker::MonomorphizationTable;
use zirael_utils::prelude::{
    Colorize, Identifier, Mode, ReportBuilder, ReportKind, Reports, SourceFileId, Sources, Span,
    debug, resolve, strip_same_root, warn,
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
        }
    }

    pub fn lower_modules(&mut self, lexed_modules: &mut Vec<HirModule>) -> Vec<IrModule> {
        let mut ir_modules =
            lexed_modules.iter_mut().map(|module| self.lower_module(module)).collect::<Vec<_>>();

        ir_modules
    }

    fn push_scope(&mut self, scope_type: ScopeType) {
        self.symbol_table.push_scope(scope_type);
    }

    fn pop_scope(&mut self) -> Vec<DropStackEntry> {
        let pop = self.symbol_table.pop_scope().unwrap();
        self.symbol_table.get_scope_unchecked(pop).drop_stack
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
        self.pop_scope();

        if !self.mono_table.entries.is_empty() {
            self.process_monomorphization_entries(&mut ir_module);
        }

        ir_module
    }

    fn lower_item(&mut self, item: &mut HirItem) -> Option<IrItem> {
        let sym = self.symbol_table.get_symbol_unchecked(&item.symbol_id);

        match &mut item.kind {
            HirItemKind::Function(func) => {
                if let SymbolKind::Function { signature, .. } = &sym.kind {
                    let hir_function = self.lower_function(func);

                    Some(IrItem {
                        name: self.mangle_symbol(item.symbol_id),
                        kind: IrItemKind::Function(hir_function),
                        sym_id: item.symbol_id,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn lower_function(&mut self, func: &mut HirFunction) -> IrFunction {
        self.push_scope(ScopeType::Function(func.id));
        let parameters = func
            .signature
            .parameters
            .iter()
            .map(|p| IrParam { name: self.mangle_symbol(p.symbol_id), ty: p.ty.clone() })
            .collect::<Vec<_>>();

        let mut body = self.lower_body(func.body.clone());
        let entries = self.pop_scope();
        if let Some(body) = &mut body {
            self.add_drop(entries, &mut body.stmts);
        }

        IrFunction {
            parameters,
            return_type: func.signature.return_type.clone(),
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

    fn heap_variable(&self, name: String, ty: Type) -> IrStmt {
        IrStmt::Var(
            name,
            IrExpr::new(
                Type::Pointer(Box::new(ty.clone())),
                IrExprKind::CCall(
                    "malloc".to_string(),
                    vec![IrExpr::new(
                        Type::Int,
                        IrExprKind::CCall(
                            "sizeof".to_string(),
                            vec![IrExpr::new(ty.clone(), IrExprKind::Type(ty))],
                        ),
                    )],
                ),
            ),
        )
    }

    fn after_heap_assigment(&mut self, name: String, expr: IrExpr) -> IrStmt {
        IrStmt::Expr(IrExpr::new(
            expr.ty.clone(),
            IrExprKind::Assign(
                Box::new(IrExpr::new(
                    Type::Inferred,
                    IrExprKind::Unary(
                        UnaryOp::Deref,
                        Box::new(IrExpr::new(
                            expr.ty.clone(),
                            IrExprKind::Symbol(name.to_string()),
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
                    if !symbol.is_used && !var_name.starts_with("_") {
                        debug!("eliminating unused variable: {}", var_name);

                        self.warn(
                            ReportBuilder::builder(
                                &format!("unused variable: {}", var_name.dimmed().bold()),
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
                        ir_block.push(IrStmt::Var(name, self.lower_expr(init)))
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

    fn lower_expr(&mut self, expr: HirExpr) -> IrExpr {
        let kind = match expr.kind {
            HirExprKind::Block(stms) => IrExprKind::Block(self.lower_block(stms, expr.id)),
            HirExprKind::Literal(literal) => IrExprKind::Literal(literal),
            HirExprKind::Call { callee, args, call_info } => {
                let identifier = if let Some(call_info) = call_info {
                    if let Some(mono_id) = call_info.monomorphized_id {
                        self.get_monomorphized_function_name(mono_id)
                    } else {
                        self.mangle_symbol(call_info.original_symbol)
                    }
                } else if let HirExprKind::Symbol(id) = callee.kind {
                    self.mangle_symbol(id)
                } else {
                    unreachable!("Invalid call expression structure")
                };

                let args = args.iter().map(|arg| self.lower_expr(arg.clone())).collect::<Vec<_>>();

                IrExprKind::Call(identifier, args)
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
            _ => IrExprKind::Symbol("".to_string()),
        };

        IrExpr { ty: expr.ty, kind }
    }
}
