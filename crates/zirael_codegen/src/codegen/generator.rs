use crate::{
    codegen::{Codegen, Gen},
    ir::{
        IrBlock, IrExpr, IrExprKind, IrField, IrFunction, IrItem, IrItemKind, IrModule, IrParam,
        IrStmt, IrStruct, IrTypeExtension,
    },
};
use itertools::Itertools as _;
use std::path::PathBuf;
use zirael_parser::{
    BinaryOp, Literal, SymbolId, SymbolRelationNode, Type, UnaryOp,
    ast::monomorphized_symbol::MonomorphizedSymbol,
};
use zirael_utils::prelude::{CompilationInfo, debug, resolve};

pub fn run_codegen(
    modules: Vec<IrModule>,
    info: &CompilationInfo,
    mut order: Vec<SymbolRelationNode>,
    used_externals: Vec<String>,
) -> anyhow::Result<()> {
    let header = &mut Codegen::new();
    let implementation = &mut Codegen::new();
    let header_file = PathBuf::from(info.name.clone()).with_extension("h");

    header.writeln("#include <stdlib.h>");
    header.writeln("#include <uchar.h>");
    header.writeln("#include <stdint.h>");
    header.writeln("#include <stdbool.h>");

    let module_items = modules.iter().flat_map(|m| &m.items).collect::<Vec<_>>();
    let mono_items = modules.iter().flat_map(|m| &m.mono_items).collect::<Vec<_>>();

    if order.is_empty() {
        order = module_items.iter().map(|i| SymbolRelationNode::Symbol(i.sym_id)).collect_vec();
    }

    for node in &order {
        match node {
            SymbolRelationNode::Symbol(id) => {
                if let Some(item) = module_items.iter().find(|i| i.sym_id == *id) {
                    let has_monomorphized_versions = mono_items.iter().any(|m| m.sym_id == *id);
                    if !has_monomorphized_versions {
                        item.generate_header(header);
                    }
                }
            }
            SymbolRelationNode::Monomorphization(mono_id) => {
                if let Some(mono_item) = mono_items.iter().find(|m| m.mono_id == Some(*mono_id)) {
                    mono_item.generate_header(header);
                }
            }
        }
    }

    implementation.writeln(&format!("#include \"{}\"", header_file.display()));
    for external in used_externals {
        let file = PathBuf::from(external.clone()).with_extension("h");
        implementation.writeln(&format!("#include \"{}\"", file.display()));
    }
    implementation.writeln("");

    for node in &order {
        match node {
            SymbolRelationNode::Symbol(id) => {
                let Some(item) = module_items.iter().find(|i| i.sym_id == *id) else {
                    debug!("skipping symbol with id: {:?}", node);
                    continue;
                };

                let has_monomorphized_versions = mono_items.iter().any(|m| m.sym_id == *id);

                if !has_monomorphized_versions {
                    item.generate(implementation);
                }
            }
            SymbolRelationNode::Monomorphization(mono_id) => {
                if let Some(mono_item) = mono_items.iter().find(|m| m.mono_id == Some(*mono_id)) {
                    mono_item.generate(implementation);
                }
            }
        }
    }

    for mono_item in mono_items {
        if let Some(mono_id) = mono_item.mono_id {
            let mono_node = SymbolRelationNode::Monomorphization(mono_id);
            if !order.contains(&mono_node) {
                mono_item.generate(implementation);
            }
        }
    }

    fs_err::create_dir_all(&info.write_to)?;
    fs_err::write(info.write_to.join(header_file), header.content.buffer())?;
    fs_err::write(
        info.write_to.join(info.name.clone()).with_extension("c"),
        implementation.content.buffer(),
    )?;

    Ok(())
}

fn function_signature(func: &IrFunction, name: &str, p: &mut Codegen) {
    func.return_type.generate(p);
    p.write(" ");
    p.write(name);

    p.write("(");
    for (i, param) in func.parameters.iter().enumerate() {
        param.generate(p);
        if i != func.parameters.len() - 1 {
            p.write(", ");
        }
    }
    p.write(")");
}

impl Gen for IrItem {
    fn generate_header(&self, cg: &mut Codegen) {
        match &self.kind {
            IrItemKind::Function(func) => func.generate_header(cg),
            IrItemKind::Struct(ir) => ir.generate_header(cg),
            IrItemKind::TypeExtension(ty) => ty.generate_header(cg),
        }
    }

    fn generate(&self, p: &mut Codegen) {
        match &self.kind {
            IrItemKind::Function(func) => func.generate(p),
            IrItemKind::Struct(ir) => ir.generate(p),
            IrItemKind::TypeExtension(ty) => ty.generate(p),
        }
        p.newline();
    }
}

impl Gen for IrTypeExtension {
    fn generate_header(&self, cg: &mut Codegen) {
        for func in &self.methods {
            func.generate_header(cg);
        }
    }

    fn generate(&self, cg: &mut Codegen) {
        for func in &self.methods {
            func.generate(cg);
        }
    }
}

impl Gen for IrStruct {
    fn generate_header(&self, cg: &mut Codegen) {
        cg.writeln(&format!("struct {};", self.name));
    }

    fn generate(&self, cg: &mut Codegen) {
        cg.write(&format!("struct {} {{", self.name));
        cg.newline();
        cg.indent();

        for field in &self.fields {
            field.generate(cg);
        }

        cg.dedent();
        cg.write("};");
        cg.newline();
    }
}

impl Gen for IrField {
    fn generate(&self, cg: &mut Codegen) {
        cg.write_indented("");
        self.ty.generate(cg);
        cg.write(" ");
        cg.write(&self.name);
        cg.writeln(";");
    }
}

impl Gen for IrFunction {
    fn generate_header(&self, cg: &mut Codegen) {
        function_signature(self, self.name.as_str(), cg);
        cg.writeln(";");
        cg.newline();
    }

    fn generate(&self, p: &mut Codegen) {
        function_signature(self, self.name.as_str(), p);

        if let Some(body) = &self.body {
            if body.stmts.is_empty() {
                p.writeln(";");
                return;
            }

            p.write(" ");
            body.generate(p);
        }
        p.newline();
    }
}

impl Gen for IrBlock {
    fn generate(&self, p: &mut Codegen) {
        p.block(|c| {
            for stmt in &self.stmts {
                stmt.generate(c);
            }
        });
    }
}

impl Gen for IrStmt {
    fn generate(&self, p: &mut Codegen) {
        match &self {
            Self::Var(name, init) => {
                p.write_indented("");
                init.ty.generate(p);
                p.write(" ");
                p.write(name);
                p.write(" = ");
                init.generate(p);
                p.write(";\n");
            }
            Self::Expr(expr) => {
                if let IrExprKind::Block(block) = &expr.kind
                    && block.stmts.is_empty()
                {
                    return;
                }

                p.write_indented("");
                expr.generate(p);
                p.write(";\n");
            }
            Self::Return(expr) => {
                if let Some(expr) = expr {
                    p.write_indented("return ");
                    expr.generate(p);
                    p.write(";\n");
                } else {
                    p.write_indented("return;\n");
                }
            }
        }
    }
}

impl Gen for IrExpr {
    fn generate(&self, p: &mut Codegen) {
        match &self.kind {
            IrExprKind::Block(block) => block.generate(p),
            IrExprKind::Symbol(sym) => p.write(sym),
            IrExprKind::Literal(lit) => {
                let lit = match lit {
                    Literal::Integer(int) => int.to_string(),
                    Literal::Float(float) => {
                        let s = float.to_string();
                        if s.contains('.') { format!("{}f", s) } else { format!("{}.0f", s) }
                    }
                    Literal::Char(char) => format!("'{char}'"),
                    Literal::String(string) => format!("\"{string}\""),
                    Literal::Bool(bool) => {
                        if *bool {
                            "true".to_owned()
                        } else {
                            "false".to_owned()
                        }
                    }
                };
                p.write(lit.as_str());
            }
            IrExprKind::Call(func, args) => {
                p.write(func);
                p.write("(");
                for (i, arg) in args.iter().enumerate() {
                    arg.generate(p);
                    if i != args.len() - 1 {
                        p.write(", ");
                    }
                }
                p.write(")");
            }
            IrExprKind::StructInit(name, fields) => {
                p.write("(struct ");
                p.write(name);
                p.write(") ");
                p.write(" { ");
                for (name, expr) in fields {
                    p.write(" .");
                    p.write(name);
                    p.write(" = ");
                    expr.generate(p);
                    p.write(", ")
                }
                p.write(" }");
            }
            IrExprKind::Type(ty) => ty.generate(p),
            IrExprKind::CCall(name, args) => {
                p.write(name);
                p.write("(");
                for (i, arg) in args.iter().enumerate() {
                    arg.generate(p);
                    if i != args.len() - 1 {}
                }
                p.write(")");
            }
            IrExprKind::Assign(lhs, rhs) => {
                lhs.generate(p);
                p.write(" = ");
                rhs.generate(p);
            }
            IrExprKind::Unary(op, expr) => match op {
                UnaryOp::Minus => {
                    p.write("-");
                    expr.generate(p);
                }
                UnaryOp::Not => {
                    p.write("!");
                    expr.generate(p);
                }
                UnaryOp::BitwiseNot => {
                    p.write("~");
                    expr.generate(p);
                }
                UnaryOp::Ref => {
                    p.write("&");
                    expr.generate(p);
                }
                UnaryOp::Deref => {
                    p.write("*");
                    expr.generate(p);
                }
                UnaryOp::Box => {}
            },
            IrExprKind::Binary(lhs, op, rhs) => {
                let op = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                    BinaryOp::Rem => "%",

                    BinaryOp::Eq => "==",
                    BinaryOp::Ne => "!=",
                    BinaryOp::Lt => "<",
                    BinaryOp::Le => "<=",
                    BinaryOp::Gt => ">",
                    BinaryOp::Ge => ">=",

                    BinaryOp::And => "&&",
                    BinaryOp::Or => "||",

                    BinaryOp::BitAnd => "&",
                    BinaryOp::BitOr => "|",
                    BinaryOp::BitXor => "^",
                    BinaryOp::Shl => "<<",
                    BinaryOp::Shr => ">>",
                };

                lhs.generate(p);
                p.write(" ");
                p.write(op);
                p.write(" ");
                rhs.generate(p);
            }
            IrExprKind::FieldAccess(fields) => {
                for part in fields {
                    part.generate(p);
                }
            }
            _ => {}
        }
    }
}

impl Gen for IrParam {
    fn generate(&self, p: &mut Codegen) {
        self.ty.generate(p);
        p.write(" ");
        p.write(self.name.as_str());
    }
}

impl Gen for Type {
    fn generate(&self, p: &mut Codegen) {
        match self {
            Self::Int => p.write("int64_t"),
            Self::Uint => p.write("uint64_t"),
            Self::Float => p.write("double"),
            Self::Void => p.write("void"),
            Self::Char => p.write("char"),
            Self::String => p.write("char32_t*"),
            Self::Bool => p.write("bool"),
            Self::Pointer(ty) | Self::Reference(ty) => {
                ty.generate(p);
                p.write("*");
            }
            Self::Named { name, .. } => p.write(&resolve(name)),
            _ => p.write(&format!("/* TODO {self:?} */")),
        }
    }
}
