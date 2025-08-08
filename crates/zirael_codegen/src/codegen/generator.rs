use crate::{
    codegen::{Codegen, Gen},
    ir::{IrBlock, IrExpr, IrExprKind, IrItem, IrItemKind, IrModule, IrParam, IrStmt},
};
use zirael_parser::{BinaryOp, Literal, Stmt, SymbolId, Type, UnaryOp};
use zirael_utils::prelude::warn;

pub fn run_codegen(
    modules: Vec<IrModule>,
    name: String,
    order: Vec<SymbolId>,
) -> anyhow::Result<()> {
    let cg = &mut Codegen::new();

    cg.writeln("#include <stdlib.h>");
    cg.writeln("#include <uchar.h>");
    let module_items = modules.iter().map(|m| &m.items).flatten().collect::<Vec<_>>();

    for id in order {
        let Some(item) = module_items.iter().find(|i| i.sym_id == id) else {
            warn!("invalid id in codegen order");
            continue;
        };
        item.generate(cg);
    }

    cg.write_to_file(
        dirs::cache_dir().unwrap().join("zirael-compiler").join(name).with_extension("c"),
    )
}

impl Gen for IrItem {
    fn generate(&self, p: &mut Codegen) {
        match &self.kind {
            IrItemKind::Function(func) => {
                func.return_type.generate(p);
                p.write(" ");
                p.write(self.name.as_str());
                p.write("(");
                for (i, param) in func.parameters.iter().enumerate() {
                    param.generate(p);

                    if i != func.parameters.len() - 1 {
                        p.write(", ");
                    }
                }
                p.write(")");

                if let Some(body) = &func.body {
                    p.write(" ");
                    body.generate(p);
                }
            }
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
            IrStmt::Var(name, init) => {
                p.write_indented("");
                init.ty.generate(p);
                p.write(" ");
                p.write(name);
                p.write(" = ");
                init.generate(p);
                p.write(";\n");
            }
            IrStmt::Expr(expr) => {
                p.write_indented("");
                expr.generate(p);
                p.write(";\n");
            }
            IrStmt::Return(expr) => {
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
                    Literal::Float(float) => float.to_string(),
                    Literal::Char(char) => format!("'{}'", char),
                    Literal::String(string) => format!("\"{}\"", string),
                    Literal::Bool(bool) => {
                        if *bool {
                            "true".to_string()
                        } else {
                            "false".to_string()
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
                p.write(";");
            }
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
            Type::Int => p.write("int"),
            Type::Float => p.write("float"),
            Type::Void => p.write("void"),
            Type::Char => p.write("char32_t"),
            Type::String => p.write("char32_t*"),
            Type::Bool => p.write("bool"),
            Type::Pointer(ty) | Type::Reference(ty) => {
                ty.generate(p);
                p.write("*");
            }
            _ => p.write("/* TODO */"),
        }
    }
}
