use crate::{
  codegen::{Codegen, Gen},
  ir::{
    IrBlock, IrEnum, IrExpr, IrExprKind, IrField, IrFunction, IrItem, IrItemKind, IrModule,
    IrParam, IrPattern, IrStmt, IrStruct, IrTypeExtension, IrVariantData,
  },
};
use itertools::Itertools as _;
use std::path::PathBuf;
use zirael_parser::{BinaryOp, Literal, MainFunction, SymbolRelationNode, Type, UnaryOp};
use zirael_utils::prelude::{CompilationInfo, debug, resolve};

pub fn run_codegen(
  modules: &mut Vec<IrModule>,
  info: &CompilationInfo,
  mut order: Vec<SymbolRelationNode>,
  used_externals: Vec<String>,
  main_fn: &Option<MainFunction>,
) -> anyhow::Result<PathBuf> {
  let header = &mut Codegen::new();
  let implementation = &mut Codegen::new();
  let header_file = PathBuf::from(info.name.clone()).with_extension("h");

  header.writeln("#include <stdlib.h>");
  header.writeln("#include <uchar.h>");
  header.writeln("#include <stdint.h>");
  header.writeln("#include <stdbool.h>");

  let mut module_items = modules.iter().flat_map(|m| &m.items).cloned().collect::<Vec<_>>();
  let mono_items = modules.iter().flat_map(|m| &m.mono_items).cloned().collect::<Vec<_>>();

  let _c_main_function = module_items.iter().find(|item| item.name == "main");

  if order.is_empty() {
    order = module_items.iter().map(|i| SymbolRelationNode::Symbol(i.sym_id)).collect_vec();
  }

  for module_item in &mut module_items {
    if let IrItemKind::Enum(ref mut enum_data) = module_item.kind {
      let mut mono_variants = vec![];
      let mut remove = vec![];

      for (i, variant) in enum_data.variants.iter().enumerate() {
        let variant_mono_items: Vec<_> = mono_items
          .iter()
          .filter_map(|m| {
            if let IrItemKind::EnumVariant(ref ir_variant) = m.kind {
              if variant.symbol_id == ir_variant.symbol_id {
                Some(ir_variant.clone())
              } else {
                None
              }
            } else {
              None
            }
          })
          .collect();

        if !variant_mono_items.is_empty() {
          mono_variants.extend(variant_mono_items);
          remove.push(i);
        }
      }

      if !mono_variants.is_empty() && !remove.is_empty() {
        remove.sort_by(|a, b| b.cmp(a));
        for i in remove {
          enum_data.variants.remove(i);
        }
        enum_data.variants.extend(mono_variants);
      }
    }
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
          debug!("skipping symbol with id: {node:?}");
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

  if let Some(func) = main_fn
    && let MainFunction::Mangled(mangled) = func
  {
    implementation.writeln("int main() {");
    implementation.indent();
    implementation.writeln(&format!("{mangled}();"));
    implementation.writeln("return 0;");
    implementation.dedent();
    implementation.writeln("}");
  }

  let output_file = info.write_to.join(info.name.clone()).with_extension("c");
  fs_err::create_dir_all(&info.write_to)?;
  fs_err::write(info.write_to.join(header_file), header.content.buffer())?;
  fs_err::write(output_file.clone(), implementation.content.buffer())?;

  Ok(output_file)
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

const STANDARD_C_FUNCTIONS: &[&str] = &[
  // <stdlib.h>
  "malloc", "free", "calloc", "realloc", "exit", "abort", "atoi", "atol", "atof", "rand", "srand",
  // <string.h>
  "strlen", "strcpy", "strncpy", "strcat", "strncat", "strcmp", "strncmp", "strchr", "strrchr",
  "memcpy", "memmove", "memset", "memcmp", // <stdio.h>
  "printf", "scanf", "puts", "gets", "fopen", "fclose", "fread", "fwrite", "fgets", "fputs",
  "fprintf", "fscanf", "sprintf", "sscanf", // <math.h>
  "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "sqrt", "pow", "exp", "log",
  "log10", // <ctype.h>
  "isalpha", "isdigit", "isalnum", "isspace", "isupper", "islower", "toupper", "tolower",
];

fn is_standard_c_function(name: &str) -> bool {
  STANDARD_C_FUNCTIONS.contains(&name)
}

impl Gen for IrItem {
  fn generate_header(&self, cg: &mut Codegen) {
    match &self.kind {
      IrItemKind::Function(func) => func.generate_header(cg),
      IrItemKind::Struct(ir) => ir.generate_header(cg),
      IrItemKind::TypeExtension(ty) => ty.generate_header(cg),
      IrItemKind::Enum(en) => en.generate_header(cg),
      IrItemKind::EnumVariant(_) => {}
    }
  }

  fn generate(&self, p: &mut Codegen) {
    match &self.kind {
      IrItemKind::Function(func) => func.generate(p),
      IrItemKind::Struct(ir) => ir.generate(p),
      IrItemKind::TypeExtension(ty) => ty.generate(p),
      IrItemKind::Enum(en) => en.generate(p),
      IrItemKind::EnumVariant(_i) => {}
    }
  }
}

impl Gen for IrEnum {
  fn generate_header(&self, cg: &mut Codegen) {
    let base_name = self.name.as_str();

    cg.writeln("typedef enum {");
    cg.indent();
    for variant in &self.variants {
      cg.writeln(&format!("{base_name}_{},", variant.name.as_str()));
    }
    cg.dedent();
    cg.writeln(&format!("}} {base_name}_tags;"));
    cg.newline();

    cg.writeln("typedef struct {");
    cg.indent();
    cg.writeln(&format!("enum {base_name}_tags tag;"));
    cg.writeln("union {");
    cg.indent();

    for variant in &self.variants {
      let variant_name = format!("{}_{}", base_name, variant.name.as_str());
      match &variant.data {
        IrVariantData::Struct(fields) => {
          cg.writeln("struct {");
          cg.indent();
          for field in fields {
            field.generate(cg);
          }
          cg.dedent();
          cg.writeln(&format!("}} {variant_name};"));
        }
        IrVariantData::Unit => {
          cg.writeln(&format!("struct {{}} {variant_name};"));
        }
      }
    }
    cg.dedent();
    cg.writeln("} data;");
    cg.dedent();
    cg.writeln(&format!("}} {base_name};"));
    cg.newline();
  }

  fn generate(&self, cg: &mut Codegen) {
    let base_name = self.name.as_str();

    for variant in &self.variants {
      cg.write_indented(&format!("{} {}_constructor", base_name, variant.name.as_str()));

      match &variant.data {
        IrVariantData::Unit => cg.writeln("() {"),
        IrVariantData::Struct(fields) => {
          cg.write("(");
          for (i, field) in fields.iter().enumerate() {
            field.ty.generate(cg);
            cg.write(" ");
            cg.write(&field.name);
            if i != fields.len() - 1 {
              cg.write(", ");
            }
          }
          cg.writeln(") {");
        }
      }

      cg.indent();

      let variant_name = format!("{}_{}", base_name, variant.name.as_str());
      cg.writeln(&format!("{base_name} result;"));
      cg.writeln(&format!("result.tag = {variant_name};"));

      match &variant.data {
        IrVariantData::Unit => {}
        IrVariantData::Struct(fields) => {
          for field in fields {
            cg.writeln(&format!("result.data.{}.{} = {};", variant_name, field.name, field.name));
          }
        }
      }

      cg.writeln("return result;");
      cg.dedent();
      cg.writeln("}");
      cg.newline();
    }
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
    cg.writeln(&format!("struct {} {{", self.name));
    cg.indent();

    for field in &self.fields {
      field.generate(cg);
    }

    cg.dedent();
    cg.writeln("};");
    cg.newline();
  }

  fn generate(&self, _cg: &mut Codegen) {}
}

impl Gen for IrField {
  fn generate(&self, cg: &mut Codegen) {
    cg.write_indented("");
    self.ty.generate(cg);
    cg.write(" ");
    cg.write(&self.name);
    cg.write(";\n");
  }
}

impl Gen for IrFunction {
  fn generate_header(&self, cg: &mut Codegen) {
    if let Some(extern_) = &self.extern_ {
      if !is_standard_c_function(&extern_.original_name) {
        function_signature(self, &extern_.original_name, cg);
        cg.writeln(";");
        cg.newline();
      }
    } else {
      function_signature(self, self.name.as_str(), cg);
      cg.writeln(";");
      cg.newline();
    }
  }

  fn generate(&self, p: &mut Codegen) {
    function_signature(self, self.name.as_str(), p);

    if let Some(extern_) = &self.extern_ {
      // todo: handle abi
      p.write("{");
      p.newline();
      p.indent();

      p.write_indented(&format!("return {}(", &extern_.original_name));
      for (i, arg) in self.parameters.iter().enumerate() {
        p.write(arg.name.as_str());
        if i != self.parameters.len() - 1 {
          p.write(", ");
        }
      }
      p.write(");\n");

      p.dedent();
      p.write("}; ");
      p.newline();
    } else {
      if let Some(body) = &self.body {
        if body.stmts.is_empty() {
          p.writeln(";");
          return;
        }

        p.write(" ");
        body.generate(p);
      } else {
        p.writeln(";");
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

        match &expr.kind {
          IrExprKind::If { .. } | IrExprKind::Block(_) => {
            p.write("\n");
          }
          _ => {
            p.write(";\n");
          }
        }
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
            if s.contains('.') { format!("{s}f") } else { format!("{s}.0f") }
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
          p.write(", ");
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
      IrExprKind::Ternary(condition, true_expr, false_expr) => {
        p.write("(");
        condition.generate(p);
        p.write(") ? ");
        true_expr.generate(p);
        p.write(" : ");
        false_expr.generate(p);
      }
      IrExprKind::If { condition, then_branch, else_branch } => {
        p.write("if (");
        condition.generate(p);
        p.write(")");

        match &then_branch.kind {
          IrExprKind::Block(block) => {
            p.write(" {\n");
            p.indent();
            for stmt in &block.stmts {
              stmt.generate(p);
            }
            p.dedent();
            p.write_indented("}");
          }
          _ => {
            p.write(" {\n");
            p.indent();
            p.write_indented("");
            then_branch.generate(p);
            p.writeln(";");
            p.dedent();
            p.write_indented("}");
          }
        }

        if let Some(else_branch) = else_branch {
          match &else_branch.kind {
            IrExprKind::Block(block) => {
              p.write(" else {\n");
              p.indent();
              for stmt in &block.stmts {
                stmt.generate(p);
              }
              p.dedent();
              p.write_indented("}");
            }
            IrExprKind::If { .. } => {
              p.write(" else ");
              else_branch.generate(p);
            }
            _ => {
              p.write(" else {\n");
              p.indent();
              p.write_indented("");
              else_branch.generate(p);
              p.writeln(";");
              p.dedent();
              p.write_indented("}");
            }
          }
        }
      }
      IrExprKind::Match { scrutinee, arms } => {
        p.write("({\n");
        p.indent();

        p.write_indented("");
        scrutinee.ty.generate(p);
        p.write(" scrutinee_value =");

        scrutinee.generate(p);
        p.write(";\n");

        p.write_indented("");
        arms[0].body.ty.generate(p);

        p.write(" match_result;\n");

        for (i, arm) in arms.iter().enumerate() {
          if i == 0 {
            p.write_indented("if (");
          } else {
            p.write_indented("} else if (");
          }

          match &arm.pattern {
            IrPattern::Wildcard => {
              p.write("1");
            }
            IrPattern::Variable(_) => {
              p.write("1");
            }
            IrPattern::Literal(literal) => {
              p.write("scrutinee_value == ");
              match literal {
                Literal::Integer(val) => p.write(&val.to_string()),
                Literal::Float(val) => p.write(&format!("{val}f")),
                Literal::Char(val) => p.write(&format!("'{val}'")),
                Literal::String(val) => {
                  p.write(&format!("strcmp(scrutinee_value, \"{val}\") == 0"));
                }
                Literal::Bool(val) => p.write(if *val { "true" } else { "false" }),
              }
            }
            IrPattern::EnumVariant { tag_name, .. } => {
              p.write("scrutinee_value.tag == ");
              p.write(tag_name);
            }
          }

          p.writeln(") {");
          p.indent();

          match &arm.pattern {
            IrPattern::Variable(var_name) => {
              p.write_indented(&format!("auto {var_name} = scrutinee_value;\n"));
            }
            IrPattern::EnumVariant { tag_name, bindings } => {
              for (ty, field_name, var_name) in bindings {
                p.write_indented("");
                ty.generate(p);
                p.write(&format!(" {var_name} = scrutinee_value.data.{tag_name}.{field_name};\n"));
              }
            }
            _ => {}
          }

          p.write_indented("match_result = ");
          match &arm.body.kind {
            IrExprKind::Block(block) => {
              p.write("({\n");
              p.indent();
              for (idx, stmt) in block.stmts.iter().enumerate() {
                match stmt {
                  IrStmt::Return(Some(expr)) => {
                    p.write_indented("");
                    expr.generate(p);
                    p.write(";\n");
                    break;
                  }
                  IrStmt::Return(None) => {
                    p.write_indented("/* void return */\n");
                    break;
                  }
                  IrStmt::Expr(expr) if idx == block.stmts.len() - 1 => {
                    p.write_indented("");
                    expr.generate(p);
                    p.write(";\n");
                  }
                  _ => {
                    stmt.generate(p);
                  }
                }
              }
              p.dedent();
              p.write_indented("})");
            }
            _ => {
              arm.body.generate(p);
            }
          }
          p.write(";\n");

          p.dedent();
        }

        p.writeln("}");

        p.write_indented("match_result;\n");
        p.dedent();
        p.write_indented("})");
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
      Self::Int => p.write("int64_t"),
      Self::Uint => p.write("uint64_t"),
      Self::Float => p.write("double"),
      Self::Void => p.write("void"),
      Self::Char => p.write("char"),
      Self::String => p.write("char*"),
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
