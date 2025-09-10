use crate::codegen::{Codegen, Gen as _};
use crate::ir::{IrExpr, IrExprKind, IrMatchArm, IrPattern, IrStmt};
use zirael_parser::{Literal, Type};

pub fn generate_match(p: &mut Codegen, scrutinee: &IrExpr, arms: &Vec<IrMatchArm>) {
  p.write("({\n");
  p.indent();

  let is_void_match = arms[0].body.ty == Type::Void;
  let is_never_match = arms.iter().any(|arm| arm.body.ty == Type::Never);

  if !is_void_match {
    p.write_indented("");
    scrutinee.ty.generate(p);
    p.write(" scrutinee_value = ");
    scrutinee.generate(p);
    p.write(";\n");

    p.write_indented("");
    arms[0].body.ty.generate(p);
    p.write(" match_result;\n");
  }

  for (i, arm) in arms.iter().enumerate() {
    if i == 0 {
      p.write_indented("if (");
    } else {
      p.write_indented("} else if (");
    }

    generate_scrutinee(p, &arm.pattern, is_void_match, scrutinee);

    p.writeln(") {");
    p.indent();

      if !is_void_match {
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

        if arm.body.ty == Type::Never {
          p.write_indented("");
          generate_arm_block(p, arm);
          p.write(";\n");
        } else {
          p.write_indented("match_result = ");
          generate_arm_block(p, arm);
          p.write(";\n");
        }
      } else {
        p.write_indented("");
        generate_arm_block(p, arm);
        p.write(";\n");
      }    p.dedent();
  }

  p.writeln("}");

  if !is_void_match {
    p.write_indented("match_result;\n");
  }

  p.dedent();
  p.write_indented("})");
}

fn generate_arm_block(p: &mut Codegen, arm: &IrMatchArm) {
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
}

fn generate_scrutinee(
  p: &mut Codegen,
  pattern: &IrPattern,
  is_void_match: bool,
  scrutinee: &IrExpr,
) {
  match &pattern {
    IrPattern::Wildcard => {
      p.write("1");
    }
    IrPattern::Variable(_) => {
      p.write("1");
    }
    IrPattern::Literal(literal) => {
      if is_void_match {
        p.write("(");
        scrutinee.generate(p);
        p.write(") == ");
      } else {
        p.write("scrutinee_value == ");
      }

      match literal {
        Literal::Integer(val) => p.write(&val.to_string()),
        Literal::Float(val) => p.write(&format!("{val}f")),
        Literal::Char(val) => p.write(&format!("'{val}'")),
        Literal::String(val) => {
          if is_void_match {
            p.write("strcmp((");
            scrutinee.generate(p);
            p.write(&format!("), \"{val}\") == 0"));
          } else {
            p.write(&format!("strcmp(scrutinee_value, \"{val}\") == 0"));
          }
        }
        Literal::Bool(val) => p.write(if *val { "true" } else { "false" }),
      }
    }
    IrPattern::EnumVariant { tag_name, .. } => {
      if is_void_match {
        p.write("(");
        scrutinee.generate(p);
        p.write(").tag == ");
      } else {
        p.write("scrutinee_value.tag == ");
      }
      p.write(tag_name);
    }
  }
}
