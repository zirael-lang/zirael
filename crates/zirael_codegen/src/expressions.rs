// Add these methods to your CodeGenerator implementation

use crate::codegen::Codegen;
use crate::generator::CodeGenerator;
use std::collections::HashMap;
use zirael_hir::hir::HirFunction;
use zirael_hir::hir::expr::{
  AccessKind, FieldSymbol, HirExpr, HirExprKind, HirMatchArm, HirPattern, HirStmt,
};
use zirael_parser::ty::Ty;
use zirael_parser::{BinaryOp, CallInfo, Literal, OriginalSymbolId, SymbolId};
use zirael_utils::ident_table::Identifier;

impl<'a> CodeGenerator<'a> {
  fn generate_expr(&mut self, codegen: &mut Codegen, expr: &HirExpr) -> String {
    match &expr.kind {
      HirExprKind::Literal(literal) => self.generate_literal(literal),
      HirExprKind::Symbol(symbol_id) => self.get_symbol_name(*symbol_id),
      HirExprKind::Binary { left, op, right } => {
        self.generate_binary_expr(codegen, left, op, right)
      }
      HirExprKind::Ternary { condition, true_expr, false_expr } => {
        self.generate_ternary_expr(codegen, condition, true_expr, false_expr)
      }
      HirExprKind::Unary { op, operand } => self.generate_unary_expr(codegen, op, operand),
      HirExprKind::Block(stmts) => self.generate_block(codegen, stmts),
      HirExprKind::Assign { lhs, rhs } => self.generate_assign_expr(codegen, lhs, rhs),
      HirExprKind::Call { callee, args, call_info } => {
        self.generate_call_expr(codegen, callee, args, call_info)
      }
      HirExprKind::StructInit { name, fields, call_info } => {
        self.generate_struct_init_expr(codegen, name, fields, call_info)
      }
      HirExprKind::FieldAccess { base_field, main_access, fields } => {
        self.generate_field_access_expr(codegen, base_field, main_access, fields)
      }
      HirExprKind::IndexAccess { object, index } => {
        self.generate_index_access_expr(codegen, object, index)
      }
      HirExprKind::Match { scrutinee, arms } => self.generate_match_expr(codegen, scrutinee, arms),
      HirExprKind::If { condition, then_branch, else_branch } => {
        self.generate_if_expr(codegen, condition, then_branch, else_branch.as_ref())
      }
      HirExprKind::Error => "/* ERROR */".to_string(),
    }
  }

  fn get_symbol_name(&self, symbol_id: SymbolId) -> String {
    self
      .get_mangled_name(&OriginalSymbolId::Symbol(symbol_id))
      .unwrap_or_else(|| format!("unknown_symbol"))
  }

  fn generate_binary_expr(
    &mut self,
    codegen: &mut Codegen,
    left: &HirExpr,
    op: &BinaryOp,
    right: &HirExpr,
  ) -> String {
    let left_code = self.generate_expr(codegen, left);
    let right_code = self.generate_expr(codegen, right);
    let op_str = self.binary_op_to_c(op);
    format!("({} {} {})", left_code, op_str, right_code)
  }

  fn generate_ternary_expr(
    &mut self,
    codegen: &mut Codegen,
    condition: &HirExpr,
    true_expr: &HirExpr,
    false_expr: &HirExpr,
  ) -> String {
    let cond_code = self.generate_expr(codegen, condition);
    let true_code = self.generate_expr(codegen, true_expr);
    let false_code = self.generate_expr(codegen, false_expr);
    format!("({} ? {} : {})", cond_code, true_code, false_code)
  }

  fn generate_unary_expr(
    &mut self,
    codegen: &mut Codegen,
    op: &zirael_parser::UnaryOp,
    operand: &HirExpr,
  ) -> String {
    let operand_code = self.generate_expr(codegen, operand);
    let op_str = self.unary_op_to_c(op);
    format!("({}{})", op_str, operand_code)
  }

  fn generate_assign_expr(
    &mut self,
    codegen: &mut Codegen,
    lhs: &HirExpr,
    rhs: &HirExpr,
  ) -> String {
    let lhs_code = self.generate_expr(codegen, lhs);
    let rhs_code = self.generate_expr(codegen, rhs);
    format!("({} = {})", lhs_code, rhs_code)
  }

  fn get_from_call_site(
    &mut self,
    callee: &HirExpr,
    call_info: &Option<CallInfo>,
    codegen: &mut Codegen,
  ) -> String {
    let id = if let Some(call_info) = call_info {
      if let Some(mono_id) = call_info.monomorphized_id {
        OriginalSymbolId::Monomorphization(mono_id)
      } else {
        OriginalSymbolId::Symbol(call_info.original_symbol)
      }
    } else {
      if let HirExprKind::Symbol(symbol_id) = &callee.kind {
        OriginalSymbolId::Symbol(*symbol_id)
      } else {
        return self.generate_expr(codegen, callee);
      }
    };

    if let Some(name) = self.get_mangled_name(&id) {
      return name;
    }

    self.generate_expr(codegen, callee)
  }

  fn generate_call_expr(
    &mut self,
    codegen: &mut Codegen,
    callee: &HirExpr,
    args: &[HirExpr],
    call_info: &Option<CallInfo>,
  ) -> String {
    let callee_code = self.get_from_call_site(callee, call_info, codegen);

    let args_code: Vec<String> = args.iter().map(|arg| self.generate_expr(codegen, arg)).collect();
    format!("{}({})", callee_code, args_code.join(", "))
  }

  fn generate_struct_init_expr(
    &mut self,
    codegen: &mut Codegen,
    name: &HirExpr,
    fields: &HashMap<Identifier, HirExpr>,
    call_info: &Option<CallInfo>,
  ) -> String {
    let struct_name = self.get_from_call_site(name, call_info, codegen);
    let mut field_inits = Vec::new();

    for (field_name, field_expr) in fields {
      let field_code = self.generate_expr(codegen, field_expr);
      field_inits.push(format!(".{} = {}", field_name, field_code));
    }

    format!("({}){{ {} }}", struct_name, field_inits.join(", "))
  }

  fn generate_field_access_expr(
    &mut self,
    codegen: &mut Codegen,
    base_field: &FieldSymbol,
    main_access: &AccessKind,
    fields: &[(Identifier, AccessKind)],
  ) -> String {
    let mut result = match base_field {
      FieldSymbol::Symbol(symbol_id) => self.get_symbol_name(*symbol_id),
      FieldSymbol::Expr(expr) => self.generate_expr(codegen, expr),
    };

    result = format!("{}{}", result, main_access);

    for (field_name, access_kind) in fields {
      result = format!("{}{}{}", result, field_name, access_kind);
    }

    result
  }

  fn generate_index_access_expr(
    &mut self,
    codegen: &mut Codegen,
    object: &HirExpr,
    index: &HirExpr,
  ) -> String {
    let object_code = self.generate_expr(codegen, object);
    let index_code = self.generate_expr(codegen, index);
    format!("{}[{}]", object_code, index_code)
  }

  fn generate_match_expr(
    &mut self,
    codegen: &mut Codegen,
    scrutinee: &HirExpr,
    arms: &[HirMatchArm],
  ) -> String {
    let scrutinee_code = self.generate_expr(codegen, scrutinee);
    let mut result = String::new();
    result.push_str("({ ");

    for (i, arm) in arms.iter().enumerate() {
      if i == 0 {
        result.push_str("if (");
      } else {
        result.push_str(" else if (");
      }

      let pattern_condition = self.generate_pattern_condition(&scrutinee_code, &arm.pattern);
      result.push_str(&pattern_condition);
      result.push_str(") { ");

      let body_code = self.generate_expr(codegen, &arm.body);
      result.push_str(&body_code);
      result.push_str("; }");
    }

    result.push_str(" })");
    result
  }

  fn generate_if_expr(
    &mut self,
    codegen: &mut Codegen,
    condition: &HirExpr,
    then_branch: &HirExpr,
    else_branch: Option<&Box<HirExpr>>,
  ) -> String {
    let cond_code = self.generate_expr(codegen, condition);
    let then_code = self.generate_expr(codegen, then_branch);

    if let Some(else_expr) = else_branch {
      let else_code = self.generate_expr(codegen, else_expr);
      format!("({} ? {} : {})", cond_code, then_code, else_code)
    } else {
      format!("({{ if ({}) {{ {}; }} }})", cond_code, then_code)
    }
  }

  fn generate_stmt(&mut self, codegen: &mut Codegen, stmt: &HirStmt) {
    match stmt {
      HirStmt::Expr(expr) => {
        let expr_code = self.generate_expr(codegen, expr);
        codegen.line(&format!("{};", expr_code));
      }
      HirStmt::Var { symbol_id, init } => {
        let var_name = self.get_symbol_name(*symbol_id);
        let init_code = self.generate_expr(codegen, init);
        let var_type = self.resolve_type_id(&init.ty);
        codegen.line(&format!("{} {} = {};", var_type, var_name, init_code));
      }
      HirStmt::Return(expr_opt) => {
        if let Some(expr) = expr_opt {
          let expr_code = self.generate_expr(codegen, expr);
          codegen.line(&format!("return {};", expr_code));
        } else {
          codegen.line("return;");
        }
      }
    }
  }

  fn generate_block(&mut self, codegen: &mut Codegen, stmts: &[HirStmt]) -> String {
    if stmts.is_empty() {
      return "/* empty block */".to_string();
    }

    let mut block_code = String::new();
    block_code.push_str("({ ");

    for (i, stmt) in stmts.iter().enumerate() {
      if i == stmts.len() - 1 {
        match stmt {
          HirStmt::Expr(expr) => {
            let expr_code = self.generate_expr(codegen, expr);
            block_code.push_str(&expr_code);
          }
          _ => {
            let mut temp_codegen = Codegen::new();
            self.generate_stmt(&mut temp_codegen, stmt);
            block_code.push_str(&temp_codegen.to_string().trim());
            block_code.push_str(" (void)0");
          }
        }
      } else {
        let mut temp_codegen = Codegen::new();
        self.generate_stmt(&mut temp_codegen, stmt);
        block_code.push_str(&temp_codegen.to_string().trim());
        block_code.push(' ');
      }
    }

    block_code.push_str(" })");
    block_code
  }

  fn generate_match(
    &mut self,
    codegen: &mut Codegen,
    scrutinee: &HirExpr,
    arms: &[HirMatchArm],
  ) -> String {
    let scrutinee_code = self.generate_expr(codegen, scrutinee);

    let mut result = String::new();
    result.push_str("({ ");

    for (i, arm) in arms.iter().enumerate() {
      if i == 0 {
        result.push_str("if (");
      } else {
        result.push_str(" else if (");
      }

      let pattern_condition = self.generate_pattern_condition(&scrutinee_code, &arm.pattern);
      result.push_str(&pattern_condition);
      result.push_str(") { ");

      let body_code = self.generate_expr(codegen, &arm.body);
      result.push_str(&body_code);
      result.push_str("; }");
    }

    result.push_str(" })");
    result
  }

  fn generate_pattern_condition(&mut self, scrutinee: &str, pattern: &HirPattern) -> String {
    match pattern {
      HirPattern::Wildcard => "true".to_string(),

      HirPattern::Identifier(_) => "true".to_string(),

      HirPattern::Literal(literal) => {
        let literal_code = self.generate_literal(literal);
        format!("{} == {}", scrutinee, literal_code)
      }

      HirPattern::EnumVariant { symbol_id, fields: _ } => {
        let variant_name = self
          .get_mangled_name(&OriginalSymbolId::Symbol(*symbol_id))
          .unwrap_or_else(|| format!("variant_{}", symbol_id.index()));
        format!("{} == {}", scrutinee, variant_name)
      }

      HirPattern::Struct { symbol_id: _, fields: _ } => {
        "true /* TODO: struct pattern */".to_string()
      }
    }
  }

  fn generate_literal(&self, literal: &Literal) -> String {
    match literal {
      Literal::Bool(b) => b.to_string(),
      Literal::Integer(i) => i.to_string(),
      Literal::Float(f) => f.to_string(),
      Literal::Char(c) => format!("'{}'", c.escape_default()),
      Literal::String(s) => format!("\"{}\"", s.escape_default()),
    }
  }

  fn binary_op_to_c(&self, op: &BinaryOp) -> &'static str {
    match op {
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
    }
  }

  fn unary_op_to_c(&self, op: &zirael_parser::UnaryOp) -> &'static str {
    use zirael_parser::UnaryOp;
    match op {
      UnaryOp::Not => "!",
      UnaryOp::Minus => "-",
      UnaryOp::BitwiseNot => "~",
      UnaryOp::Deref => "*",
      UnaryOp::Ref => "&",
      UnaryOp::Box => todo!(),
    }
  }

  pub fn generate_function_implementation(
    &mut self,
    codegen: &mut Codegen,
    func: &HirFunction,
    original_id: &OriginalSymbolId,
  ) {
    if let Some(mangled_name) = self.get_mangled_name(original_id) {
      let impl_key = format!("{}_impl", mangled_name);
      if !self.generated_symbols.insert(impl_key) {
        return;
      }

      let return_type = self.resolve_type_id(&func.signature.return_type);
      let params = self.build_parameter_list(&func.signature.parameters);

      codegen.line(&format!("{} {}({}) {{", return_type, mangled_name, params));
      codegen.indent();

      self.generate_function_body(&func, codegen);

      codegen.dedent();
      codegen.line("}");
      codegen.empty_line();
    }
  }

  fn generate_function_body(&mut self, func: &HirFunction, codegen: &mut Codegen) {
    if let Some(body) = &func.body {
      match &body.root_expr.kind {
        HirExprKind::Block(stmts) => {
          for stmt in stmts {
            self.generate_stmt(codegen, stmt);
          }
        }
        _ => {
          if self.needs_return_statement(&func.signature.return_type) {
            let expr_code = self.generate_expr(codegen, &body.root_expr);
            codegen.line(&format!("return {};", expr_code));
          } else {
            let expr_code = self.generate_expr(codegen, &body.root_expr);
            codegen.line(&format!("{};", expr_code));
          }
        }
      }
    } else if func.is_extern {
      codegen.line("// External function - no implementation");
    } else {
      codegen.line("// TODO: Implement function body");
      if self.needs_return_statement(&func.signature.return_type) {
        let return_ty = self.mono_symbol_table.resolve(func.signature.return_type);
        let line = match return_ty {
          Ty::Void | Ty::Never => "",
          Ty::Bool => "return false;",
          Ty::Int | Ty::Uint => "return 0;",
          Ty::Float => "return 0.0;",
          Ty::Char => "return '\\0';",
          Ty::String => "return NULL;",
          _ => "return NULL;",
        };
        codegen.line(line);
      }
    }
  }
}
