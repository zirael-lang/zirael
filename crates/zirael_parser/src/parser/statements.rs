use crate::parser::Parser;
use crate::parser::errors::AllVarsInitialized;
use crate::{Block, ExprStmt, NodeId, Statement, TokenType, VarDecl};

impl Parser<'_> {
  pub fn parse_block(&mut self) -> Block {
    let span = self.peek().span;
    self.expect(TokenType::LeftBrace, "to open block");
    let mut statements = vec![];

    loop {
      if let Some(stmt) = self.parse_statement() {
        statements.push(stmt);
      } else {
        if !self.check(&TokenType::Semicolon)
          && !self.check(&TokenType::RightBrace)
          && !self.is_at_end()
        {
          self.advance();
        }
      }

      if self.check(&TokenType::Semicolon) {
        self.eat_semis();
        if self.check(&TokenType::RightBrace) {
          break;
        }
      } else if self.check(&TokenType::RightBrace) {
        break;
      } else if self.is_at_end() {
        break;
      }
    }

    self.expect(TokenType::RightBrace, "to close block");

    Block {
      id: NodeId::new(),
      span: self.span_from(span),
      statements,
    }
  }

  pub fn stmt_safe_boundary(&mut self) {
    self.advance_until_one_of(&[TokenType::Semicolon]);
    if self.check(&TokenType::Semicolon) {
      self.eat_semis();
    }
  }

  pub fn parse_statement(&mut self) -> Option<Statement> {
    use TokenType::*;
    let span = self.peek().span;

    match self.peek().kind {
      // in this context const just means the value isn't mutable
      Var | Const => {
        self.advance();
        let is_mut = self.previous().kind == Var;
        let name = self.parse_identifier();
        let ty = if self.check(&Colon) {
          self.advance();
          Some(self.parse_type())
        } else {
          None
        };

        if !self.check(&Assign) {
          self.emit(AllVarsInitialized {
            span: self.span_from(span),
          });
          self.stmt_safe_boundary();
          return None;
        }
        let value = self.parse_expr();

        Some(Statement::VarDecl(VarDecl {
          span: self.span_from(span),
          id: NodeId::new(),
          name,
          value,
          is_mut,
          ty,
        }))
      }
      _ => {
        let expr = self.parse_expr();

        let has_semicolon = self.check(&Semicolon);
        if has_semicolon {
          self.eat_semis()
        }

        Some(Statement::Expr(ExprStmt {
          id: NodeId::new(),
          expr,
          span: self.span_from(span),
          has_semicolon,
        }))
      }
    }
  }
}
