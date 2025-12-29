use crate::ast::{
  Block, ConstDecl, ExprStmt, ForStmt, LoopStmt, ReturnStmt, Statement, VarDecl, WhileStmt,
  NodeId,
};
use crate::lexer::TokenType;
use crate::parser::{ParseResult, Parser};
use zirael_utils::prelude::Span;

impl Parser {
  /// Parse a statement
  pub fn parse_statement(&mut self) -> ParseResult<Statement> {
    let start = self.current_span();

    // Variable declaration
    if self.eat(TokenType::Var) {
      return self.parse_var_decl(start).map(Statement::VarDecl);
    }

    // Const declaration
    if self.eat(TokenType::Const) {
      return self.parse_const_decl(start).map(Statement::ConstDecl);
    }

    // For loop
    if self.check(&TokenType::For) {
      return self.parse_for_stmt().map(Statement::For);
    }

    // While loop
    if self.check(&TokenType::While) {
      return self.parse_while_stmt().map(Statement::While);
    }

    // Loop
    if self.check(&TokenType::Loop) {
      return self.parse_loop_stmt().map(Statement::Loop);
    }

    // Return statement
    if self.check(&TokenType::Return) {
      return self.parse_return_stmt().map(Statement::Return);
    }

    // Block statement
    if self.check(&TokenType::LeftBrace) {
      return self.parse_block().map(Statement::Block);
    }

    // Expression statement
    self.parse_expr_stmt().map(Statement::Expr)
  }

  fn parse_var_decl(&mut self, start: Span) -> ParseResult<VarDecl> {
    let is_mut = self.eat(TokenType::Mut);
    let name = self.parse_identifier()?;

    let ty = if self.eat(TokenType::Colon) { Some(self.parse_type()?) } else { None };

    self.expect(TokenType::Assign, "in variable declaration")?;
    let value = self.parse_expr()?;
    self.expect(TokenType::Semicolon, "after variable declaration")?;

    Ok(VarDecl { id: NodeId::new(), is_mut, name, ty, value, span: self.span_from(start) })
  }

  fn parse_const_decl(&mut self, start: Span) -> ParseResult<ConstDecl> {
    let name = self.parse_identifier()?;

    let ty = if self.eat(TokenType::Colon) { Some(self.parse_type()?) } else { None };

    self.expect(TokenType::Assign, "in const declaration")?;
    let value = self.parse_expr()?;
    self.expect(TokenType::Semicolon, "after const declaration")?;

    Ok(ConstDecl { id: NodeId::new(), name, ty, value, span: self.span_from(start) })
  }

  fn parse_for_stmt(&mut self) -> ParseResult<ForStmt> {
    let start = self.expect(TokenType::For, "at start of for loop")?.span;
    let binding = self.parse_identifier()?;
    self.expect(TokenType::In, "in for loop")?;
    let iterator = self.parse_expr()?;
    let body = self.parse_block()?;

    Ok(ForStmt { id: NodeId::new(), binding, iterator, body, span: self.span_from(start) })
  }

  fn parse_while_stmt(&mut self) -> ParseResult<WhileStmt> {
    let start = self.expect(TokenType::While, "at start of while loop")?.span;
    let condition = self.parse_expr()?;
    let body = self.parse_block()?;

    Ok(WhileStmt { id: NodeId::new(), condition, body, span: self.span_from(start) })
  }

  fn parse_loop_stmt(&mut self) -> ParseResult<LoopStmt> {
    let start = self.expect(TokenType::Loop, "at start of loop")?.span;
    let body = self.parse_block()?;

    Ok(LoopStmt { id: NodeId::new(), body, span: self.span_from(start) })
  }

  fn parse_return_stmt(&mut self) -> ParseResult<ReturnStmt> {
    let start = self.expect(TokenType::Return, "at start of return")?.span;

    let value = if self.check(&TokenType::Semicolon) { None } else { Some(self.parse_expr()?) };

    self.expect(TokenType::Semicolon, "after return statement")?;

    Ok(ReturnStmt { id: NodeId::new(), value, span: self.span_from(start) })
  }

  fn parse_expr_stmt(&mut self) -> ParseResult<ExprStmt> {
    let start = self.current_span();
    let expr = self.parse_expr()?;

    // Expression statements can optionally end with semicolon
    let has_semicolon = self.eat(TokenType::Semicolon);

    Ok(ExprStmt { id: NodeId::new(), expr, has_semicolon, span: self.span_from(start) })
  }

  pub fn parse_block(&mut self) -> ParseResult<Block> {
    let start = self.expect(TokenType::LeftBrace, "at start of block")?.span;

    let mut statements = Vec::new();

    while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
      match self.parse_statement() {
        Ok(stmt) => statements.push(stmt),
        Err(e) => {
          self.report_error(e);
          self.synchronize();
        }
      }
    }

    self.expect(TokenType::RightBrace, "after block")?;

    Ok(Block { id: NodeId::new(), statements, span: self.span_from(start) })
  }
}
