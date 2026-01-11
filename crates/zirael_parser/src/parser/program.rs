use crate::expressions::Expr;
use crate::items::Item;
use crate::parser::Parser;
use crate::parser::errors::{
  ConstCannotBeUninitialized, ConstExpectedFuncOrIdent,
  ConstItemsNeedTypeAnnotation, FunctionCamelCase, ModStringLit,
};
use crate::parser::parser::ITEM_TOKENS;
use crate::{
  ConstItem, FunctionItem, ItemKind, ModItem, NeverType, NodeId, Path,
  ProgramNode, TokenType, Type, UnitType, Visibility, log_parse_failure,
};
use stringcase::camel_case;
use zirael_source::span::Span;
use zirael_utils::prelude::{Identifier, debug};

impl Parser<'_> {
  pub fn parse_program(&mut self) -> Option<ProgramNode> {
    self.imports.clear();
    self.discovery_modules.clear();
    let mut items: Vec<Item> = vec![];

    while !self.is_at_end() {
      if let Some(item) = self.parse_item() {
        items.push(item);
      } else {
        self.synchronize_to_next_item();
        self.eat_semis();

        if !self.is_at_end() && !ITEM_TOKENS.contains(&self.peek().kind) {
          self.advance();
        }
      }
    }

    Some(ProgramNode {
      id: NodeId::new(),
      attributes: vec![],
      imports: self.imports.clone(),
      discover_modules: self.discovery_modules.clone(),
      items,
    })
  }

  fn check_possible_comment(&mut self) {
    while let TokenType::DocComment(comment) = &self.peek().kind {
      self.push_comment(comment.clone());
      self.advance();
    }
  }

  /// tries to find next item to start parsing from.
  fn synchronize_to_next_item(&mut self) {
    self.advance_until_one_of(ITEM_TOKENS);
  }

  fn parse_module_discovery(&mut self) -> Option<Path> {
    if let TokenType::StringLiteral(_) = &self.peek().kind {
      self.emit(ModStringLit {
        span: self.peek().span,
      });

      return None;
    }
    Some(self.parse_path())
  }

  fn parse_function(
    &mut self,
    is_const: bool,
    span_start: Span,
  ) -> Option<FunctionItem> {
    let name = self.parse_identifier();
    self.validate_function_name(name);
    let generics = self.parse_generic_parameters();

    self.expect(TokenType::LeftParen, "to open parameter list");
    let params = self.parse_function_parameters();
    self.expect(TokenType::RightParen, "to close parameter list");

    let return_type = if self.check(&TokenType::Arrow) {
      self.advance();
      if self.eat(TokenType::Not) {
        // `!` type is only allowed in the function return type
        Type::Never(NeverType {
          id: NodeId::new(),
          span: self.previous().span,
        })
      } else {
        self.parse_type()
      }
    } else {
      Type::Unit(UnitType {
        id: NodeId::new(),
        span: Span::dummy(),
      })
    };
    let body = if self.check(&TokenType::LeftBrace) {
      Some(self.parse_block())
    } else {
      None
    };

    Some(FunctionItem {
      id: NodeId::new(),
      is_const,
      name,
      generics,
      params,
      return_type,
      body,
      span: Default::default(),
    })
  }

  fn validate_function_name(&mut self, name: Identifier) {
    if name.text() != camel_case(&name.text()) {
      self.emit(FunctionCamelCase { span: *name.span() });
    }
  }

  fn parse_const(&mut self, span: Span) -> Option<ItemKind> {
    match self.peek().kind {
      TokenType::Func => Some(ItemKind::Function(log_parse_failure!(
        self.parse_function(true, span),
        "module item"
      )?)),
      TokenType::Identifier(_) => {
        let ident = self.parse_identifier();

        let colon = self.eat(TokenType::Colon);
        let ty = if !colon {
          self.emit(ConstItemsNeedTypeAnnotation {
            span: self.previous().span,
          });
          Type::Invalid
        } else {
          let ty = self.parse_type();
          if matches!(ty, Type::Invalid) {
            self.emit(ConstItemsNeedTypeAnnotation {
              span: self.previous().span,
            });
          }

          ty
        };

        let expr = if !self.eat(TokenType::Assign) {
          self.emit(ConstCannotBeUninitialized {
            span: self.peek().span,
          });
          Expr::dummy()
        } else {
          self.parse_expr()
        };

        Some(ItemKind::Const(ConstItem {
          id: NodeId::new(),
          name: ident,
          ty,
          value: expr,
          span: self.span_from(span),
        }))
      }

      _ => {
        self.emit(ConstExpectedFuncOrIdent {
          span: self.peek().span,
          found: self.peek().kind.clone(),
        });

        None
      }
    }
  }

  pub fn is_module_discovery_beginning(&self) -> bool {
    matches!(
      self.peek().kind,
      TokenType::Identifier(_)
        | TokenType::SelfValue
        | TokenType::Package
        | TokenType::Super
        | TokenType::StringLiteral(_) // the string is here only because we report an error later
    )
  }

  fn parse_item(&mut self) -> Option<Item> {
    self.doc_comment = None;
    self.check_possible_comment();

    let span_start = self.peek().span;
    let visibility = if let Some(t) = self.is(TokenType::Pub) {
      Visibility::Public(t.span)
    } else {
      Visibility::Private
    };

    let token = self.expect_any(ITEM_TOKENS, "as an item beginning")?.kind;
    let kind = match token {
      TokenType::Mod => {
        if self.is_identifier()
          && self.peek_ahead(1)?.kind == TokenType::LeftBrace
        {
          let name = self.parse_identifier();
          self.expect(TokenType::LeftBrace, "to open a module declaration");

          todo!("mod decl not implemented");
          Some(ItemKind::Mod(ModItem {
            name,
            id: NodeId::new(),
            span: Default::default(),
            items: vec![],
          }))
        } else if self.is_module_discovery_beginning() {
          let path = self.parse_module_discovery();
          let Some(path) = path else { return None };

          self.discovery_modules.push(path);
          self.eat_semis();
          return None;
        } else {
          None
        }
      }
      TokenType::Const => {
        log_parse_failure!(self.parse_const(span_start), "const item")
      }
      TokenType::Func => Some(ItemKind::Function(log_parse_failure!(
        self.parse_function(false, span_start),
        "function item"
      )?)),
      _ => unreachable!(),
    };

    self.eat_semis();

    Some(Item {
      id: NodeId::new(),
      kind: kind?,
      // TODO: attributes parsing
      attributes: vec![],
      span: self.span_from(span_start),
      visibility,
      doc_comments: self.doc_comment.clone(),
    })
  }
}
