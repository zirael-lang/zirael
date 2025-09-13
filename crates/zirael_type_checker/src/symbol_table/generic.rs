use std::collections::HashMap;
use zirael_parser::ty::TyId;
use zirael_parser::{FunctionSignature, GenericParameter, MonomorphizationId, SymbolId, Type};
use zirael_utils::ident_table::Identifier;

#[derive(Debug, Clone)]
pub struct GenericFunction {
  pub symbol_id: SymbolId,
  pub name: Identifier,
  pub signature: FunctionSignature,
  pub generics: Vec<GenericParameter>,
  pub is_extern: bool,
  pub body_type: Option<TyId>,
}

#[derive(Debug, Clone)]
pub struct GenericStruct {
  pub symbol_id: SymbolId,
  pub name: Identifier,
  pub generics: Vec<GenericParameter>,
  pub fields: Vec<GenericStructField>,
}

#[derive(Debug, Clone)]
pub struct GenericStructField {
  pub name: Identifier,
  pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct GenericEnum {
  pub symbol_id: SymbolId,
  pub name: Identifier,
  pub generics: Vec<GenericParameter>,
  pub variants: Vec<GenericEnumVariant>,
}

#[derive(Debug, Clone)]
pub struct GenericEnumVariant {
  pub name: Identifier,
  pub symbol_id: SymbolId,
  pub fields: Vec<GenericStructField>,
}

#[derive(Debug, Clone)]
pub struct MonomorphizedFunction {
  pub original_symbol_id: SymbolId,
  pub mono_id: Option<MonomorphizationId>,
  pub name: Identifier,
  pub mangled_name: String,
  pub signature: FunctionSignature,
  pub concrete_types: HashMap<Identifier, TyId>,
  pub is_extern: bool,
}

#[derive(Debug, Clone)]
pub struct MonomorphizedStruct {
  pub original_symbol_id: SymbolId,
  pub mono_id: MonomorphizationId,
  pub name: Identifier,
  pub mangled_name: String,
  pub fields: Vec<MonomorphizedStructField>,
  pub concrete_types: HashMap<Identifier, TyId>,
}

#[derive(Debug, Clone)]
pub struct MonomorphizedStructField {
  pub name: Identifier,
  pub concrete_ty: TyId,
  pub is_public: bool,
}

#[derive(Debug, Clone)]
pub struct MonomorphizedEnum {
  pub original_symbol_id: SymbolId,
  pub mono_id: MonomorphizationId,
  pub name: Identifier,
  pub mangled_name: String,
  pub variants: Vec<MonomorphizedEnumVariant>,
  pub concrete_types: HashMap<Identifier, TyId>,
}

#[derive(Debug, Clone)]
pub struct MonomorphizedEnumVariant {
  pub name: Identifier,
  pub symbol_id: SymbolId,
  pub fields: Vec<MonomorphizedStructField>,
}

impl GenericFunction {
  pub fn new(symbol_id: SymbolId, name: Identifier, signature: FunctionSignature) -> Self {
    Self { symbol_id, name, signature, generics: Vec::new(), is_extern: false, body_type: None }
  }

  pub fn with_generics(mut self, generics: Vec<GenericParameter>) -> Self {
    self.generics = generics;
    self
  }

  pub fn with_extern(mut self, is_extern: bool) -> Self {
    self.is_extern = is_extern;
    self
  }

  pub fn with_body_type(mut self, body_type: TyId) -> Self {
    self.body_type = Some(body_type);
    self
  }
}

impl GenericStruct {
  pub fn new(symbol_id: SymbolId, name: Identifier) -> Self {
    Self { symbol_id, name, generics: Vec::new(), fields: Vec::new() }
  }

  pub fn with_generics(mut self, generics: Vec<GenericParameter>) -> Self {
    self.generics = generics;
    self
  }

  pub fn with_fields(mut self, fields: Vec<GenericStructField>) -> Self {
    self.fields = fields;
    self
  }
}

impl GenericEnum {
  pub fn new(symbol_id: SymbolId, name: Identifier) -> Self {
    Self { symbol_id, name, generics: Vec::new(), variants: Vec::new() }
  }

  pub fn with_generics(mut self, generics: Vec<GenericParameter>) -> Self {
    self.generics = generics;
    self
  }

  pub fn with_variants(mut self, variants: Vec<GenericEnumVariant>) -> Self {
    self.variants = variants;
    self
  }
}

impl MonomorphizedFunction {
  pub fn new(
    original_symbol_id: SymbolId,
    name: Identifier,
    mangled_name: String,
    signature: FunctionSignature,
  ) -> Self {
    Self {
      original_symbol_id,
      mono_id: None,
      name,
      mangled_name,
      signature,
      concrete_types: HashMap::new(),
      is_extern: false,
    }
  }

  pub fn with_concrete_types(mut self, concrete_types: HashMap<Identifier, TyId>) -> Self {
    self.concrete_types = concrete_types;
    self
  }
}

impl MonomorphizedStruct {
  pub fn new(
    original_symbol_id: SymbolId,
    mono_id: MonomorphizationId,
    name: Identifier,
    mangled_name: String,
  ) -> Self {
    Self {
      original_symbol_id,
      mono_id,
      name,
      mangled_name,
      fields: Vec::new(),
      concrete_types: HashMap::new(),
    }
  }

  pub fn with_fields(mut self, fields: Vec<MonomorphizedStructField>) -> Self {
    self.fields = fields;
    self
  }

  pub fn with_concrete_types(mut self, concrete_types: HashMap<Identifier, TyId>) -> Self {
    self.concrete_types = concrete_types;
    self
  }
}

impl MonomorphizedEnum {
  pub fn new(
    original_symbol_id: SymbolId,
    mono_id: MonomorphizationId,
    name: Identifier,
    mangled_name: String,
  ) -> Self {
    Self {
      original_symbol_id,
      mono_id,
      name,
      mangled_name,
      variants: Vec::new(),
      concrete_types: HashMap::new(),
    }
  }

  pub fn with_variants(mut self, variants: Vec<MonomorphizedEnumVariant>) -> Self {
    self.variants = variants;
    self
  }

  pub fn with_concrete_types(mut self, concrete_types: HashMap<Identifier, TyId>) -> Self {
    self.concrete_types = concrete_types;
    self
  }
}
