use std::collections::HashMap;
use zirael_parser::ty::TyId;
use zirael_parser::{
  EnumVariantData, FunctionSignature, GenericParameter, MonomorphizationId, SymbolId, Type,
};
use zirael_utils::ident_table::Identifier;
use zirael_utils::prelude::Span;

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
  pub data: GenericEnumData,
}

#[derive(Debug, Clone)]
pub enum GenericEnumData {
  Unit,
  Struct(Vec<GenericStructField>),
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
pub struct MonomorphizedEnumVariant {
  pub original_symbol_id: SymbolId,
  pub mono_id: MonomorphizationId,
  pub name: Identifier,
  pub symbol_id: SymbolId,
  pub fields: Vec<MonomorphizedStructField>,
  pub concrete_types: HashMap<Identifier, TyId>,
}

#[derive(Debug, Clone)]
pub struct GenericSymbolBase {
  pub symbol_id: SymbolId,
  pub name: Identifier,
  pub is_used: bool,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MonomorphizedSymbolBase {
  pub original_symbol_id: SymbolId,
  pub name: Identifier,
  pub concrete_types: HashMap<Identifier, TyId>,
}

#[derive(Debug, Clone)]
pub enum GenericSymbolKind {
  Function {
    signature: FunctionSignature,
    generics: Vec<GenericParameter>,
    is_extern: bool,
    body_type: Option<TyId>,
  },
  Struct {
    generics: Vec<GenericParameter>,
    fields: Vec<GenericStructField>,
  },
  Enum {
    generics: Vec<GenericParameter>,
    variants: Vec<GenericEnumVariant>,
  },
  EnumVariant {
    parent_enum: SymbolId,
    data: GenericEnumData,
    has_generics: bool,
  },
  Value {
    ty: TyId,
  },
}

#[derive(Debug, Clone)]
pub enum MonomorphizedSymbolKind {
  Function {
    mono_id: Option<MonomorphizationId>,
    mangled_name: String,
    signature: FunctionSignature,
    is_extern: bool,
  },
  Struct {
    mono_id: Option<MonomorphizationId>,
    mangled_name: String,
    fields: Vec<MonomorphizedStructField>,
  },
  EnumVariant {
    mono_id: MonomorphizationId,
    fields: Vec<MonomorphizedStructField>,
    parent_enum: SymbolId,
  },
}

#[derive(Clone, Debug)]
pub struct GenericSymbol {
  pub base: GenericSymbolBase,
  pub kind: GenericSymbolKind,
}

#[derive(Clone, Debug)]
pub struct MonomorphizedSymbol {
  pub base: MonomorphizedSymbolBase,
  pub kind: MonomorphizedSymbolKind,
}

impl MonomorphizedSymbol {
  pub fn original_symbol_id(&self) -> SymbolId {
    self.base.original_symbol_id
  }

  pub fn name(&self) -> &Identifier {
    &self.base.name
  }

  pub fn concrete_types(&self) -> &HashMap<Identifier, TyId> {
    &self.base.concrete_types
  }

  pub fn mono_id(&self) -> Option<MonomorphizationId> {
    match &self.kind {
      MonomorphizedSymbolKind::Function { mono_id, .. } => *mono_id,
      MonomorphizedSymbolKind::Struct { mono_id, .. } => *mono_id,
      MonomorphizedSymbolKind::EnumVariant { mono_id, .. } => Some(*mono_id),
    }
  }
}

impl GenericSymbol {
  pub fn symbol_id(&self) -> SymbolId {
    self.base.symbol_id
  }

  pub fn name(&self) -> &Identifier {
    &self.base.name
  }

  pub fn generics(&self) -> Option<&Vec<GenericParameter>> {
    match &self.kind {
      GenericSymbolKind::Function { generics, .. } => Some(generics),
      GenericSymbolKind::Struct { generics, .. } => Some(generics),
      GenericSymbolKind::Enum { generics, .. } => Some(generics),
      _ => None,
    }
  }
}

impl GenericSymbol {
  pub fn function(
    symbol_id: SymbolId,
    name: Identifier,
    signature: FunctionSignature,
    generics: Vec<GenericParameter>,
    is_extern: bool,
    body_type: Option<TyId>,
    is_used: bool,
    span: Span,
  ) -> Self {
    Self {
      base: GenericSymbolBase { symbol_id, name, is_used, span },
      kind: GenericSymbolKind::Function { signature, generics, is_extern, body_type },
    }
  }

  pub fn value(symbol_id: SymbolId, name: Identifier, ty: TyId, is_used: bool, span: Span) -> Self {
    Self {
      base: GenericSymbolBase { symbol_id, name, is_used, span },
      kind: GenericSymbolKind::Value { ty },
    }
  }

  pub fn struct_def(
    symbol_id: SymbolId,
    name: Identifier,
    generics: Vec<GenericParameter>,
    fields: Vec<GenericStructField>,
    is_used: bool,
    span: Span,
  ) -> Self {
    Self {
      base: GenericSymbolBase { symbol_id, name, is_used, span },
      kind: GenericSymbolKind::Struct { generics, fields },
    }
  }

  pub fn enum_def(
    symbol_id: SymbolId,
    name: Identifier,
    generics: Vec<GenericParameter>,
    variants: Vec<GenericEnumVariant>,
    is_used: bool,
    span: Span,
  ) -> Self {
    Self {
      base: GenericSymbolBase { symbol_id, name, is_used, span },
      kind: GenericSymbolKind::Enum { generics, variants },
    }
  }

  pub fn enum_variant(
    symbol_id: SymbolId,
    name: Identifier,
    parent_enum: SymbolId,
    data: GenericEnumData,
    is_used: bool,
    span: Span,
    has_generics: bool,
  ) -> Self {
    Self {
      base: GenericSymbolBase { symbol_id, name, is_used, span },
      kind: GenericSymbolKind::EnumVariant { parent_enum, data, has_generics },
    }
  }
}

impl MonomorphizedSymbol {
  pub fn function(
    original_symbol_id: SymbolId,
    name: Identifier,
    mangled_name: String,
    signature: FunctionSignature,
    concrete_types: HashMap<Identifier, TyId>,
    is_extern: bool,
    mono_id: Option<MonomorphizationId>,
  ) -> Self {
    Self {
      base: MonomorphizedSymbolBase { original_symbol_id, name, concrete_types },
      kind: MonomorphizedSymbolKind::Function { mono_id, mangled_name, signature, is_extern },
    }
  }

  pub fn struct_def(
    original_symbol_id: SymbolId,
    name: Identifier,
    mangled_name: String,
    fields: Vec<MonomorphizedStructField>,
    concrete_types: HashMap<Identifier, TyId>,
    mono_id: Option<MonomorphizationId>,
  ) -> Self {
    Self {
      base: MonomorphizedSymbolBase { original_symbol_id, name, concrete_types },
      kind: MonomorphizedSymbolKind::Struct { mono_id, mangled_name, fields },
    }
  }
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

impl MonomorphizedEnumVariant {
  pub fn new(
    original_symbol_id: SymbolId,
    mono_id: MonomorphizationId,
    name: Identifier,
    mangled_name: String,
    symbol_id: SymbolId,
  ) -> Self {
    Self {
      original_symbol_id,
      mono_id,
      name,
      concrete_types: HashMap::new(),
      symbol_id,
      fields: vec![],
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
