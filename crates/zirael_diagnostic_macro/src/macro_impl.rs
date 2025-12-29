use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, Expr, parse_macro_input, spanned::Spanned};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum MacroFunctionError {
  #[error("Parse error: {0}")]
  ParseError(#[from] syn::Error),
  #[error("Invalid attribute: {0}")]
  InvalidAttribute(String),
  #[error("Missing required attribute")]
  MissingAttribute,
}

const VALID_SEVERITIES: &[&str] = &["help", "warning", "error", "bug", "note"];
const VALID_LABEL_SEVERITIES: &[&str] = &["error", "warning", "help"];

fn is_span_type(ty: &syn::Type) -> bool {
  match ty {
    syn::Type::Path(type_path) => {
      type_path.path.segments.last().map(|seg| seg.ident == "Span").unwrap_or(false)
    }
    _ => false,
  }
}

fn get_lit_str(expr: &Expr) -> Result<syn::LitStr, MacroFunctionError> {
  match expr {
    Expr::Lit(expr_lit) => {
      if let syn::Lit::Str(lit_str) = &expr_lit.lit {
        Ok(lit_str.clone())
      } else {
        Err(MacroFunctionError::InvalidAttribute("Message must be a string literal".to_string()))
      }
    }
    _ => Err(MacroFunctionError::InvalidAttribute("Message must be a string literal".to_string())),
  }
}

fn collect_struct_messages(
  ast: &DeriveInput,
  attr_name: &str,
) -> Result<Vec<syn::LitStr>, MacroFunctionError> {
  let mut out = Vec::new();

  for attr in &ast.attrs {
    let is_match = attr.path().segments.last().map(|seg| seg.ident == attr_name).unwrap_or(false);

    if !is_match {
      continue;
    }

    let expr = attr.parse_args::<Expr>()?;
    out.push(get_lit_str(&expr)?);
  }

  Ok(out)
}

fn parse_code_to_tokens(expr: &Expr) -> Result<proc_macro2::TokenStream, MacroFunctionError> {
  match expr {
    Expr::Path(path) => {
      let segments = &path.path.segments;
      if segments.len() == 1 {
        let ident = &segments[0].ident;
        Ok(quote! { zirael_diagnostics::codes::#ident })
      } else {
        Ok(quote! { #path })
      }
    }

    _ => Err(MacroFunctionError::InvalidAttribute(
      "code must be an integer literal or a path (e.g. LEX_FOO)".to_string(),
    )),
  }
}

fn collect_struct_code(
  ast: &DeriveInput,
) -> Result<Option<proc_macro2::TokenStream>, MacroFunctionError> {
  let mut out: Option<proc_macro2::TokenStream> = None;

  for attr in &ast.attrs {
    let is_match = attr.path().segments.last().map(|seg| seg.ident == "code").unwrap_or(false);
    if !is_match {
      continue;
    }

    if out.is_some() {
      return Err(MacroFunctionError::InvalidAttribute("duplicate #[code] attribute".to_string()));
    }

    // Support both: #[code(1234)] and #[code = 1234]
    let code_expr: Expr = match &attr.meta {
      syn::Meta::List(_) => attr.parse_args::<Expr>()?,
      syn::Meta::NameValue(nv) => nv.value.clone(),
      syn::Meta::Path(_) => {
        return Err(MacroFunctionError::InvalidAttribute(
          "#[code] requires a value like #[code(1234)]".to_string(),
        ));
      }
    };

    out = Some(parse_code_to_tokens(&code_expr)?);
  }

  Ok(out)
}

fn get_string_literal(expr: &Expr) -> Result<String, MacroFunctionError> {
  Ok(get_lit_str(expr)?.value())
}

fn is_valid_severity(ident: &str) -> bool {
  VALID_SEVERITIES.contains(&ident.to_lowercase().as_str())
}

fn is_valid_label_severity(ident: &str) -> bool {
  VALID_LABEL_SEVERITIES.contains(&ident.to_lowercase().as_str())
}

fn get_diagnostic_level(severity: &str) -> proc_macro2::TokenStream {
  match severity.to_lowercase().as_str() {
    "error" => quote! { zirael_diagnostics::DiagnosticLevel::Error },
    "warning" => quote! { zirael_diagnostics::DiagnosticLevel::Warning },
    "bug" => quote! { zirael_diagnostics::DiagnosticLevel::Bug },
    _ => quote! { zirael_diagnostics::DiagnosticLevel::Error },
  }
}

fn get_help_fields(fields: &syn::FieldsNamed) -> Vec<&syn::Ident> {
  fields
    .named
    .iter()
    .filter_map(|field| {
      let has_help = field
        .attrs
        .iter()
        .any(|attr| attr.path().segments.last().map(|seg| seg.ident == "help").unwrap_or(false));

      if has_help { field.ident.as_ref() } else { None }
    })
    .collect()
}

pub fn macro_derive_impl(item: TokenStream) -> TokenStream {
  let ast = parse_macro_input!(item as DeriveInput);

  match impl_diagnostic_derive(&ast) {
    Ok(token_stream) => token_stream,
    Err(error) => {
      TokenStream::from(syn::Error::new(ast.span(), error.to_string()).to_compile_error())
    }
  }
}

fn impl_diagnostic_derive(ast: &DeriveInput) -> Result<TokenStream, MacroFunctionError> {
  let struct_name = &ast.ident;

  let struct_note_messages = collect_struct_messages(ast, "note")?;
  let struct_help_messages = collect_struct_messages(ast, "help")?;
  let struct_code = collect_struct_code(ast)?;

  // Find the main diagnostic attribute (#[error], #[warning], or #[bug])
  let diagnostic_attr = ast
    .attrs
    .iter()
    .find(|attr| {
      attr
        .path()
        .segments
        .last()
        .map(|seg| {
          let ident = seg.ident.to_string();
          ident == "error" || ident == "warning" || ident == "bug"
        })
        .unwrap_or(false)
    })
    .ok_or(MacroFunctionError::MissingAttribute)?;

  let severity = diagnostic_attr.path().segments.last().unwrap().ident.to_string().to_lowercase();
  let error_message = get_string_literal(&diagnostic_attr.parse_args::<Expr>()?)?;

  let fields = match &ast.data {
    syn::Data::Struct(data) => match &data.fields {
      syn::Fields::Named(fields) => fields,
      _ => {
        return Err(MacroFunctionError::InvalidAttribute(
          "Only named fields are supported".to_string(),
        ));
      }
    },
    _ => {
      return Err(MacroFunctionError::InvalidAttribute("Only structs are supported".to_string()));
    }
  };

  // Fields without severity attributes (used in message interpolation)
  let message_fields: Vec<_> = fields
    .named
    .iter()
    .filter_map(|field| {
      let has_severity_attr = field.attrs.iter().any(|attr| {
        attr
          .path()
          .segments
          .last()
          .map(|seg| is_valid_severity(&seg.ident.to_string()))
          .unwrap_or(false)
      });

      if !has_severity_attr { field.ident.as_ref() } else { None }
    })
    .collect();

  let message_fields_for_quote = &message_fields;

  // Fields with #[note] attribute
  let note_fields: Vec<_> = fields
    .named
    .iter()
    .filter_map(|field| {
      let has_note = field
        .attrs
        .iter()
        .any(|attr| attr.path().segments.last().map(|seg| seg.ident == "note").unwrap_or(false));

      if has_note { field.ident.as_ref() } else { None }
    })
    .collect();

  // Fields with #[help] attribute
  let help_fields = get_help_fields(fields);

  // Additional label fields (secondary spans)
  let label_fields: Vec<_> = fields
    .named
    .iter()
    .filter_map(|field| {
      if !is_span_type(&field.ty) {
        return None;
      }

      let attr = field.attrs.iter().find(|attr| {
        attr
          .path()
          .segments
          .last()
          .map(|seg| is_valid_label_severity(&seg.ident.to_string()))
          .unwrap_or(false)
      })?;

      let field_ident = field.ident.as_ref()?;
      let severity = attr.path().segments.last().unwrap().ident.to_string().to_lowercase();

      let message = attr
        .parse_args::<Expr>()
        .ok()
        .and_then(|expr| get_string_literal(&expr).ok())
        .unwrap_or_default();

      Some((field_ident.clone(), message, severity))
    })
    .collect();

  let message_field_refs = message_fields.iter().map(|&ident| {
    quote! { let #ident = &self.#ident; }
  });

  let label_implementations = label_fields.iter().map(|(ident, message, severity)| {
    let diagnostic_level = get_diagnostic_level(severity);

    if message.is_empty() {
      quote! {
        zirael_diagnostics::Label::new(String::new(), self.#ident, #diagnostic_level)
      }
    } else if message_fields.is_empty() {
      quote! {
        {
          let message = format!(#message);
          zirael_diagnostics::Label::new(message, self.#ident, #diagnostic_level)
        }
      }
    } else {
      quote! {
        {
          let message = format!(#message, #(#message_fields_for_quote),*);
          zirael_diagnostics::Label::new(message, self.#ident, #diagnostic_level)
        }
      }
    }
  });

  let note_field_refs = note_fields.iter().map(|&ident| {
    quote! { let #ident = &self.#ident; }
  });

  let note_strings = note_fields.iter().map(|&ident| {
    quote! { #ident.to_string() }
  });

  let help_field_refs = help_fields.iter().map(|&ident| {
    quote! { let #ident = &self.#ident; }
  });

  let help_strings = help_fields.iter().map(|&ident| {
    quote! { #ident.to_string() }
  });

  let struct_note_pushes = struct_note_messages.iter().map(|lit| {
    quote! { notes.push(#lit.to_string()); }
  });

  let struct_help_pushes = struct_help_messages.iter().map(|lit| {
    quote! { helps.push(#lit.to_string()); }
  });

  let diagnostic_level = get_diagnostic_level(&severity);

  let notes_impl = if note_fields.is_empty() && struct_note_messages.is_empty() {
    quote! { Vec::new() }
  } else {
    quote! {
      {
        #(#note_field_refs)*
        let mut notes = Vec::new();
        #(#struct_note_pushes)*
        #(notes.push(#note_strings);)*
        notes
      }
    }
  };

  let helps_impl = if help_fields.is_empty() && struct_help_messages.is_empty() {
    quote! { Vec::new() }
  } else {
    quote! {
      {
        #(#help_field_refs)*
        let mut helps = Vec::new();
        #(#struct_help_pushes)*
        #(helps.push(#help_strings);)*
        helps
      }
    }
  };

  let message_impl = if message_fields.is_empty() {
    quote! {
      let message = format!(#error_message);
    }
  } else {
    quote! {
      #(#message_field_refs)*
      let message = format!(#error_message, #(#message_fields_for_quote),*);
    }
  };

  let code_impl = if let Some(code_expr) = struct_code {
    quote! { Some(#code_expr) }
  } else {
    quote! { None }
  };

  Ok(TokenStream::from(quote! {
      #[automatically_derived]
        impl zirael_diagnostics::ToDiagnostic for #struct_name {
          fn to_diagnostic(&self) -> zirael_diagnostics::Diag {
            #message_impl

            zirael_diagnostics::Diag {
                  message,
                  level: #diagnostic_level,
                  labels: vec![
                      #(#label_implementations,)*
                  ],
                  notes: #notes_impl,
                helps: #helps_impl,
                code: #code_impl,
              }
          }
      }
  }))
}
