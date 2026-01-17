use proc_macro::TokenStream;
use quote::quote;
use std::collections::{BTreeSet, HashMap};
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
    syn::Type::Path(type_path) => type_path
      .path
      .segments
      .last()
      .map(|seg| seg.ident == "Span")
      .unwrap_or(false),
    _ => false,
  }
}

fn is_vec_span_type(ty: &syn::Type) -> bool {
  match ty {
    syn::Type::Path(type_path) => {
      let last_seg = type_path.path.segments.last();
      if let Some(seg) = last_seg {
        if seg.ident != "Vec" {
          return false;
        }
        if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
          if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first()
          {
            return is_span_type(inner_ty);
          }
        }
      }
      false
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
        Err(MacroFunctionError::InvalidAttribute(
          "Message must be a string literal".to_string(),
        ))
      }
    }
    _ => Err(MacroFunctionError::InvalidAttribute(
      "Message must be a string literal".to_string(),
    )),
  }
}

fn is_valid_format_ident(ident: &str) -> bool {
  let ident = ident.strip_prefix("r#").unwrap_or(ident);
  let mut chars = ident.chars();
  let Some(first) = chars.next() else {
    return false;
  };
  if !(first == '_' || first.is_ascii_alphabetic()) {
    return false;
  }
  chars.all(|c| c == '_' || c.is_ascii_alphanumeric())
}

/// Extracts *named* formatting placeholders used by a Rust format string.
///
/// - Supports escaped braces (`{{` and `}}`).
/// - Rejects positional placeholders (`{}`, `{0}`, `{:?}`), since this macro
///   only permits `{name}`.
/// - Ignores format specifiers (e.g. `{name:?}`, `{name:>10}`) when extracting the name.
fn extract_named_placeholders(
  fmt: &str,
) -> Result<Vec<String>, MacroFunctionError> {
  let mut out: BTreeSet<String> = BTreeSet::new();
  let mut chars = fmt.chars().peekable();

  while let Some(ch) = chars.next() {
    match ch {
      '{' => {
        if chars.peek() == Some(&'{') {
          chars.next();
          continue;
        }

        let mut inner = String::new();
        let mut closed = false;
        for next in chars.by_ref() {
          if next == '}' {
            closed = true;
            break;
          }
          inner.push(next);
        }

        if !closed {
          return Err(MacroFunctionError::InvalidAttribute(
            "unclosed `{` in format string".to_string(),
          ));
        }

        let inner = inner.trim();
        if inner.is_empty() {
          return Err(MacroFunctionError::InvalidAttribute(
            "positional formatting (`{}`) is not supported; use `{field}`"
              .to_string(),
          ));
        }

        // Split off formatting spec (e.g. name:? or name:>10)
        let name_part = inner.split([':', '!']).next().unwrap_or("").trim();

        if name_part.is_empty() {
          return Err(MacroFunctionError::InvalidAttribute(
            "positional formatting (`{:...}`) is not supported; use `{field}`"
              .to_string(),
          ));
        }

        if name_part.chars().next().is_some_and(|c| c.is_ascii_digit()) {
          return Err(MacroFunctionError::InvalidAttribute(
            "positional formatting (`{0}`) is not supported; use `{field}`"
              .to_string(),
          ));
        }

        if !is_valid_format_ident(name_part) {
          return Err(MacroFunctionError::InvalidAttribute(format!(
            "invalid format placeholder `{{{}}}`; use a field name like `{{expected}}`",
            name_part
          )));
        }

        out.insert(name_part.to_string());
      }
      '}' => {
        if chars.peek() == Some(&'}') {
          chars.next();
        } else {
          return Err(MacroFunctionError::InvalidAttribute(
            "unmatched `}` in format string".to_string(),
          ));
        }
      }
      _ => {}
    }
  }

  Ok(out.into_iter().collect())
}

fn collect_struct_messages(
  ast: &DeriveInput,
  attr_name: &str,
) -> Result<Vec<syn::LitStr>, MacroFunctionError> {
  let mut out = Vec::new();

  for attr in &ast.attrs {
    let is_match = attr
      .path()
      .segments
      .last()
      .map(|seg| seg.ident == attr_name)
      .unwrap_or(false);

    if !is_match {
      continue;
    }

    let expr = attr.parse_args::<Expr>()?;
    out.push(get_lit_str(&expr)?);
  }

  Ok(out)
}

fn parse_code_to_tokens(
  expr: &Expr,
) -> Result<proc_macro2::TokenStream, MacroFunctionError> {
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
    let is_match = attr
      .path()
      .segments
      .last()
      .map(|seg| seg.ident == "code")
      .unwrap_or(false);
    if !is_match {
      continue;
    }

    if out.is_some() {
      return Err(MacroFunctionError::InvalidAttribute(
        "duplicate #[code] attribute".to_string(),
      ));
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
      let has_help = field.attrs.iter().any(|attr| {
        attr
          .path()
          .segments
          .last()
          .map(|seg| seg.ident == "help")
          .unwrap_or(false)
      });

      if has_help { field.ident.as_ref() } else { None }
    })
    .collect()
}

pub fn macro_derive_impl(item: TokenStream) -> TokenStream {
  let ast = parse_macro_input!(item as DeriveInput);

  match impl_diagnostic_derive(&ast) {
    Ok(token_stream) => token_stream,
    Err(error) => TokenStream::from(
      syn::Error::new(ast.span(), error.to_string()).to_compile_error(),
    ),
  }
}

fn impl_diagnostic_derive(
  ast: &DeriveInput,
) -> Result<TokenStream, MacroFunctionError> {
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

  let severity = diagnostic_attr
    .path()
    .segments
    .last()
    .unwrap()
    .ident
    .to_string()
    .to_lowercase();
  let error_message_lit = get_lit_str(&diagnostic_attr.parse_args::<Expr>()?)?;
  let error_message = error_message_lit.value();

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
      return Err(MacroFunctionError::InvalidAttribute(
        "Only structs are supported".to_string(),
      ));
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

      if !has_severity_attr {
        field.ident.as_ref()
      } else {
        None
      }
    })
    .collect();

  let message_field_map: HashMap<String, &syn::Ident> = message_fields
    .iter()
    .map(|&ident| (ident.to_string(), ident))
    .collect();

  let main_message_used_names = extract_named_placeholders(&error_message)?;
  let mut main_message_used_fields: Vec<&syn::Ident> = Vec::new();
  for name in main_message_used_names {
    let Some(ident) = message_field_map.get(&name) else {
      return Err(MacroFunctionError::InvalidAttribute(format!(
        "unknown format placeholder `{{{}}}` in #[{}(...)]",
        name, severity
      )));
    };
    main_message_used_fields.push(*ident);
  }

  // Fields with #[note] attribute
  let note_fields: Vec<_> = fields
    .named
    .iter()
    .filter_map(|field| {
      let has_note = field.attrs.iter().any(|attr| {
        attr
          .path()
          .segments
          .last()
          .map(|seg| seg.ident == "note")
          .unwrap_or(false)
      });

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
      let is_span = is_span_type(&field.ty);
      let is_vec_span = is_vec_span_type(&field.ty);

      if !is_span && !is_vec_span {
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
      let severity = attr
        .path()
        .segments
        .last()
        .unwrap()
        .ident
        .to_string()
        .to_lowercase();

      let message = attr
        .parse_args::<Expr>()
        .ok()
        .and_then(|expr| get_string_literal(&expr).ok())
        .unwrap_or_default();

      Some((field_ident.clone(), message, severity, is_vec_span))
    })
    .collect();

  let main_message_field_refs = main_message_used_fields.iter().map(|&ident| {
    quote! { let #ident = &self.#ident; }
  });

  let mut label_implementations: Vec<proc_macro2::TokenStream> = Vec::new();
  for (ident, message, severity, is_vec) in &label_fields {
    let diagnostic_level = get_diagnostic_level(severity);

    if message.is_empty() {
      if *is_vec {
        label_implementations.push(quote! {
          self.#ident.iter().map(|span| {
            zirael_diagnostics::Label::new(String::new(), *span, #diagnostic_level)
          })
        });
      } else {
        label_implementations.push(quote! {
          std::iter::once(zirael_diagnostics::Label::new(String::new(), self.#ident, #diagnostic_level))
        });
      }
      continue;
    }

    let message_lit = syn::LitStr::new(message, proc_macro2::Span::call_site());
    let used_names = extract_named_placeholders(message)?;
    let mut used_fields: Vec<&syn::Ident> = Vec::new();
    for name in used_names {
      let Some(field_ident) = message_field_map.get(&name) else {
        return Err(MacroFunctionError::InvalidAttribute(format!(
          "unknown format placeholder `{{{}}}` in label message",
          name
        )));
      };
      used_fields.push(*field_ident);
    }

    if used_fields.is_empty() {
      if *is_vec {
        label_implementations.push(quote! {
          {
            let message = format!(#message_lit);
            self.#ident.iter().map(move |span| {
              zirael_diagnostics::Label::new(message.clone(), *span, #diagnostic_level)
            })
          }
        });
      } else {
        label_implementations.push(quote! {
          {
            let message = format!(#message_lit);
            std::iter::once(zirael_diagnostics::Label::new(message, self.#ident, #diagnostic_level))
          }
        });
      }
    } else {
      let field_refs = used_fields.iter().map(|&field_ident| {
        quote! { let #field_ident = &self.#field_ident; }
      });
      let field_args = used_fields.iter().map(|&field_ident| {
        quote! { #field_ident = #field_ident }
      });
      if *is_vec {
        label_implementations.push(quote! {
          {
            #(#field_refs)*
            let message = format!(#message_lit, #(#field_args),*);
            self.#ident.iter().map(move |span| {
              zirael_diagnostics::Label::new(message.clone(), *span, #diagnostic_level)
            })
          }
        });
      } else {
        label_implementations.push(quote! {
          {
            #(#field_refs)*
            let message = format!(#message_lit, #(#field_args),*);
            std::iter::once(zirael_diagnostics::Label::new(message, self.#ident, #diagnostic_level))
          }
        });
      }
    }
  }

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

  let notes_impl = if note_fields.is_empty() && struct_note_messages.is_empty()
  {
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

  let helps_impl = if help_fields.is_empty() && struct_help_messages.is_empty()
  {
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

  let message_impl = if main_message_used_fields.is_empty() {
    quote! {
      let message = format!(#error_message_lit);
    }
  } else {
    let field_args = main_message_used_fields.iter().map(|&ident| {
      quote! { #ident = #ident }
    });
    quote! {
      #(#main_message_field_refs)*
      let message = format!(#error_message_lit, #(#field_args),*);
    }
  };

  let code_impl = if let Some(code_expr) = struct_code {
    quote! { Some(#code_expr) }
  } else {
    quote! { None }
  };

  let labels_impl = if label_implementations.is_empty() {
    quote! { vec![] }
  } else {
    quote! {
      vec![
        #(#label_implementations),*
      ].into_iter().flatten().collect()
    }
  };

  Ok(TokenStream::from(quote! {
      #[automatically_derived]
        impl zirael_diagnostics::ToDiagnostic for #struct_name {
          fn to_diagnostic(&self) -> zirael_diagnostics::Diag {
            #message_impl

            zirael_diagnostics::Diag {
                  message,
                  level: #diagnostic_level,
                  labels: #labels_impl,
                  notes: #notes_impl,
                helps: #helps_impl,
                code: #code_impl,
              }
          }
      }
  }))
}
