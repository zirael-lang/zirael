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

fn get_string_literal(expr: &Expr) -> Result<String, MacroFunctionError> {
  match expr {
    Expr::Lit(expr_lit) => {
      if let syn::Lit::Str(lit_str) = &expr_lit.lit {
        Ok(lit_str.value())
      } else {
        Err(MacroFunctionError::InvalidAttribute("Message must be a string literal".to_string()))
      }
    }
    _ => Err(MacroFunctionError::InvalidAttribute("Message must be a string literal".to_string())),
  }
}

fn is_valid_severity(ident: &str) -> bool {
  VALID_SEVERITIES.contains(&ident.to_lowercase().as_str())
}

fn get_label_style(severity: &str) -> proc_macro2::TokenStream {
  match severity.to_lowercase().as_str() {
    "error" => quote! { LabelStyle::Error },
    "warning" => quote! { LabelStyle::Warning },
    "note" => quote! { LabelStyle::Note },
    "help" => quote! { LabelStyle::Help },
    _ => quote! { LabelStyle::Primary },
  }
}

fn to_pascal_case(s: &str) -> String {
  let mut result = String::with_capacity(s.len());
  result.push_str(&s[..1].to_uppercase());
  result.push_str(&s[1..]);
  result
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

  let diagnostic_attr = ast
    .attrs
    .iter()
    .find(|attr| {
      attr
        .path()
        .segments
        .last()
        .map(|seg| is_valid_severity(&seg.ident.to_string()))
        .unwrap_or(false)
    })
    .ok_or(MacroFunctionError::MissingAttribute)?;

  let severity = diagnostic_attr.path().segments.last().unwrap().ident.to_string().to_lowercase();

  let severity_variant = to_pascal_case(&severity);

  let error_string = get_string_literal(&diagnostic_attr.parse_args::<Expr>()?)?;

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

  // Fields with severity attributes (span labels)
  let span_fields: Vec<_> = fields
    .named
    .iter()
    .filter_map(|field| {
      let attr = field.attrs.iter().find(|attr| {
        attr
          .path()
          .segments
          .last()
          .map(|seg| is_valid_severity(&seg.ident.to_string()))
          .unwrap_or(false)
      })?;

      let severity = attr.path().segments.last().unwrap().ident.to_string().to_lowercase();

      let message = attr
        .parse_args::<Expr>()
        .ok()
        .and_then(|expr| get_string_literal(&expr).ok())
        .unwrap_or_default();

      Some((field.ident.clone(), message, severity))
    })
    .collect();

  let message_field_refs = message_fields.iter().map(|&ident| {
    quote! { let #ident = &self.#ident; }
  });

  let label_implementations = span_fields.iter().map(|(field_ident, message, severity)| {
    let ident = field_ident.as_ref().unwrap();
    let label_style = get_label_style(severity);
    let message_field_refs_clone = message_fields.iter().map(|&msg_ident| {
      quote! { let #msg_ident = &self.#msg_ident; }
    });

    quote! {
        {
            #(#message_field_refs_clone)*
            let message = format!(#message);

            Label::new(
                #label_style,
                self.#ident.1,
                self.#ident.0.range()
            )
            .with_message(message)
        }
    }
  });

  let note_field_refs = note_fields.iter().map(|&ident| {
    quote! { let #ident = &self.#ident; }
  });

  let note_strings = note_fields.iter().map(|&ident| {
    quote! { #ident.to_string() }
  });

  let severity_ident = syn::Ident::new(&severity_variant, proc_macro2::Span::call_site());

  Ok(TokenStream::from(quote! {
      #[automatically_derived]
      impl ToDiagnostic for #struct_name {
          fn message(&self) -> String {
              #(#message_field_refs)*
              format!(#error_string)
          }

          fn labels(&self) -> Vec<Label<usize>> {
              vec![
                  #(#label_implementations,)*
              ]
          }

          fn severity(&self) -> Severity {
              Severity::#severity_ident
          }

          fn code(&self) -> Option<String> {
              None
          }

          fn notes(&self) -> Vec<String> {
              #(#note_field_refs)*
              vec![
                  #(#note_strings,)*
              ]
          }
      }
  }))
}
