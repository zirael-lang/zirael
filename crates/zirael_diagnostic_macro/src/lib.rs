mod macro_impl;

use crate::macro_impl::macro_derive_impl;
use proc_macro::TokenStream;

#[proc_macro_derive(
  Diagnostic,
  attributes(error, note, notel, warning, help, bug, span, code)
)]
pub fn derive_diagnostic(item: TokenStream) -> TokenStream {
  macro_derive_impl(item)
}
