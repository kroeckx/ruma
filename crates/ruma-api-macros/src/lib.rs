#![doc(html_favicon_url = "https://www.ruma.io/favicon.ico")]
#![doc(html_logo_url = "https://www.ruma.io/images/logo.png")]
//! A procedural macro for easily generating [ruma-api]-compatible endpoints.
//!
//! This crate should never be used directly; instead, use it through the
//! re-exports in ruma-api. Also note that for technical reasons, the
//! `ruma_api!` macro is only documented in ruma-api, not here.
//!
//! [ruma-api]: https://github.com/ruma/ruma/tree/main/ruma-api

#![recursion_limit = "256"]

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod api;
mod auth_scheme;
mod request;
mod response;
mod util;

use api::Api;
use request::expand_derive_request;
use response::expand_derive_response;

#[proc_macro]
pub fn ruma_api(input: TokenStream) -> TokenStream {
    let api = parse_macro_input!(input as Api);
    api::expand_all(api).unwrap_or_else(syn::Error::into_compile_error).into()
}

/// Internal helper taking care of the request-specific parts of `ruma_api!`.
#[proc_macro_derive(Request, attributes(ruma_api))]
pub fn derive_request(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand_derive_request(input).unwrap_or_else(syn::Error::into_compile_error).into()
}

/// Internal helper taking care of the response-specific parts of `ruma_api!`.
#[proc_macro_derive(Response, attributes(ruma_api))]
pub fn derive_response(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand_derive_response(input).unwrap_or_else(syn::Error::into_compile_error).into()
}
