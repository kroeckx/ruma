//! Details of the `response` section of the procedural macro.

use proc_macro2::TokenStream;
use quote::quote;
use syn::{punctuated::Punctuated, Attribute, Field, Token};

use super::metadata::Metadata;

/// The result of processing the `response` section of the macro.
pub(crate) struct Response {
    /// The attributes that will be applied to the struct definition.
    pub attributes: Vec<Attribute>,

    /// The fields of the response.
    pub fields: Punctuated<Field, Token![,]>,
}

impl Response {
    pub(super) fn expand(
        &self,
        metadata: &Metadata,
        error_ty: &TokenStream,
        ruma_api: &TokenStream,
    ) -> TokenStream {
        let ruma_api_macros = quote! { #ruma_api::exports::ruma_api_macros };
        let ruma_serde = quote! { #ruma_api::exports::ruma_serde };
        let serde = quote! { #ruma_api::exports::serde };

        let docs =
            format!("Data in the response from the `{}` API endpoint.", metadata.name.value());
        let struct_attributes = &self.attributes;

        let has_test_exhaustive_field = self
            .fields
            .iter()
            .filter_map(|f| f.ident.as_ref())
            .any(|ident| ident == "__test_exhaustive");

        let non_exhaustive_attr = if has_test_exhaustive_field {
            quote! {}
        } else {
            quote! { #[cfg_attr(not(feature = "unstable-exhaustive-types"), non_exhaustive)] }
        };

        let fields = &self.fields;
        quote! {
            #[doc = #docs]
            #[derive(
                Clone,
                Debug,
                #ruma_api_macros::Response,
                #ruma_serde::Outgoing,
                #ruma_serde::_FakeDeriveSerde,
            )]
            #non_exhaustive_attr
            #[incoming_derive(!Deserialize)]
            #[ruma_api(error_ty = #error_ty)]
            #( #struct_attributes )*
            pub struct Response {
                #(#fields),*
            }
        }
    }
}
