//! Details of the `request` section of the procedural macro.

use std::collections::btree_map::{BTreeMap, Entry};

use proc_macro2::TokenStream;
use quote::quote;
use syn::{punctuated::Punctuated, Attribute, Field, Lifetime, Token};

use crate::util::any_cfg;

use super::metadata::Metadata;

#[derive(Default)]
pub(super) struct RequestLifetimes {
    pub body: BTreeMap<Lifetime, Option<Attribute>>,
    pub path: BTreeMap<Lifetime, Option<Attribute>>,
    pub query: BTreeMap<Lifetime, Option<Attribute>>,
    pub header: BTreeMap<Lifetime, Option<Attribute>>,
}

/// The result of processing the `request` section of the macro.
pub(crate) struct Request {
    /// The attributes that will be applied to the struct definition.
    pub(super) attributes: Vec<Attribute>,

    /// The fields of the request.
    pub(super) fields: Punctuated<Field, Token![,]>,

    /// The collected lifetime identifiers from the declared fields.
    pub(super) lifetimes: RequestLifetimes,
}

impl Request {
    /// The combination of every fields unique lifetime annotation.
    fn all_lifetimes(&self) -> BTreeMap<Lifetime, Option<Attribute>> {
        let mut lifetimes = self.lifetimes.body.clone();
        let mut add_lifetimes = |lts: &BTreeMap<Lifetime, Option<Attribute>>| {
            for (lt, new_cfg) in lts {
                match lifetimes.entry(lt.to_owned()) {
                    Entry::Vacant(v) => {
                        v.insert(new_cfg.to_owned());
                    }
                    Entry::Occupied(mut o) => {
                        let cfg = o.get_mut();
                        *cfg = Option::zip(cfg.as_ref(), new_cfg.as_ref())
                            .map(|(a, b)| any_cfg([a, b].iter().copied()));
                    }
                }
            }
        };

        add_lifetimes(&self.lifetimes.path);
        add_lifetimes(&self.lifetimes.query);
        add_lifetimes(&self.lifetimes.header);

        lifetimes
    }

    pub(super) fn expand(
        &self,
        metadata: &Metadata,
        error_ty: &TokenStream,
        ruma_api: &TokenStream,
    ) -> TokenStream {
        let ruma_api_macros = quote! { #ruma_api::exports::ruma_api_macros };
        let ruma_serde = quote! { #ruma_api::exports::ruma_serde };
        let serde = quote! { #ruma_api::exports::serde };

        let docs = format!(
            "Data for a request to the `{}` API endpoint.\n\n{}",
            metadata.name.value(),
            metadata.description.value(),
        );
        let struct_attributes = &self.attributes;

        let method = &metadata.method;
        let path = &metadata.path;
        let auth_attributes = metadata.authentication.iter().map(|field| {
            let attrs = &field.attrs;
            let value = &field.value;

            // TODO!
            quote! { #[cfg_attr()] }
        });

        let lifetimes = self.all_lifetimes().iter().map(|(lt, attr)| quote! { #attr #lt });
        let fields = &self.fields;

        quote! {
            #[doc = #docs]
            #[derive(
                Clone,
                Debug,
                #ruma_api_macros::Request,
                #ruma_serde::Outgoing,
                #ruma_serde::_FakeDeriveSerde,
            )]
            #[cfg_attr(not(feature = "unstable-exhaustive-types"), non_exhaustive)]
            #[incoming_derive(!Deserialize)]
            #[ruma_api(
                method = #method,
                path = #path,
                error_ty = #error_ty,
            )]
            #( #auth_attributes )*
            #( #struct_attributes )*
            pub struct Request< #(#lifetimes),* > {
                #(#fields),*
            }
        }
    }
}
