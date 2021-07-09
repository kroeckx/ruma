//! Functions to aid the `Api::to_tokens` method.

use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{parse_quote, AttrStyle, Attribute, Ident};

pub(crate) fn all_cfgs<'a>(cfgs: impl IntoIterator<Item = &'a Attribute>) -> Attribute {
    combined_cfgs(cfgs, format_ident!("all"))
}

pub(crate) fn any_cfg<'a>(cfgs: impl IntoIterator<Item = &'a Attribute>) -> Attribute {
    combined_cfgs(cfgs, format_ident!("any"))
}

fn combined_cfgs<'a>(cfgs: impl IntoIterator<Item = &'a Attribute>, combiner: Ident) -> Attribute {
    let sub_cfgs = cfgs.into_iter().map(|attr| {
        let meta = attr.parse_meta().expect("cfg attribute can be parsed to syn::Meta");
        match meta {
            syn::Meta::List(mut l) => {
                assert!(l.path.is_ident("cfg"), "expected cfg attributes only");
                assert_eq!(l.nested.len(), 1, "expected one item inside cfg()");

                l.nested.pop().unwrap().into_value()
            }
            _ => panic!("unexpected cfg syntax"),
        }
    });

    parse_quote! { #[cfg( #combiner( #(#sub_cfgs),* ) )] }
}

pub(crate) fn is_valid_endpoint_path(string: &str) -> bool {
    string.as_bytes().iter().all(|b| (0x21..=0x7E).contains(b))
}

pub(crate) fn import_ruma_api() -> TokenStream {
    if let Ok(FoundCrate::Name(name)) = crate_name("ruma-api") {
        let import = format_ident!("{}", name);
        quote! { ::#import }
    } else if let Ok(FoundCrate::Name(name)) = crate_name("ruma") {
        let import = format_ident!("{}", name);
        quote! { ::#import::api }
    } else if let Ok(FoundCrate::Name(name)) = crate_name("matrix-sdk") {
        let import = format_ident!("{}", name);
        quote! { ::#import::ruma::api }
    } else if let Ok(FoundCrate::Name(name)) = crate_name("matrix-sdk-appservice") {
        let import = format_ident!("{}", name);
        quote! { ::#import::ruma::api }
    } else {
        quote! { ::ruma_api }
    }
}

pub(crate) fn is_cfg_attribute(attr: &Attribute) -> bool {
    matches!(attr.style, AttrStyle::Outer) && attr.path.is_ident("cfg")
}
