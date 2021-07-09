use std::collections::BTreeSet;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, Field, Generics, Ident, Lifetime, LitStr};

use crate::{auth_scheme::AuthScheme, util};

mod incoming;
mod outgoing;

pub fn expand_derive_request(input: DeriveInput) -> syn::Result<TokenStream> {
    let req: Request = todo!();
    Ok(req.expand_all())
}

#[derive(Default)]
struct RequestLifetimes {
    pub body: BTreeSet<Lifetime>,
    pub path: BTreeSet<Lifetime>,
    pub query: BTreeSet<Lifetime>,
    pub header: BTreeSet<Lifetime>,
}

struct Request {
    generics: Generics,
    lifetimes: RequestLifetimes,
    fields: Vec<RequestField>,

    authentication: AuthScheme,
    method: Ident,
    path: LitStr,
    error_ty: TokenStream,
}

impl Request {
    /// Whether or not this request has any data in the HTTP body.
    fn has_body_fields(&self) -> bool {
        self.fields.iter().any(|field| field.is_body())
    }

    /// Whether or not this request has any data in HTTP headers.
    fn has_header_fields(&self) -> bool {
        self.fields.iter().any(|field| field.is_header())
    }

    /// Whether or not this request has any data in the URL path.
    fn has_path_fields(&self) -> bool {
        self.fields.iter().any(|field| field.is_path())
    }

    /// Whether or not this request has any data in the query string.
    fn has_query_fields(&self) -> bool {
        self.fields.iter().any(|field| field.is_query())
    }

    /// Produces an iterator over all the body fields.
    fn body_fields(&self) -> impl Iterator<Item = &Field> {
        self.fields.iter().filter_map(|field| field.as_body_field())
    }

    /// Whether any `body` field has a lifetime annotation.
    fn has_body_lifetimes(&self) -> bool {
        !self.lifetimes.body.is_empty()
    }

    /// Whether any `query` field has a lifetime annotation.
    fn has_query_lifetimes(&self) -> bool {
        !self.lifetimes.query.is_empty()
    }

    /// Whether any field has a lifetime.
    fn contains_lifetimes(&self) -> bool {
        !(self.lifetimes.body.is_empty()
            && self.lifetimes.path.is_empty()
            && self.lifetimes.query.is_empty()
            && self.lifetimes.header.is_empty())
    }

    /// The lifetimes on fields with the `query` attribute.
    fn query_lifetimes(&self) -> TokenStream {
        todo!("util::lifetime_decls(&self.lifetimes.query)")
    }

    /// The lifetimes on fields with the `body` attribute.
    fn body_lifetimes(&self) -> TokenStream {
        todo!("util::lifetime_decls(&self.lifetimes.body)")
    }

    /// Produces an iterator over all the header fields.
    fn header_fields(&self) -> impl Iterator<Item = &RequestField> {
        self.fields.iter().filter(|field| field.is_header())
    }

    /// Gets the number of path fields.
    fn path_field_count(&self) -> usize {
        self.fields.iter().filter(|field| field.is_path()).count()
    }

    /// Returns the body field.
    pub fn newtype_body_field(&self) -> Option<&Field> {
        self.fields.iter().find_map(RequestField::as_newtype_body_field)
    }

    /// Returns the body field.
    fn newtype_raw_body_field(&self) -> Option<&Field> {
        self.fields.iter().find_map(RequestField::as_newtype_raw_body_field)
    }

    /// Returns the query map field.
    fn query_map_field(&self) -> Option<&Field> {
        self.fields.iter().find_map(RequestField::as_query_map_field)
    }

    fn expand_all(&self) -> TokenStream {
        let ruma_api = util::import_ruma_api();
        let ruma_serde = quote! { #ruma_api::exports::ruma_serde };
        let serde = quote! { #ruma_api::exports::serde };

        let request_body_struct =
            if let Some(body_field) = self.fields.iter().find(|f| f.is_newtype_body()) {
                let field = Field { ident: None, colon_token: None, ..body_field.field().clone() };
                // Though we don't track the difference between new type body and body
                // for lifetimes, the outer check and the macro failing if it encounters
                // an illegal combination of field attributes, is enough to guarantee
                // `body_lifetimes` correctness.
                let (derive_deserialize, lifetimes) = if self.has_body_lifetimes() {
                    (TokenStream::new(), self.body_lifetimes())
                } else {
                    (quote!(#serde::Deserialize), TokenStream::new())
                };

                Some((derive_deserialize, quote! { #lifetimes (#field); }))
            } else if self.has_body_fields() {
                let fields = self.fields.iter().filter(|f| f.is_body());
                let (derive_deserialize, lifetimes) = if self.has_body_lifetimes() {
                    (TokenStream::new(), self.body_lifetimes())
                } else {
                    (quote!(#serde::Deserialize), TokenStream::new())
                };
                let fields = fields.map(RequestField::field);

                Some((derive_deserialize, quote! { #lifetimes { #(#fields),* } }))
            } else {
                None
            }
            .map(|(derive_deserialize, def)| {
                quote! {
                    /// Data in the request body.
                    #[derive(
                        Debug,
                        #ruma_serde::Outgoing,
                        #serde::Serialize,
                        #derive_deserialize
                    )]
                    struct RequestBody #def
                }
            });

        let request_query_struct = if let Some(f) = self.query_map_field() {
            let field = Field { ident: None, colon_token: None, ..f.clone() };
            let (derive_deserialize, lifetime) = if self.has_query_lifetimes() {
                (TokenStream::new(), self.query_lifetimes())
            } else {
                (quote!(#serde::Deserialize), TokenStream::new())
            };

            quote! {
                /// Data in the request's query string.
                #[derive(
                    Debug,
                    #ruma_serde::Outgoing,
                    #serde::Serialize,
                    #derive_deserialize
                )]
                struct RequestQuery #lifetime (#field);
            }
        } else if self.has_query_fields() {
            let fields = self.fields.iter().filter_map(RequestField::as_query_field);
            let (derive_deserialize, lifetime) = if self.has_query_lifetimes() {
                (TokenStream::new(), self.query_lifetimes())
            } else {
                (quote!(#serde::Deserialize), TokenStream::new())
            };

            quote! {
                /// Data in the request's query string.
                #[derive(
                    Debug,
                    #ruma_serde::Outgoing,
                    #serde::Serialize,
                    #derive_deserialize
                )]
                struct RequestQuery #lifetime {
                    #(#fields),*
                }
            }
        } else {
            TokenStream::new()
        };

        let outgoing_request_impl = self.expand_outgoing(&ruma_api);
        let incoming_request_impl = self.expand_incoming(&ruma_api);

        quote! {
            #outgoing_request_impl
            #incoming_request_impl
        }
    }
}

/* parse

fields.map(|mut field| {
    let mut field_kind = None;
    let mut header = None;

    for attr in mem::take(&mut field.attrs) {
        let meta = match Meta::from_attribute(&attr)? {
            Some(m) => m,
            None => {
                field.attrs.push(attr);
                continue;
            }
        };

        if field_kind.is_some() {
            return Err(syn::Error::new_spanned(
                attr,
                "There can only be one field kind attribute",
            ));
        }

        field_kind = Some(match meta {
            Meta::Word(ident) => match &ident.to_string()[..] {
                attr @ "body" | attr @ "raw_body" => req_res_meta_word(
                    attr,
                    &field,
                    &mut newtype_body_field,
                    RequestFieldKind::NewtypeBody,
                    RequestFieldKind::NewtypeRawBody,
                )?,
                "path" => RequestFieldKind::Path,
                "query" => RequestFieldKind::Query,
                "query_map" => {
                    if let Some(f) = &query_map_field {
                        let mut error = syn::Error::new_spanned(
                            field,
                            "There can only be one query map field",
                        );
                        error.combine(syn::Error::new_spanned(
                            f,
                            "Previous query map field",
                        ));
                        return Err(error);
                    }

                    query_map_field = Some(field.clone());
                    RequestFieldKind::QueryMap
                }
                _ => {
                    return Err(syn::Error::new_spanned(
                        ident,
                        "Invalid #[ruma_api] argument, expected one of \
                                `body`, `path`, `query`, `query_map`",
                    ));
                }
            },
            Meta::NameValue(MetaNameValue { name, value }) => {
                req_res_name_value(name, value, &mut header, RequestFieldKind::Header)?
            }
        });
    }

    match field_kind.unwrap_or(RequestFieldKind::Body) {
        RequestFieldKind::Header => collect_lifetime_idents(&mut lifetimes.header, &field),
        RequestFieldKind::Body => collect_lifetime_idents(&mut lifetimes.body, &field),
        RequestFieldKind::NewtypeBody => {
            collect_lifetime_idents(&mut lifetimes.body, &field)
        }
        RequestFieldKind::NewtypeRawBody => {
            collect_lifetime_idents(&mut lifetimes.body, &field)
        }
        RequestFieldKind::Path => collect_lifetime_idents(&mut lifetimes.path, &field),
        RequestFieldKind::Query => collect_lifetime_idents(&mut lifetimes.query, &field),
        RequestFieldKind::QueryMap => collect_lifetime_idents(&mut lifetimes.query, &field),
    }

    Ok(RequestField::new(field_kind.unwrap_or(RequestFieldKind::Body), field, header))
})
*/

/// The types of fields that a request can have.
enum RequestField {
    /// JSON data in the body of the request.
    Body(Field),

    /// Data in an HTTP header.
    Header(Field, Ident),

    /// A specific data type in the body of the request.
    NewtypeBody(Field),

    /// Arbitrary bytes in the body of the request.
    NewtypeRawBody(Field),

    /// Data that appears in the URL path.
    Path(Field),

    /// Data that appears in the query string.
    Query(Field),

    /// Data that appears in the query string as dynamic key-value pairs.
    QueryMap(Field),
}

impl RequestField {
    /// Creates a new `RequestField`.
    fn new(kind: RequestFieldKind, field: Field, header: Option<Ident>) -> Self {
        match kind {
            RequestFieldKind::Body => RequestField::Body(field),
            RequestFieldKind::Header => {
                RequestField::Header(field, header.expect("missing header name"))
            }
            RequestFieldKind::NewtypeBody => RequestField::NewtypeBody(field),
            RequestFieldKind::NewtypeRawBody => RequestField::NewtypeRawBody(field),
            RequestFieldKind::Path => RequestField::Path(field),
            RequestFieldKind::Query => RequestField::Query(field),
            RequestFieldKind::QueryMap => RequestField::QueryMap(field),
        }
    }

    /// Whether or not this request field is a body kind.
    fn is_body(&self) -> bool {
        matches!(self, RequestField::Body(..))
    }

    /// Whether or not this request field is a header kind.
    fn is_header(&self) -> bool {
        matches!(self, RequestField::Header(..))
    }

    /// Whether or not this request field is a newtype body kind.
    fn is_newtype_body(&self) -> bool {
        matches!(self, RequestField::NewtypeBody(..))
    }

    /// Whether or not this request field is a path kind.
    fn is_path(&self) -> bool {
        matches!(self, RequestField::Path(..))
    }

    /// Whether or not this request field is a query string kind.
    fn is_query(&self) -> bool {
        matches!(self, RequestField::Query(..))
    }

    /// Return the contained field if this request field is a body kind.
    fn as_body_field(&self) -> Option<&Field> {
        self.field_of_kind(RequestFieldKind::Body)
    }

    /// Return the contained field if this request field is a body kind.
    fn as_newtype_body_field(&self) -> Option<&Field> {
        self.field_of_kind(RequestFieldKind::NewtypeBody)
    }

    /// Return the contained field if this request field is a raw body kind.
    fn as_newtype_raw_body_field(&self) -> Option<&Field> {
        self.field_of_kind(RequestFieldKind::NewtypeRawBody)
    }

    /// Return the contained field if this request field is a query kind.
    fn as_query_field(&self) -> Option<&Field> {
        self.field_of_kind(RequestFieldKind::Query)
    }

    /// Return the contained field if this request field is a query map kind.
    fn as_query_map_field(&self) -> Option<&Field> {
        self.field_of_kind(RequestFieldKind::QueryMap)
    }

    /// Gets the inner `Field` value.
    fn field(&self) -> &Field {
        match self {
            RequestField::Body(field)
            | RequestField::Header(field, _)
            | RequestField::NewtypeBody(field)
            | RequestField::NewtypeRawBody(field)
            | RequestField::Path(field)
            | RequestField::Query(field)
            | RequestField::QueryMap(field) => field,
        }
    }

    /// Gets the inner `Field` value if it's of the provided kind.
    fn field_of_kind(&self, kind: RequestFieldKind) -> Option<&Field> {
        match (self, kind) {
            (RequestField::Body(field), RequestFieldKind::Body)
            | (RequestField::Header(field, _), RequestFieldKind::Header)
            | (RequestField::NewtypeBody(field), RequestFieldKind::NewtypeBody)
            | (RequestField::NewtypeRawBody(field), RequestFieldKind::NewtypeRawBody)
            | (RequestField::Path(field), RequestFieldKind::Path)
            | (RequestField::Query(field), RequestFieldKind::Query)
            | (RequestField::QueryMap(field), RequestFieldKind::QueryMap) => Some(field),
            _ => None,
        }
    }
}

/// The types of fields that a request can have, without their values.
#[derive(Clone, Copy, PartialEq, Eq)]
enum RequestFieldKind {
    Body,
    Header,
    NewtypeBody,
    NewtypeRawBody,
    Path,
    Query,
    QueryMap,
}
