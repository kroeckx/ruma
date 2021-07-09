use std::{
    collections::btree_map::{BTreeMap, Entry},
    mem,
};

use syn::{
    braced,
    parse::{Parse, ParseStream},
    spanned::Spanned,
    visit::Visit,
    Attribute, Field, Ident, Lifetime, Token, Type,
};

use super::{
    attribute::{Meta, MetaNameValue},
    request::RequestLifetimes,
    response::{ResponseField, ResponseFieldKind},
    Api, Metadata, Request, Response,
};
use crate::util::{all_cfgs, any_cfg};

mod kw {
    use syn::custom_keyword;

    custom_keyword!(error);
    custom_keyword!(request);
    custom_keyword!(response);
}

impl Parse for Api {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let metadata: Metadata = input.parse()?;

        let req_attrs = input.call(Attribute::parse_outer)?;
        let (request, attributes) = if input.peek(kw::request) {
            let request = parse_request(input, req_attrs)?;
            let after_req_attrs = input.call(Attribute::parse_outer)?;

            (Some(request), after_req_attrs)
        } else {
            // There was no `request` field so the attributes are for `response`
            (None, req_attrs)
        };

        let response = if input.peek(kw::response) {
            Some(parse_response(input, attributes)?)
        } else if !attributes.is_empty() {
            return Err(syn::Error::new_spanned(
                &attributes[0],
                "attributes are not supported on the error type",
            ));
        } else {
            None
        };

        let error_ty = input
            .peek(kw::error)
            .then(|| {
                let _: kw::error = input.parse()?;
                let _: Token![:] = input.parse()?;

                input.parse()
            })
            .transpose()?;

        // TODO!
        /*if let Some(req) = &request {
            let newtype_body_field = req.newtype_body_field();
            if metadata.method == "GET" && (req.has_body_fields() || newtype_body_field.is_some()) {
                let mut combined_error: Option<syn::Error> = None;
                let mut add_error = |field| {
                    let error =
                        syn::Error::new_spanned(field, "GET endpoints can't have body fields");
                    if let Some(combined_error_ref) = &mut combined_error {
                        combined_error_ref.combine(error);
                    } else {
                        combined_error = Some(error);
                    }
                };

                for field in req.body_fields() {
                    add_error(field);
                }

                if let Some(field) = newtype_body_field {
                    add_error(field);
                }

                return Err(combined_error.unwrap());
            }
        }*/

        Ok(Self { metadata, request, response, error_ty })
    }
}

fn parse_request(input: ParseStream<'_>, attributes: Vec<Attribute>) -> syn::Result<Request> {
    let request_kw: kw::request = input.parse()?;
    let _: Token![:] = input.parse()?;
    let fields;
    braced!(fields in input);

    //let mut newtype_body_field = None;
    //let mut query_map_field = None;
    let mut lifetimes = RequestLifetimes::default();

    let fields = fields.parse_terminated::<Field, Token![,]>(Field::parse_named)?;

    // TODO!
    //if newtype_body_field.is_some() && fields.iter().any(|f| f.is_body()) {
    //    // TODO: highlight conflicting fields,
    //    return Err(syn::Error::new_spanned(
    //        request_kw,
    //        "Can't have both a newtype body field and regular body fields",
    //    ));
    //}
    //
    //if query_map_field.is_some() && fields.iter().any(|f| f.is_query()) {
    //    return Err(syn::Error::new_spanned(
    //        // TODO: raw,
    //        request_kw,
    //        "Can't have both a query map field and regular query fields",
    //    ));
    //}
    //
    // // TODO when/if `&[(&str, &str)]` is supported remove this
    // if query_map_field.is_some() && !lifetimes.query.is_empty() {
    //     return Err(syn::Error::new_spanned(
    //         request_kw,
    //         "Lifetimes are not allowed for query_map fields",
    //     ));
    // }

    Ok(Request { attributes, fields, lifetimes })
}

// TODO!
fn parse_response(input: ParseStream<'_>, attributes: Vec<Attribute>) -> syn::Result<Response> {
    let response_kw: kw::response = input.parse()?;
    let _: Token![:] = input.parse()?;
    let fields;
    braced!(fields in input);

    let mut newtype_body_field = None;

    let fields: Vec<_> = fields
        .parse_terminated::<Field, Token![,]>(Field::parse_named)?
        .into_iter()
        .map(|mut field| {
            if has_lifetime(&field.ty) {
                return Err(syn::Error::new(
                    field.ident.span(),
                    "Lifetimes on Response fields cannot be supported until GAT are stable",
                ));
            }

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
                        s @ "body" | s @ "raw_body" => req_res_meta_word(
                            s,
                            &field,
                            &mut newtype_body_field,
                            ResponseFieldKind::NewtypeBody,
                            ResponseFieldKind::NewtypeRawBody,
                        )?,
                        _ => {
                            return Err(syn::Error::new_spanned(
                                ident,
                                "Invalid #[ruma_api] argument with value, expected `body`",
                            ));
                        }
                    },
                    Meta::NameValue(MetaNameValue { name, value }) => {
                        req_res_name_value(name, value, &mut header, ResponseFieldKind::Header)?
                    }
                });
            }

            Ok(match field_kind.unwrap_or(ResponseFieldKind::Body) {
                ResponseFieldKind::Body => ResponseField::Body(field),
                ResponseFieldKind::Header => {
                    ResponseField::Header(field, header.expect("missing header name"))
                }
                ResponseFieldKind::NewtypeBody => ResponseField::NewtypeBody(field),
                ResponseFieldKind::NewtypeRawBody => ResponseField::NewtypeRawBody(field),
            })
        })
        .collect::<syn::Result<_>>()?;

    if newtype_body_field.is_some() && fields.iter().any(|f| f.is_body()) {
        // TODO: highlight conflicting fields,
        return Err(syn::Error::new_spanned(
            response_kw,
            "Can't have both a newtype body field and regular body fields",
        ));
    }

    Ok(Response { attributes, fields })
}

fn has_lifetime(ty: &Type) -> bool {
    struct Visitor {
        found_lifetime: bool,
    }

    impl<'ast> Visit<'ast> for Visitor {
        fn visit_lifetime(&mut self, _lt: &'ast Lifetime) {
            self.found_lifetime = true;
        }
    }

    let mut vis = Visitor { found_lifetime: false };
    vis.visit_type(ty);
    vis.found_lifetime
}

fn collect_lifetime_idents(lifetimes: &mut BTreeMap<Lifetime, Option<Attribute>>, field: &Field) {
    struct Visitor<'lt> {
        field_cfg: Option<Attribute>,
        lifetimes: &'lt mut BTreeMap<Lifetime, Option<Attribute>>,
    }

    impl<'ast> Visit<'ast> for Visitor<'_> {
        fn visit_lifetime(&mut self, lt: &'ast Lifetime) {
            match self.lifetimes.entry(lt.clone()) {
                Entry::Vacant(v) => {
                    v.insert(self.field_cfg.clone());
                }
                Entry::Occupied(mut o) => {
                    let lifetime_cfg = o.get_mut();

                    // If at least one field uses this lifetime and has no cfg attribute, we don't
                    // need a cfg attribute for the lifetime either.
                    *lifetime_cfg = Option::zip(lifetime_cfg.as_ref(), self.field_cfg.as_ref())
                        .map(|(a, b)| any_cfg([a, b].iter().copied()));
                }
            }
        }
    }

    let field_cfg = (!field.attrs.is_empty())
        .then(|| all_cfgs(field.attrs.iter().filter(|a| a.path.is_ident("cfg"))));

    Visitor { lifetimes, field_cfg }.visit_type(&field.ty)
}

fn req_res_meta_word<T>(
    attr_kind: &str,
    field: &Field,
    newtype_body_field: &mut Option<Field>,
    body_field_kind: T,
    raw_field_kind: T,
) -> syn::Result<T> {
    if let Some(f) = &newtype_body_field {
        let mut error = syn::Error::new_spanned(field, "There can only be one newtype body field");
        error.combine(syn::Error::new_spanned(f, "Previous newtype body field"));
        return Err(error);
    }

    *newtype_body_field = Some(field.clone());
    Ok(match attr_kind {
        "body" => body_field_kind,
        "raw_body" => raw_field_kind,
        _ => unreachable!(),
    })
}

fn req_res_name_value<T>(
    name: Ident,
    value: Ident,
    header: &mut Option<Ident>,
    field_kind: T,
) -> syn::Result<T> {
    if name != "header" {
        return Err(syn::Error::new_spanned(
            name,
            "Invalid #[ruma_api] argument with value, expected `header`",
        ));
    }

    *header = Some(value);
    Ok(field_kind)
}
