#![expect(clippy::wildcard_enum_match_arm, reason = "too many false positives")]

mod coding;
mod constants;
mod hashing;
mod parse;
mod types;
mod values;

use self::{
    coding::encode_value,
    hashing::HashableValue,
    parse::{Context, MapArm, WithContext},
    types::TypePtr,
    values::{AsTypedValue, TypedValue, Value},
};
use h::codegen::{Ascribe, CodeGenerator, Codegen, PassThrough};
use proc_macro2::TokenStream;
use proc_macro_error2::{emit_call_site_error, emit_error, set_dummy};
use quote::{quote, ToTokens};
use std::collections::HashMap;
use syn::{parse_macro_input, spanned::Spanned, Expr};

#[expect(clippy::needless_pass_by_value, reason = "needlessly complicates API")]
fn parse_keys<'a>(
    context: &Context,
    keys: impl Iterator<Item = &'a Expr> + Clone,
) -> Result<(TypePtr, Vec<Vec<u8>>), ()> {
    let mut key_type = if let Some(ref ty) = context.key_type {
        TypePtr::from_syn_type(ty)
    } else {
        TypePtr::Infer
    };

    let mut parsed_keys = Vec::new();
    for key in keys.clone() {
        let TypedValue { value, ty } = key.as_typed_value(key.span());
        let mut failed = false;
        key_type.unify_with(ty, &mut |e| {
            e.emit_inference();
            failed = true;
        });
        if failed {
            key_type = TypePtr::Inconsistent;
        }
        parsed_keys.push(value);
    }
    if key_type.has_inconsistencies() || parsed_keys.iter().any(Value::has_inconsistencies) {
        // Diagnostics have already been emitted
        return Err(());
    }

    if key_type.has_infer() {
        if let Some(syn_key_type) = &context.key_type {
            emit_error!(syn_key_type, "type annotations needed\nspecify the key type manually by elaborating the `for {};` annotation\nnote: integer sizes need to be specified exactly", key_type);
        } else if let Some(key) = keys.clone().next() {
            emit_error!(key, "type annotations needed\nspecify the key type manually by prepending `for {};` to the macro input\nnote: integer sizes need to be specified exactly", key_type);
        } else {
            emit_call_site_error!("type annotations needed\nspecify the key type manually by prepending `for {};` to the macro input\nnote: integer sizes need to be specified exactly", key_type);
        }
        return Err(());
    }

    // Settle the literals, simplify the format, and reduce memory usage before starting PHF
    // computation.
    let mut failed = false;
    let encoded_keys: Vec<Vec<u8>> = parsed_keys
        .into_iter()
        .map(|key| {
            if let Ok(encoded) = encode_value(&key, &key_type) {
                encoded
            } else {
                failed = true;
                Vec::new()
            }
        })
        .collect();
    if failed {
        // Diagnostics have already been emitted
        return Err(());
    }

    let mut map = HashMap::new();
    for (i, key) in encoded_keys.iter().enumerate() {
        if let Some(j) = map.insert(key, i) {
            let old = keys.clone().nth(j);
            let new = keys.clone().nth(i);
            emit_error!(old, "this key is equivalent to...");
            emit_error!(new, "...this key");
            return Err(());
        }
    }

    Ok((key_type, encoded_keys))
}

fn set_dummy_with_ty(ty: &TokenStream) {
    // Stupid-ass warnings
    set_dummy(quote! {
        if false {
            proc_macro_call!();
        }

        macro_rules! unimplemented {
            () => {{
                fn conjure<T>() -> T {
                    unreachable!();
                }
                conjure::<#ty>()
            }};
        }
    });
}

fn generate<T: Codegen>(
    context: &Context,
    value: T,
    ty: impl FnOnce(&mut CodeGenerator) -> TokenStream,
) -> TokenStream {
    let mut gen = CodeGenerator::new();
    if let Some(path) = &context.h_crate {
        gen.set_crate("h", path.to_token_stream());
    }
    gen.set_mutability(context.mutability.is_some());

    let ty = ty(&mut gen);
    let value = gen.generate(&Ascribe::new(value, ty));
    if context.mutability.is_some() {
        value
    } else {
        quote!(&#value)
    }
}

// `proc_macro_error(proc_macro_hack)` does not enable the `proc_macro_hack` crate. It only tweaks
// the error output to be valid in expression position.

#[proc_macro_error2::proc_macro_error(proc_macro_hack)]
#[proc_macro]
pub fn map(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Depending on whether `mut;` is present, we either need to emit `&Map` or `Map`. If parsing
    // fails, we have no idea what's more correct to emit, so let's just emit `_` instead.
    set_dummy_with_ty(&quote!(_));

    let input = parse_macro_input!(item as WithContext<MapArm>);

    if input.context.no_alloc {
        if let Some(token) = &input.context.mutability {
            emit_error!(
                token,
                "`mut;` requires the `h` crate feature `alloc` to be enabled",
            );
        }
    }

    let key_type = if let Some(ty) = &input.context.key_type {
        ty.to_token_stream()
    } else {
        quote!(_)
    };
    if input.context.mutability.is_some() {
        set_dummy_with_ty(&quote!(::h::Map<#key_type, _>));
    } else {
        set_dummy_with_ty(&quote!(&::h::Map<#key_type, _>));
    }

    let Ok((inferred_key_type, encoded_keys)) =
        parse_keys(&input.context, input.elements.iter().map(|arm| &arm.key))
    else {
        return quote! {}.into();
    };

    let map = h::Map::from_entries(
        encoded_keys
            .into_iter()
            .zip(input.elements)
            .map(|(data, element)| {
                (
                    HashableValue::new(&inferred_key_type, data, element.key.to_token_stream()),
                    PassThrough::new(element.value.to_token_stream()),
                )
            })
            .collect(),
    );

    generate(&input.context, map, |gen| {
        let map = gen.path("h::Map");
        quote!(#map<#key_type, _>)
    })
    .into()
}

#[proc_macro_error2::proc_macro_error(proc_macro_hack)]
#[proc_macro]
pub fn set(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    set_dummy_with_ty(&quote!(&::h::Set<_>));

    let input = parse_macro_input!(item as WithContext<Expr>);

    if let Some(token) = &input.context.mutability {
        emit_error!(token, "`mut;` is not supported on sets");
    }

    let element_type = if let Some(ty) = &input.context.key_type {
        ty.to_token_stream()
    } else {
        quote!(_)
    };
    set_dummy_with_ty(&quote!(&::h::Set<#element_type>));

    let Ok((inferred_element_type, encoded_elements)) =
        parse_keys(&input.context, input.elements.iter())
    else {
        return quote! {}.into();
    };

    let set = h::Set::from_elements(
        encoded_elements
            .into_iter()
            .zip(input.elements)
            .map(|(data, element)| {
                HashableValue::new(&inferred_element_type, data, element.to_token_stream())
            })
            .collect(),
    );

    generate(&input.context, set, |gen| {
        let set_ty = gen.path("h::Set");
        quote!(#set_ty<#element_type>)
    })
    .into()
}

#[proc_macro_error2::proc_macro_error(proc_macro_hack)]
#[proc_macro]
pub fn phf(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    set_dummy_with_ty(&quote!(&::h::Phf<_>));

    let input = parse_macro_input!(item as WithContext<Expr>);

    if let Some(token) = &input.context.mutability {
        emit_error!(token, "`mut;` is not supported on PHFs");
    }

    let key_type = if let Some(ty) = &input.context.key_type {
        ty.to_token_stream()
    } else {
        quote!(_)
    };
    set_dummy_with_ty(&quote!(&::h::Phf<#key_type>));

    let Ok((inferred_key_type, encoded_keys)) = parse_keys(&input.context, input.elements.iter())
    else {
        return quote! {}.into();
    };

    let phf = h::Phf::from_keys(encoded_keys.into_iter().zip(input.elements).map(
        |(data, element)| HashableValue::new(&inferred_key_type, data, element.to_token_stream()),
    ));

    generate(&input.context, phf, |gen| {
        let phf_ty = gen.path("h::Phf");
        quote!(#phf_ty<#key_type>)
    })
    .into()
}
