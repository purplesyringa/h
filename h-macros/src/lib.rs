#![allow(clippy::std_instead_of_alloc, reason = "we're not in #[no_std]")]
#![allow(clippy::wildcard_enum_match_arm, reason = "too many false positives")]

mod codegen;
mod coding;
mod constants;
mod parse;
mod types;
mod values;

use codegen::PassThrough;
use coding::{encode_value, EncodedValue};
use h::codegen::CodeGenerator;
use proc_macro2::TokenStream;
use proc_macro_error2::{emit_call_site_error, emit_error, set_dummy};
use quote::{quote, ToTokens};
use std::collections::HashMap;
use std::rc::Rc;
use syn::{parse_macro_input, Expr};
use types::TypePtr;
use values::{evaluate_syn_expr, TypedValue, Value};

fn hash_keys<'a>(
    context: &parse::Context,
    keys: impl Iterator<Item = &'a Expr> + Clone,
) -> Result<impl Iterator<Item = EncodedValue>, ()> {
    let mut key_type = if let Some(ref ty) = context.key_type {
        TypePtr::from_syn_type(ty)
    } else {
        TypePtr::Infer
    };

    let mut parsed_keys = Vec::new();
    for key in keys.clone() {
        let TypedValue { value, ty } = evaluate_syn_expr(key);
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
        if context.key_type.is_some() {
            emit_call_site_error!("type annotations needed\nspecify the key type manually by elaborating the `for {};` annotation\nnote: integer sizes need to be specified exactly", key_type);
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
            let (encoded, value_success) = encode_value(&key, &key_type);
            failed |= !value_success;
            encoded
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

    let key_type = Rc::new(key_type);
    Ok(encoded_keys
        .into_iter()
        .enumerate()
        .map(move |(i, data)| EncodedValue {
            ty: Rc::clone(&key_type),
            data,
            tokens: keys.clone().nth(i).to_token_stream(),
        }))
}

fn set_dummy_with_ty(ty: &TokenStream) {
    // Stupid-ass warnings
    set_dummy(quote! {
        if false {
            proc_macro_call!();
        }

        macro_rules! unimplemented {
            () => {
                ::h::conjure::<#ty>()
            };
        }
    });
}

// This option does not enable the `proc_macro_hack` crate. It only tweaks the error output to be
// valid in expression position.
#[proc_macro_error2::proc_macro_error(proc_macro_hack)]
#[proc_macro]
pub fn map(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    set_dummy_with_ty(&quote!(::h::Map<_, _>));

    let input = parse_macro_input!(item as parse::WithContext<parse::MapArm>);

    let key_type = if let Some(ty) = &input.context.key_type {
        ty.to_token_stream()
    } else {
        quote!(_)
    };
    set_dummy_with_ty(&quote!(::h::Map<#key_type, _>));

    let Ok(keys) = hash_keys(&input.context, input.elements.iter().map(|arm| &arm.key)) else {
        return quote! {}.into();
    };

    let map: h::Map<EncodedValue, PassThrough> = h::Map::from_entries(
        keys.zip(
            input
                .elements
                .iter()
                .map(|arm| PassThrough(arm.value.to_token_stream())),
        )
        .collect(),
    );

    let mut gen = CodeGenerator::new();
    if let Some(path) = input.context.h_crate {
        gen.set_crate("h", path.into_token_stream());
    }

    let map = gen.generate(&map);
    quote! {{
        let map: ::h::Map<#key_type, _> = #map;
        map
    }}
    .into()
}
