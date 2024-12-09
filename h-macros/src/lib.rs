#![allow(clippy::std_instead_of_alloc, reason = "we're not in #[no_std]")]
#![allow(clippy::wildcard_enum_match_arm, reason = "too many false positives")]

mod coding;
mod constants;
mod hashing;
mod parse;
mod types;
mod values;

use self::{
    coding::encode_value,
    hashing::{with_hashable_keys, Callback},
    parse::{Context, MapArm, WithContext},
    types::TypePtr,
    values::{AsTypedValue, TypedValue, Value},
};
use h::{
    codegen::{CodeGenerator, Codegen},
    hash::PortableHash,
};
use proc_macro2::TokenStream;
use proc_macro_error2::{emit_call_site_error, emit_error, set_dummy};
use quote::{quote, ToTokens};
use std::collections::HashMap;
use syn::{parse_macro_input, spanned::Spanned, Expr};

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

struct PassThrough(TokenStream);

impl Codegen for PassThrough {
    fn generate_piece(&self, _gen: &mut CodeGenerator) -> TokenStream {
        self.0.clone()
    }
}

struct Ascribe<T> {
    value: T,
    ty: TokenStream,
}

impl<T: Codegen> Codegen for Ascribe<T> {
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream {
        let value = gen.piece(&self.value);
        let identity = gen.path("core::convert::identity");
        let ty = &self.ty;
        quote!(#identity::<#ty>(#value))
    }
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
    let value = gen.generate(&Ascribe { value, ty });
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
    set_dummy_with_ty(&quote!(::h::Map<_, _>));

    let input = parse_macro_input!(item as WithContext<MapArm>);

    let key_type = if let Some(ty) = &input.context.key_type {
        ty.to_token_stream()
    } else {
        quote!(_)
    };
    set_dummy_with_ty(&quote!(::h::Map<#key_type, _>));

    let Ok((inferred_key_type, encoded_keys)) =
        parse_keys(&input.context, input.elements.iter().map(|arm| &arm.key))
    else {
        return quote! {}.into();
    };

    struct Cb<'a> {
        input: &'a WithContext<MapArm>,
        key_type: TokenStream,
    }

    impl Callback for Cb<'_> {
        type Output = TokenStream;

        fn call_once<Key: PortableHash + Codegen>(
            self,
            keys: impl Iterator<Item = Key>,
        ) -> TokenStream {
            generate(
                &self.input.context,
                h::Map::<Key, PassThrough>::from_entries(
                    keys.zip(
                        self.input
                            .elements
                            .iter()
                            .map(|arm| PassThrough(arm.value.to_token_stream())),
                    )
                    .collect(),
                ),
                |gen| {
                    let map = gen.path("h::Map");
                    let key_type = self.key_type;
                    quote!(#map<#key_type, _>)
                },
            )
        }
    }

    with_hashable_keys(
        encoded_keys
            .into_iter()
            .zip(input.elements.iter().map(|arm| arm.key.to_token_stream())),
        &inferred_key_type,
        Cb {
            input: &input,
            key_type,
        },
    )
    .into()
}

#[proc_macro_error2::proc_macro_error(proc_macro_hack)]
#[proc_macro]
pub fn set(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    set_dummy_with_ty(&quote!(::h::Set<_>));

    let input = parse_macro_input!(item as WithContext<Expr>);

    if let Some(token) = &input.context.mutability {
        emit_error!(token, "`mut;` is not supported on sets");
    }

    let element_type = if let Some(ty) = &input.context.key_type {
        ty.to_token_stream()
    } else {
        quote!(_)
    };
    set_dummy_with_ty(&quote!(::h::Set<#element_type>));

    let Ok((inferred_element_type, encoded_elements)) =
        parse_keys(&input.context, input.elements.iter())
    else {
        return quote! {}.into();
    };

    struct Cb<'a> {
        input: &'a WithContext<Expr>,
        element_type: TokenStream,
    }

    impl Callback for Cb<'_> {
        type Output = TokenStream;

        fn call_once<Element: PortableHash + Codegen>(
            self,
            elements: impl Iterator<Item = Element>,
        ) -> TokenStream {
            generate(
                &self.input.context,
                h::Set::<Element>::from_elements(elements.collect()),
                |gen| {
                    let set_ty = gen.path("h::Set");
                    let element_type = self.element_type;
                    quote!(#set_ty<#element_type>)
                },
            )
        }
    }

    with_hashable_keys(
        encoded_elements
            .into_iter()
            .zip(input.elements.iter().map(Expr::to_token_stream)),
        &inferred_element_type,
        Cb {
            input: &input,
            element_type,
        },
    )
    .into()
}

#[proc_macro_error2::proc_macro_error(proc_macro_hack)]
#[proc_macro]
pub fn phf(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    set_dummy_with_ty(&quote!(::h::Phf<_>));

    let input = parse_macro_input!(item as WithContext<Expr>);

    if let Some(token) = &input.context.mutability {
        emit_error!(token, "`mut;` is not supported on PHFs");
    }

    let key_type = if let Some(ty) = &input.context.key_type {
        ty.to_token_stream()
    } else {
        quote!(_)
    };
    set_dummy_with_ty(&quote!(::h::Phf<#key_type>));

    let Ok((inferred_key_type, encoded_keys)) = parse_keys(&input.context, input.elements.iter())
    else {
        return quote! {}.into();
    };

    struct Cb<'a> {
        input: &'a WithContext<Expr>,
        key_type: TokenStream,
    }

    impl Callback for Cb<'_> {
        type Output = TokenStream;

        fn call_once<Key: PortableHash + Codegen>(
            self,
            keys: impl ExactSizeIterator<Item = Key> + Clone,
        ) -> TokenStream {
            generate(&self.input.context, h::Phf::<Key>::from_keys(keys), |gen| {
                let phf_ty = gen.path("h::Phf");
                let key_type = self.key_type;
                quote!(#phf_ty<#key_type>)
            })
        }
    }

    with_hashable_keys(
        encoded_keys
            .into_iter()
            .zip(input.elements.iter().map(Expr::to_token_stream)),
        &inferred_key_type,
        Cb {
            input: &input,
            key_type,
        },
    )
    .into()
}
