#![cfg(feature = "codegen")]

//! Code generation.
//!
//! This module is useful for generating tables programmatically in `build.rs`.
//!
//! Scalars, tuples, strings, and hash tables can be codegen-ed. For custom types, you'll need to
//! implement the [`Codegen`] trait manually. Note that values can be serialized and deserialized on
//! different platforms (in case of cross-compilation) and with different crate features. Your
//! codegen logic might need to take this into consideration.
//!
//! <div class="warning">
//!
//! Runtime-constructed hash tables own the values and store them in a [`Vec`]. [`Codegen`] doesn't
//! try to guess your intentions, so the generated code will create a vector, even in a `const`
//! context. To prevent this, call `.borrow()` on the hash table beforehand, so that a reference to
//! a `static` is generated instead. This is not the default behavior, as runtime initialization is
//! necessary if the value cannot be constructed in `const`.
//!
//! </div>
//!
//!
//! # Example
//!
//! ```rust
//! # fn main() -> std::io::Result<()> {
//! // Create a hash map
//! let map: h::Map<i32, i32> = h::Map::from_entries(vec![(1, 2), (3, 4)]);
//!
//! // Convert to code
//! let code = h::codegen::CodeGenerator::new().generate(&map.borrow());
//!
//! // `code` can now be saved to an `.rs` file and then loaded with `include!`
//! # Ok(())
//! # }
//! ```

use super::BorrowedOrOwnedSlice;
use alloc::borrow::{Cow, ToOwned};
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use proc_macro2::{Ident, Literal, TokenStream, TokenTree};
use quote::{format_ident, quote};
use std::collections::{HashMap, HashSet};

/// Code generator.
pub struct CodeGenerator {
    crate_paths: HashMap<String, TokenStream>,
    path_to_alias: HashMap<String, Ident>,
    aliases: HashSet<String>,
    alloc_wanted: bool,
}

impl CodeGenerator {
    /// Create a code generator with default settings.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            crate_paths: HashMap::from([("h".into(), quote!(::h))]),
            path_to_alias: HashMap::new(),
            aliases: HashSet::new(),
            alloc_wanted: false,
        }
    }

    /// Configure name-to-path mapping for crates.
    ///
    /// By default, `h` is mapped to `::h`. Reconfiguring this might be necessary if you want to
    /// generate code with `h` from a proc-macro.
    #[inline]
    pub fn with_crate(&mut self, name: &str, path: TokenStream) {
        self.crate_paths.insert(name.into(), path);
    }

    /// Turn a value into code.
    #[inline]
    #[allow(clippy::missing_panics_doc, reason = "false positive")]
    pub fn generate<T: ?Sized + Codegen>(mut self, value: &T) -> TokenStream {
        let value = self.piece(value);

        let mut crate_paths = core::mem::take(&mut self.crate_paths);
        let extern_crate_alloc = (self.alloc_wanted
            && crate_paths
                .insert("alloc".to_owned(), quote!(_Alloc))
                .is_none())
        .then_some(quote!(
            extern crate alloc as _Alloc;
        ));

        let mut uses = Vec::new();
        for (path, alias) in core::mem::take(&mut self.path_to_alias) {
            let mut components = path.split("::");

            let crate_name = components.next().unwrap();
            let crate_path = if let Some(crate_path) = crate_paths.get(crate_name) {
                crate_path.clone()
            } else {
                let ident = format_ident!("{crate_name}");
                quote!(:: #ident)
            };
            let components = components.map(|component| format_ident!("{component}"));

            uses.push(quote!(use #crate_path #(:: #components)* as #alias;));
        }

        quote!(
            {
                let __codegen_value = #value;
                #extern_crate_alloc
                #(#uses)*
                __codegen_value
            }
        )
    }

    /// Turn a value into a recursively useable piece of code.
    #[inline]
    pub fn piece<T: ?Sized + Codegen>(&mut self, piece: &T) -> TokenStream {
        piece.generate_piece(self)
    }

    /// Resolve a path.
    ///
    /// The input string must look like `crate::path::inside::it`, or just `crate`. Semantically,
    /// this path must be `use`able. This is different from using the path directly for two reasons:
    ///
    /// - This method resolves crates according to the paths configured by
    ///   [`CodeGenerator::with_crate`].
    /// - This method replaces long paths with short aliases imported just once with `use`, reducing
    ///   code size.
    #[inline]
    #[allow(
        clippy::maybe_infinite_iter,
        clippy::missing_panics_doc,
        reason = "false positive"
    )]
    pub fn path(&mut self, path: &str) -> TokenStream {
        if let Some(alias) = self.path_to_alias.get(path) {
            return quote!(#alias);
        }

        if path.split_once("::").unwrap_or((path, "")).0 == "alloc" {
            self.alloc_wanted = true;
        }

        let mut alias = format!("__{}", path.rsplit_once("::").unwrap_or(("", path)).1);
        if self.aliases.contains(&alias) {
            alias = (2usize..)
                .map(|n| format!("{alias}{n}"))
                .find(|attempt| !self.aliases.contains(attempt))
                .unwrap();
        }

        let alias = format_ident!("{alias}");
        self.path_to_alias.insert(path.to_owned(), alias.clone());
        quote!(#alias)
    }
}

impl Default for CodeGenerator {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

/// Values that can be turned into code.
pub trait Codegen {
    /// Emit a piece of code corresponding to this value.
    ///
    /// This method is only supposed to be called recursively from [`Codegen`] implementations. Call
    /// [`CodeGenerator::generate`] to produce the complete code output for a single value.
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream;
}

macro_rules! literal {
    ($($ty:ty => $method:ident,)*) => {
        $(
            impl Codegen for $ty {
                #[inline]
                fn generate_piece(&self, _gen: &mut CodeGenerator) -> TokenStream {
                    TokenTree::Literal(Literal::$method(*self)).into()
                }
            }
        )*
    };
}

literal! {
    u8 => u8_suffixed,
    u16 => u16_suffixed,
    u32 => u32_suffixed,
    u64 => u64_suffixed,
    u128 => u128_suffixed,
    usize => usize_suffixed,
    i8 => i8_suffixed,
    i16 => i16_suffixed,
    i32 => i32_suffixed,
    i64 => i64_suffixed,
    i128 => i128_suffixed,
    isize => isize_suffixed,
    f32 => f32_suffixed,
    f64 => f64_suffixed,
}

impl Codegen for bool {
    #[inline]
    fn generate_piece(&self, _gen: &mut CodeGenerator) -> TokenStream {
        TokenTree::Ident(format_ident!("{self}")).into()
    }
}

impl<T: ?Sized> Codegen for core::marker::PhantomData<T> {
    #[inline]
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream {
        gen.path("core::marker::PhantomData")
    }
}

impl<T: Codegen> Codegen for alloc::vec::Vec<T> {
    #[inline]
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream {
        if let Some(bytes) = as_byte_slice(&**self) {
            let vec = gen.path("alloc::vec::Vec");
            let bytes = Literal::byte_string(bytes);
            quote!(#vec::from(#bytes))
        } else {
            let vec = gen.path("alloc::vec");
            let rest = gen.piece(&**self);
            quote!(#vec! #rest)
        }
    }
}

impl<T: Codegen> Codegen for [T] {
    #[inline]
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream {
        let elements = self.iter().map(|element| gen.piece(element));
        quote!([#(#elements),*])
    }
}

impl<T: ?Sized + Codegen> Codegen for &T {
    #[inline]
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream {
        if let Some(bytes) = as_byte_slice(*self) {
            let bytes = Literal::byte_string(bytes);
            quote!(#bytes as &[u8])
        } else {
            let target = gen.piece(*self);
            quote!(&#target)
        }
    }
}

fn as_byte_slice<T: ?Sized>(object: &T) -> Option<&[u8]> {
    if typeid::of::<T>() == typeid::of::<[u8]>() {
        Some(unsafe { core::mem::transmute_copy::<&T, &[u8]>(&object) })
    } else {
        None
    }
}

impl<T: ?Sized + ToOwned<Owned: Codegen> + Codegen> Codegen for Cow<'_, T> {
    #[inline]
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream {
        match self {
            Cow::Borrowed(b) => {
                let borrowed = gen.path("alloc::borrow::Cow::Borrowed");
                let target = gen.piece(b);
                quote!(#borrowed(#target))
            }
            Cow::Owned(o) => {
                let owned = gen.path("alloc::borrow::Cow::Owned");
                let target = gen.piece(o);
                quote!(#owned(#target))
            }
        }
    }
}

impl<T: Codegen> Codegen for BorrowedOrOwnedSlice<'_, T> {
    #[inline]
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream {
        match self {
            Self::Borrowed(b) => {
                let borrowed = gen.path("h::BorrowedOrOwnedSlice::Borrowed");
                let target = gen.piece(b);
                quote!(#borrowed(#target))
            }
            #[cfg(feature = "alloc")]
            Self::Owned(o) => {
                let owned = gen.path("h::BorrowedOrOwnedSlice::Owned");
                let target = gen.piece(o);
                quote!(#owned(#target))
            }
        }
    }
}

impl<T: Codegen> Codegen for Option<T> {
    #[inline]
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream {
        match &self {
            None => gen.path("core::option::Option::None"),
            Some(value) => {
                let some = gen.path("core::option::Option::Some");
                let value = gen.piece(value);
                quote!(#some(#value))
            }
        }
    }
}

impl Codegen for () {
    #[inline]
    fn generate_piece(&self, _gen: &mut CodeGenerator) -> TokenStream {
        quote!(())
    }
}

impl<T: Codegen, U: ?Sized + Codegen> Codegen for (T, U) {
    #[inline]
    fn generate_piece(&self, gen: &mut CodeGenerator) -> TokenStream {
        let a = gen.piece(&self.0);
        let b = gen.piece(&self.1);
        quote!((#a, #b))
    }
}
