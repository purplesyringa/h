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
//!
//! # Mutability and `const`
//!
//! The generated code can be tweaked for two use cases: if the resulting value should be mutable or
//! should stay immutable. Here, mutability includes interior mutability.
//!
//! By default, immutable objects are generated. They can be used in a `const` context and don't
//! require dynamic allocations if the types support that (i.e. unless `Vec`, `Box`, or other types
//! with non-`const` constructors are used). Attempting to mutate such objects may work, fail to
//! compile, panic, or lead to any other outcome (but not cause UB).
//!
//! If `set_mutability(true)` is called on the [`CodeGenerator`], mutable objects are generated.
//! Such objects can be placed to a mutable variable and modified, and `Cell`s inside such objects
//! may safely be interacted with. However, to hold this property, heap allocations might be
//! necessary, making the code not `const`-friendly.
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
//! let code = h::codegen::CodeGenerator::new().generate(&map);
//!
//! // TODO: Save code to a file
//! # Ok(())
//! # }
//! ```
//!
//! ```ignore
//! // Constant immutable map
//! const MAP: h::Map<i32, i32> = include!("path/to/generated/code.rs");
//! ```

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
    mutability: bool,
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
            mutability: false,
        }
    }

    /// Generate code for a mutable object.
    ///
    /// This is necessary if you want to modify the resulting object, including via interior
    /// mutability. If this option is disabled, the generated objects might reference constant
    /// static data, but are usable in a `const` context.
    #[inline]
    pub fn set_mutability(&mut self, mutability: bool) {
        self.mutability = mutability;
    }

    /// Configure name-to-path mapping for crates.
    ///
    /// By default, `h` is mapped to `::h`. Reconfiguring this might be necessary if you want to
    /// generate code with `h` from a proc-macro.
    #[inline]
    pub fn set_crate(&mut self, name: &str, path: TokenStream) {
        self.crate_paths.insert(name.into(), path);
    }

    /// Check if generating code for a mutable context.
    ///
    /// See the documentation for [`set_mutability`](Self::set_mutability) for an explanation.
    #[inline]
    #[must_use]
    pub const fn mutability(&self) -> bool {
        self.mutability
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
    ///   [`CodeGenerator::set_crate`].
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
    u8 => u8_unsuffixed,
    u16 => u16_unsuffixed,
    u32 => u32_unsuffixed,
    u64 => u64_unsuffixed,
    u128 => u128_unsuffixed,
    usize => usize_unsuffixed,
    i8 => i8_unsuffixed,
    i16 => i16_unsuffixed,
    i32 => i32_unsuffixed,
    i64 => i64_unsuffixed,
    i128 => i128_unsuffixed,
    isize => isize_unsuffixed,
    f32 => f32_unsuffixed,
    f64 => f64_unsuffixed,
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
