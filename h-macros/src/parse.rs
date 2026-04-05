//! High-level macro input parsing.

use proc_macro_error2::emit_call_site_error;
use syn::{
    parse::{Parse, ParseStream, Result},
    punctuated::Punctuated,
    Expr, Path, Token, Type,
};

/// `key => value` arm of a `map!` macro.
#[derive(Debug)]
pub struct MapArm {
    /// The key part.
    pub key: Expr,
    /// The value part.
    pub value: Expr,
}

impl Parse for MapArm {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let key = input.parse::<Expr>()?;
        input.parse::<Token![=>]>()?;
        let value = input.parse::<Expr>()?;
        Ok(Self { key, value })
    }
}

/// Annotations in macro input.
///
/// This includes both user-supplied annotations like `for _;` and `mut;` and annotations generated
/// by the declarative macros in the `h` crate, like `crate _;`.
#[derive(Debug)]
pub struct Context {
    /// Whether using the `alloc` crate is banned.
    pub no_alloc: bool,
    /// The path to the `h` crate provided with `crate _;`.
    ///
    /// Used to generate paths to items inside `h` that work when the `map!` macro is invoked from
    /// another macro (e.g. `helper!` using `$crate::h::map!`) which is then in turn used in another
    /// crate.
    ///
    /// This is `None` if the annotation is omitted, i.e. if proc macros from `h_macros` are invoked
    /// directly rather than via the `h` declarative macros, in which case we emit a diagnostic but
    /// keep going.
    pub h_crate: Option<Path>,
    /// The key type hint provided with `for _;`.
    pub key_type: Option<Type>,
    /// The `mut;` annotation for the `map!` macro.
    pub mutability: Option<Token![mut]>,
}

/// Custom [`syn`] keywords.
mod kw {
    syn::custom_keyword!(no_alloc);
}

impl Parse for Context {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let no_alloc = if input.parse::<kw::no_alloc>().is_ok() {
            input.parse::<Token![;]>()?;
            true
        } else {
            false
        };

        let h_crate = if input.parse::<Token![crate]>().is_ok() {
            let path = input.parse::<Path>()?;
            input.parse::<Token![;]>()?;
            Some(path)
        } else {
            emit_call_site_error!("macros need to be imported from `h` instead of `h_macros`");
            None
        };

        let key_type = if input.parse::<Token![for]>().is_ok() {
            let ty = input.parse::<Type>()?;
            input.parse::<Token![;]>()?;
            Some(ty)
        } else {
            None
        };

        let mutability = if let Ok(token) = input.parse::<Token![mut]>() {
            input.parse::<Token![;]>()?;
            Some(token)
        } else {
            None
        };

        Ok(Self {
            no_alloc,
            h_crate,
            key_type,
            mutability,
        })
    }
}

/// Type alias for `Token![,]`.
///
/// This is necessary because using `Token![,]` inside structs with `#[derive]` attributes results
/// in "`derive` cannot be used on items with type macros".
type Comma = Token![,];

/// Parsed input containing context together with a sequence of `Element`.
#[derive(Debug)]
pub struct WithContext<Element> {
    /// The shared context.
    pub context: Context,
    /// Individual elements.
    pub elements: Punctuated<Element, Comma>,
}

impl<Element: Parse> Parse for WithContext<Element> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self {
            context: input.parse::<Context>()?,
            elements: Punctuated::parse_terminated(input)?,
        })
    }
}
