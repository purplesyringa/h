//! Practical, efficient perfect hash tables.
//!
//! This crates provides fast [perfect hash functions][phf] and hash tables built upon them. A PHF
//! is a hash function without collisions on a certain subset of keys. Static hash tables built upon
//! such hashes don't need to resolve collisions, speeding up access time at the expense of build
//! time.
//!
//! [phf]: https://en.wikipedia.org/wiki/Perfect_hash_function
//!
//!
//! # Usage
//!
//! `h` implements a hash [`Map`], a hash [`Set`], and a low-level [`Phf`].
//!
//! These types can be initialized in several ways:
//!
//! 1. They can be built in runtime by calling `from_*` methods. This is slow, so consider
//!    [`HashMap`](std::collections::HashMap) instead.
//! 2. They can be built in compile time with macros, such as [`map!`]. Zero-cost in runtime.
//! 3. They can be built in `build.rs` with `from_*`, translated to code with [`codegen`] and then
//!    `include!`d. Zero-cost in runtime and supports programmatic generation.
//! 4. They can be built, saved to a file, and then loaded back with [`rkyv`]. Zero-cost when
//!    loading. Also consider using this instead of codegen for large data.
//!
//! The types can either own the data (when constructed manually) or borrow it (when constructed
//! with macros or deserialized with [`rkyv`]). Lifetime parameters specify the duration of that
//! borrow; use `'static` if the data is owned.

#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(
    missing_docs,
    rustdoc::missing_crate_level_docs,
    rustdoc::unescaped_backticks,
    clippy::cargo,
    clippy::pedantic,
    clippy::alloc_instead_of_core,
    clippy::allow_attributes_without_reason,
    clippy::arithmetic_side_effects,
    clippy::as_underscore,
    clippy::assertions_on_result_states,
    clippy::clone_on_ref_ptr,
    clippy::decimal_literal_representation,
    clippy::default_numeric_fallback,
    clippy::deref_by_slicing,
    clippy::doc_markdown,
    clippy::else_if_without_else,
    clippy::empty_drop,
    clippy::empty_enum_variants_with_brackets,
    clippy::empty_structs_with_brackets,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::fn_to_numeric_cast_any,
    clippy::format_push_string,
    clippy::infinite_loop,
    clippy::inline_asm_x86_att_syntax,
    clippy::mem_forget, // use ManuallyDrop instead
    clippy::missing_assert_message,
    clippy::missing_const_for_fn,
    // clippy::missing_docs_in_private_items,
    clippy::missing_inline_in_public_items,
    clippy::missing_safety_doc,
    clippy::mixed_read_write_in_expression,
    clippy::multiple_unsafe_ops_per_block,
    clippy::mutex_atomic,
    clippy::needless_raw_strings,
    clippy::pub_without_shorthand,
    clippy::rc_buffer,
    clippy::rc_mutex,
    clippy::redundant_type_annotations,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::self_named_module_files,
    clippy::semicolon_inside_block,
    clippy::separated_literal_suffix,
    clippy::shadow_unrelated,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    clippy::string_lit_chars_any,
    clippy::string_to_string,
    clippy::tests_outside_test_module,
    clippy::try_err,
    // clippy::undocumented_unsafe_blocks,
    clippy::unnecessary_safety_comment,
    clippy::unnecessary_safety_doc,
    clippy::unnecessary_self_imports,
    clippy::unneeded_field_pattern,
    clippy::unused_result_ok,
    clippy::wildcard_enum_match_arm,
)]
#![allow(
    clippy::inline_always,
    reason = "I'm not an idiot, this is a result of benchmarking/profiling"
)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

pub mod codegen;
pub mod hash;
mod map;
mod phf;
pub(crate) mod scatter;
mod set;
pub(crate) mod unhashed;

pub use map::Map;
pub use phf::Phf;
pub use set::Set;

#[doc(hidden)]
pub mod low_level {
    pub use super::unhashed::{Mixer, Phf as UnhashedPhf};
}

#[derive(Clone, Debug)]
#[doc(hidden)]
#[non_exhaustive]
pub enum BorrowedOrOwnedSlice<'a, T> {
    Borrowed(&'a [T]),
    #[cfg(feature = "alloc")]
    Owned(alloc::vec::Vec<T>),
}

impl<T> core::ops::Deref for BorrowedOrOwnedSlice<'_, T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &[T] {
        match self {
            Self::Borrowed(r) => r,
            #[cfg(feature = "alloc")]
            Self::Owned(o) => o,
        }
    }
}

#[cfg(test)]
mod tests;
