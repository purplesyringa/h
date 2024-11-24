//! Fast perfect hash tables.
//!
//! This crates provides practical and efficient [perfect hash functions][phf] and hash tables built
//! upon them. A PHF is a hash function without collisions on a certain subset of keys. Static hash
//! tables built upon such hashes don't need to resolve collisions, speeding up access time at
//! expense of build time.
//!
//! [phf]: https://en.wikipedia.org/wiki/Perfect_hash_function
//!
//!
//! # Usage
//!
//! Add a dependency to `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! h = "0.1"
//! ```
//!
//! Add `no-default-features = true` for `#![no_std]` support.
//!
//! Create and use a hashmap constructed in compile time:
//!
//! ```rust
//! const TABLE: h::StaticMap<u32, usize> = h::static_map! {
//!     2648081974 => 123,
//!     127361636 => 456,
//!     3593220713 => 789,
//! };
//!
//! assert_eq!(TABLE.get(&127361636), Some(&456));
//! assert_eq!(TABLE.get(&123), None);
//! ```
//!
//!
//! # Imperfect hashes
//!
//! PHFs with a small output range, i.e. suitable for use as indices in hash tables, are built by
//! refining imperfect hash functions -- still collision-free, but only with a full 64-bit output.
//!
//! Note that using [`core::hash::Hash`] for this is incorrect, as it's not portable between
//! platforms. If that hash is used, generating a hash map with a macro and then using it in runtime
//! may fail.
//!
//! Instead, this cache provides [`PortableHash`], which requires portability. You can implement
//! this trait for your types and then use the default [`GenericHasher`] for hash tables.
//! Alternatively, you can provide your own [`ImperfectHasher`] tuned to your data.

#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(
    missing_crate_level_docs,
    missing_docs,
    rustdoc::unescaped_backticks,
    clippy::cargo,
    clippy::pedantic,
    clippy::alloc_instead_of_core,
    clippy::allow_attributes_without_reason,
    // clippy::arithmetic_side_effects,
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

extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

mod codegen;
mod hashed;
mod hashfn;
mod map;
mod scatter;
mod set;
mod unhashed;

pub use hashed::Phf;
pub use hashfn::{GenericHasher, ImperfectHasher, PortableHash};
pub use map::{Map, StaticMap};
pub use set::{Set, StaticSet};

#[doc(hidden)]
pub mod low_level {
    pub use super::unhashed::{Mixer, Phf as UnhashedPhf};
    // Reexport because `alloc` is not available without `extern crate alloc`
    pub use alloc::borrow::Cow;
}

#[cfg(test)]
mod tests;
