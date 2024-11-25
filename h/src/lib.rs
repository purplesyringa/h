//! Practical, efficient perfect hash tables.
//!
//! This crates provides fast [perfect hash functions][phf] and hash tables built upon them. A PHF
//! is a hash function without collisions on a certain subset of keys. Static hash tables built upon
//! such hashes don't need to resolve collisions, speeding up access time at expense of build time.
//!
//! [phf]: https://en.wikipedia.org/wiki/Perfect_hash_function
//!
//! `h` provides collision-free hash maps, hash sets, and hash functions (whose output you can
//! interpret yourself). It supports four use cases:
//!
//! - When the data is only available in runtime, use [`runtime::Map`]. It's slower to build than
//!   [`HashMap`](std::collections::HashMap), but faster to access.
//! - When the data is fixed, use [`static::map!`] to build the table in compile-time.
//! - When the data is fixed, but programmatically generated, use [`codegen`] to generate Rust code
//!   in `build.rs` and then `include!` it.
//! - When the data is too large or needs to be replaceable without recompiling the code, use
//!   [`rkyv::Map`] to build the table, save it to a file, and then load it in runtime or with
//!   `include_bytes!` using [`rkyv::access`].
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
//! Add `default-features = false` for `#![no_std]` support.
//!
//! Create and use a static hashmap:
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
//! # Advanced examples
//!
//! Programmatically code-gen a map in `build.rs`:
//!
//! ```rust
//! let table: h::Map<u32, usize> = h::Map::from_entries(vec![
//!     (2648081974, 123),
//!     (127361636, 456),
//!     (3593220713, 789),
//! ]);
//!
//! let code = h::codegen::CodeGenerator::new().generate(&table);
//! ```

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

extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

pub mod codegen;
pub mod hash;
mod hashed;
mod map;
mod scatter;
mod set;
mod unhashed;

pub use hashed::Phf;
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
