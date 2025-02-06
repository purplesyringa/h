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
//! `h` implements a [`Map`], a [`Set`], and a low-level [`Phf`]. These types are constructed once
//! and cannot be modified afterwards, with an exception that the values of existing keys in [`Map`]
//! can be updated.
//!
//! These types can be initialized in several ways:
//!
//! 1. Built in runtime by calling `from_*` methods. (This is slow, so profile against
//!    [`HashMap`](std::collections::HashMap).)
//! 2. Built in compile time with macros, such as [`map!`]. Zero-cost in runtime.
//! 3. Built in `build.rs` with `from_*`, translated to code with [`codegen`] and then
//!    [`include!`]d. Zero-cost in runtime and supports programmatic generation, even for
//!    non-serializable types.
//! 4. Built, saved to a file, and then loaded back with [serde](https://serde.rs/). This requires
//!    deserialization, but is likely faster than rebuilding from scratch. Also consider using this
//!    instead of codegen for large data.
//!
//!
//! # Examples
//!
//! As this crate has an unusually short name `h`, we use the full paths, e.g. `h::Map`, instead of
//! importing the items with `use`.
//!
//! Make a static map with macros:
//!
//! ```rust
//! #[derive(Debug, PartialEq)]
//! enum Keyword {
//!     Loop,
//!     Continue,
//!     Break,
//!     Fn,
//!     Extern,
//! }
//!
//! const KEYWORDS: &h::Map<&str, Keyword> = h::map! {
//!     "loop" => Keyword::Loop,
//!     "continue" => Keyword::Continue,
//!     "break" => Keyword::Break,
//!     "fn" => Keyword::Fn,
//!     "extern" => Keyword::Extern,
//! };
//!
//! assert_eq!(KEYWORDS.get("break"), Some(&Keyword::Break));
//! assert_eq!(KEYWORDS.get("ident"), None);
//! ```
//!
//! Create a map in runtime:
//!
//! ```rust
//! struct Context;
//!
//! // parser definitions
//! fn parse_loop(_ctx: &mut Context) {}
//! fn parse_continue(_ctx: &mut Context) {}
//! fn parse_break(_ctx: &mut Context) {}
//! fn parse_fn(_ctx: &mut Context) {}
//! fn parse_extern(_ctx: &mut Context) {}
//!
//! let keywords: h::Map<&str, fn(&mut Context)> = h::Map::from_entries(vec![
//!     ("loop", parse_loop),
//!     ("continue", parse_continue),
//!     ("break", parse_break),
//!     ("fn", parse_fn),
//!     ("extern", parse_extern),
//! ]);
//!
//! let handler = keywords.get("break").unwrap();
//! handler(&mut Context);
//! ```
//!
//! Programmatically code-gen a set in `build.rs`:
//!
//! ```rust
//! let keywords = vec!["loop", "continue", "break", "fn", "extern"];
//! let keywords: h::Set<&str> = h::Set::from_elements(keywords);
//! let code = h::codegen::CodeGenerator::new().generate(&keywords);
//! // save `code` to a file and `include!` it inside the crate
//! ```
//!
//!
//! # Features
//!
//! - `std` (default): Whether [`std`] is available. Isn't currently used for much except tests.
//! - `alloc` (default): Whether [`alloc`] is available. Enables implementations of certain traits
//!   for types from [`alloc`], such as [`String`](alloc::string::String) and
//!   [`Vec`](alloc::vec::Vec).
//! - `build`: Enables `from_*` methods. Necessary for runtime building or programmatic generation
//!   in `build.rs`.
//! - `codegen`: Enables [`codegen`]. Necessary for programmatic generation in `build.rs`.
//! - `macros`: Enables macros like [`map!`].
//! - `serde`: Enables [serde](https://serde.rs/) support.

#![no_std]
#![cfg_attr(doc, feature(doc_auto_cfg))]

// For docs
#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

extern crate self as h;

// Inlining documentation removes docs from private/hidden functions. Force no-inline when compiling
// dev docs.
#[cfg_attr(devdoc, doc(no_inline))]
pub use h_bare::*;

#[cfg(feature = "macros")]
pub mod macros;
