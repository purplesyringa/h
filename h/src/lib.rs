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

extern crate self as h;

#[doc(inline)]
pub use h_bare::*;

#[cfg(feature = "macros")]
pub use h_macros::map;
// #[cfg(feature = "macros")]
// pub use h_macros::phf;
// #[cfg(feature = "macros")]
// pub use h_macros::set;

#[must_use]
#[doc(hidden)]
#[inline]
pub fn conjure<T>() -> T {
    unreachable!();
}

#[cfg(feature = "macros")]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn macros() {
        let phf: Map<u64, usize> = map! {
            for u64;
            123 => 0,
            456 => 1,
        };
        assert_eq!(phf.get(&123), Some(&0));
        assert_eq!(phf.get(&456), Some(&1));
        assert_eq!(phf.get(&789), None);
    }
}
