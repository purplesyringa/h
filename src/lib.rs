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
//! Create and use a hashmap:
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
