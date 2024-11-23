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
//! # Imperfect hashes
//!
//! PHFs with a small output range, i.e. suitable for use as indices in hash tables, are built by
//! refining imperfect hash functions -- still collision-free, but only with a full 64-bit output.
//!
//! This crate provides [`GenericHasher`], which implements such a family of hash functions for
//! common types. If you need to hash a type that [`GenericHasher`] does not understand, you'll have
//! to provide your own implementation of [`ImperfectHasher`].
//!
//! Note that using [`core::hash::Hash`] for this is incorrect, as it's not portable between
//! platforms. If this hash is used, generating a hash map with a macro and then using it in runtime
//! may fail.

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
pub use hashfn::{GenericHasher, ImperfectHasher};
pub use map::{Map, StaticMap};
pub use set::{Set, StaticSet};

#[doc(hidden)]
pub mod low_level {
    pub use super::unhashed::{Mixer, Phf as UnhashedPhf};
    // Reexport because `alloc` is not available without `extern crate alloc`
    pub use alloc::borrow::Cow;
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test() {
//         use codegen::Codegen;
//         use rand::{distributions::Standard, rngs::SmallRng, Rng, SeedableRng};
//         use std::hint::black_box;
//         use std::time::Instant;

//         let mut entries: Vec<(u64, ())> = SmallRng::seed_from_u64(0x439f26744da767e5)
//             .sample_iter(Standard)
//             .take(1000000)
//             .map(|k| (k, ()))
//             .collect();
//         entries.sort_unstable();
//         entries.dedup();

//         let phf: Map<u64, ()> = Map::try_new(entries.clone()).unwrap();
//         println!("{}", Codegen(&phf));

//         let start = Instant::now();
//         for _ in 0..20 {
//             let phf: Map<u64, ()> = Map::try_new(entries.clone()).unwrap();
//             black_box(&phf);
//         }
//         println!("Build time: {:?}", start.elapsed() / 20);

//         let start = Instant::now();
//         for _ in 0..1000000000 {
//             black_box(phf.get(&black_box(0x26744da767e5439f)).copied());
//         }
//         println!(
//             "Access time: {:?}ns",
//             start.elapsed().as_nanos() as f32 / 1e9
//         );
//     }
// }
