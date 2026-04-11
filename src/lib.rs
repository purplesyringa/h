//! Practical, efficient [perfect hash functions][phf].
//!
//! A PHF is a function that maps a pre-determined set of keys to indices without collisions. Static
//! hash tables built upon such hashes can be much simpler than usual, speeding up access time at
//! the expense of build time.
//!
//! This crate does not provide hashmaps or hashsets based on this PHF, and instead focuses on
//! perfecting the low-level mechanism. Hopefully [rust-phf](https://docs.rs/phf) supports this
//! crate as the base one day. In the meantime, hashmaps can be implemented manually.
//!
//! [phf]: https://en.wikipedia.org/wiki/Perfect_hash_function
//!
//!
//! # Types
//!
//! The two core types in this library are [`Phf`] and [`State`].
//!
//! [`Phf`] is the actual type used for hashing, built with [`Phf::build`] and utilized with
//! [`Phf::get`]. If you're building the PHF in runtime (mind you, it's slow), that's all you need.
//!
//! [`State`] is a raw `struct` with public fields describing the internal state of the PHF. It can
//! be obtained with [`Phf::state`] and parsed back with [`Phf::load`]. This design allows [`State`]
//! to be safely populated by hand, e.g. with codegen or with custom deserialization, while
//! validating type invariants before access.
//!
//! [`State`]: state::State
//!
//!
//! # Hashes
//!
//! PHFs are built by refining typical, imperfect hash functions, such as [SipHash] or
//! [`rapidhash`]. The hashes should have high quality: for example, [`rapidhash::quality`] works,
//! but [`rapidhash::fast`] hangs. hyble requires full-avalanche and lack of seed-independent
//! collisions.
//!
//! [SipHash]: https://en.wikipedia.org/wiki/SipHash
//! [`rapidhash`]: https://docs.rs/rapidhash/latest/rapidhash/
//! [`rapidhash::quality`]: https://docs.rs/rapidhash/latest/rapidhash/quality/
//! [`rapidhash::fast`]: https://docs.rs/rapidhash/latest/rapidhash/fast/
//!
//!
//! # Hashing API
//!
//! The PHF is deliberately untyped and decoupled from `std`'s [`Hash`] + [`Hasher`] + [`Borrow`]
//! system, which is often inadequate or over-engineered when PHFs are required. Instead, hyble
//! requires you to provide and invoke hashers manually when necessary:
//!
//! - [`Phf::build`] takes the hasher as a callback, supporting any seeds and not forcing it to
//!   handle arbitrary data types.
//! - [`Phf::get`] requires you to manually hash the input, untying [`Phf`] from [`Borrow`].
//!
//! [`Hasher`]: core::hash::Hasher
//! [`Borrow`]: core::borrow::Borrow
//!
//!
//! # Features
//!
//! - `build`: Enables [`Phf::build`].
//! - `serde`: Enables [serde](https://serde.rs/) integration.
//!
//! hyble is `#![no_std]` by default, and only requires allocation if either feature is enabled.

#![no_std]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "build")]
mod build;

pub mod state;

mod phf;
pub use phf::{LoadError, Phf};

/// We assume that usize is at most 64-bit in many places.
const _: () = assert!(
    size_of::<usize>() <= size_of::<u64>(),
    "I want your computer."
);
