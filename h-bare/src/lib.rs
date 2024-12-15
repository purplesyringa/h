//! This crate provides types for [`h`](https://docs.rs/h). Use that crate instead.

#![no_std]
#![cfg_attr(doc, feature(doc_auto_cfg))]
#![deny(unsafe_op_in_unsafe_fn)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

pub(crate) mod bitmap;
pub mod codegen;
mod const_vec;
pub mod hash;
mod map;
mod phf;
pub(crate) mod scatter;
mod set;
pub(crate) mod unhashed_phf;

pub use map::Map;
pub use phf::Phf;
pub use set::Set;

/// Public (but hidden) reexports for codegen to access.
#[doc(hidden)]
pub mod low_level {
    pub use super::const_vec::ConstVec;
    pub use super::unhashed_phf::{Mixer, UnhashedPhf};
}

#[cfg(test)]
mod tests;
