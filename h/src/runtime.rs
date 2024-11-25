//! Runtime-constructed PHFs.

#![cfg(feature = "build")]

use super::{
    generic::{Map as GenericMap, Set as GenericSet},
    hash::GenericHasher,
};
use alloc::vec::Vec;

/// A runtime perfect hash map.
pub type Map<K, V, H = GenericHasher> = GenericMap<K, V, Vec<Option<(K, V)>>, Vec<u16>, H>;

/// A runtime perfect hash set.
pub type Set<T, H = GenericHasher> = GenericSet<T, Vec<Option<T>>, Vec<u16>, H>;
