//! Compile-time constructed PHFs.

use super::{
    generic::{Map as GenericMap, Set as GenericSet},
    hash::GenericHasher,
};

/// A compile-time perfect hash map.
pub type Map<K, V, H = GenericHasher> =
    GenericMap<K, V, &'static [Option<(K, V)>], &'static [u16], H>;

/// A compile-time perfect hash set.
pub type Set<T, H = GenericHasher> = GenericSet<T, &'static [Option<T>], &'static [u16], H>;
