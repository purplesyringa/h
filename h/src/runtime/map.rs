use super::scatter::scatter;
use crate::{
    generic,
    hash::{GenericHasher, ImperfectHasher},
};
use alloc::vec::Vec;

/// A runtime perfect hash map.
pub type Map<K, V, H = GenericHasher> = generic::Map<K, V, Vec<Option<(K, V)>>, Vec<u16>, H>;

impl<K, V, H: ImperfectHasher<K>> Map<K, V, H> {
    /// Try to generate a perfect hash map.
    ///
    /// There must not be duplicate keys in the input.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    ///
    /// To instantiate [`StaticMap`], use [`static_map!`].
    #[inline] // heavy, but monomorphized anyway
    #[must_use]
    pub fn try_from_entries(entries: Vec<(K, V)>) -> Option<Self> {
        let len = entries.len();
        let phf = generic::Phf::try_from_keys(entries.iter().map(|(key, _)| key))?;
        let mut data: Vec<Option<(K, V)>> = (0..phf.capacity()).map(|_| None).collect();
        scatter(entries, |(key, _)| phf.hash(key), &mut data);
        Some(Self::from_raw_parts(phf, data, len))
    }

    /// Generate a perfect hash map.
    ///
    /// There must not be duplicate keys in the input.
    ///
    /// To instantiate [`StaticMap`], use [`static_map!`].
    ///
    /// # Panics
    ///
    /// Panics if the underlying imperfect hash function family is finite and generation didn't
    /// succeed.
    #[inline]
    #[must_use]
    pub fn from_entries(entries: Vec<(K, V)>) -> Self {
        Self::try_from_entries(entries).expect("ran out of imperfect hash family instances")
    }
}
