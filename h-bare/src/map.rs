//! Perfect hash maps.

use super::{
    const_vec::ConstVec,
    hash::{GenericHasher, ImperfectHasher},
    Phf,
};
use core::borrow::Borrow;

/// A perfect hash map.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
#[cfg_attr(
    all(feature = "alloc", feature = "serde"),
    serde(
        bound(
            deserialize = "K: serde::Deserialize<'de>, V: serde::Deserialize<'de>, H: serde::Deserialize<'de> + ImperfectHasher<K>"
        ),
        try_from = "MapInner<K, V, H>"
    )
)]
#[cfg_attr(
    all(feature = "alloc", feature = "serde"),
    expect(
        clippy::unsafe_derive_deserialize,
        reason = "safety requirements are validated using TryFrom"
    )
)]
pub struct Map<K, V, H = GenericHasher> {
    /// The actual map.
    inner: MapInner<K, V, H>,
}

/// The actual map.
///
/// This needs to be a separate type so that `serde` can convert from this type to [`Map`] with
/// [`TryFrom`] during deserialization, so that we can validate the map.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
struct MapInner<K, V, H> {
    /// A PHF mapping keys to indices in [`data`](Self::data).
    phf: Phf<K, H>,

    /// The entries of the map, indexed by their perfect hashes. `None` means an entry whose key has
    /// such a hash is absent.
    data: ConstVec<Option<(K, V)>>,

    /// The number of elements in the map.
    ///
    /// This is equal to the number of `Some` values in [`data`](Self::data) and is used purely for
    /// optimization of [`len`](Map::len).
    len: usize,
}

#[cfg(feature = "build")]
impl<K, V, H: ImperfectHasher<K>> Map<K, V, H> {
    /// Try to generate a perfect hash map.
    ///
    /// There must not be duplicate keys in the input.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    #[inline] // heavy, but monomorphized anyway
    #[must_use]
    pub fn try_from_entries(entries: alloc::vec::Vec<(K, V)>) -> Option<Self> {
        let len = entries.len();
        let phf = Phf::try_from_keys(entries.iter().map(|(key, _)| key))?;
        let mut data: alloc::vec::Vec<_> = (0..phf.capacity()).map(|_| None).collect();
        super::scatter::scatter(entries, |(key, _)| phf.hash(key), &mut data);
        Some(Self {
            inner: MapInner {
                phf,
                data: data.into(),
                len,
            },
        })
    }

    /// Generate a perfect hash map.
    ///
    /// There must not be duplicate keys in the input.
    ///
    /// # Panics
    ///
    /// Panics if the underlying imperfect hash function family is finite and generation didn't
    /// succeed.
    #[inline]
    #[must_use]
    pub fn from_entries(entries: alloc::vec::Vec<(K, V)>) -> Self {
        Self::try_from_entries(entries).expect("ran out of imperfect hash family instances")
    }
}

impl<K, V, H> Map<K, V, H> {
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn __from_raw_parts(
        phf: Phf<K, H>,
        data: ConstVec<Option<(K, V)>>,
        len: usize,
    ) -> Self {
        Self {
            inner: MapInner { phf, data, len },
        }
    }

    /// Get a key-value pair by key.
    #[inline]
    pub fn get_key_value<Q>(&self, key: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q>,
    {
        unsafe { self.inner.data.get_unchecked(self.inner.phf.hash(key)) }
            .as_ref()
            .filter(|(k, _)| k.borrow() == key)
            .map(|(k, v)| (k, v))
    }

    /// Get a key-value pair by key, with a mutable reference to the value.
    #[inline]
    pub fn get_key_value_mut<Q>(&mut self, key: &Q) -> Option<(&K, &mut V)>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q>,
    {
        unsafe { self.inner.data.get_unchecked_mut(self.inner.phf.hash(key)) }
            .as_mut()
            .filter(|(k, _)| k.borrow() == key)
            .map(|(k, v)| (k as &K, v))
    }

    /// Get a value by key.
    #[inline]
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q>,
    {
        self.get_key_value(key).map(|(_, v)| v)
    }

    /// Get a mutable reference to the value by key.
    #[inline]
    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q>,
    {
        self.get_key_value_mut(key).map(|(_, v)| v)
    }

    /// Check if the hashmap contains a key.
    #[inline]
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q>,
    {
        self.get_key_value(key).is_some()
    }

    /// Get number of entries.
    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len
    }

    /// Check if the hashmap is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Iterate through elements.
    ///
    /// The iteration order is unspecified, but is constant for a given map.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.inner
            .data
            .iter()
            .filter_map(|pair| pair.as_ref().map(|(k, v)| (k, v)))
    }

    /// Iterate through elements mutably.
    ///
    /// The iteration order is unspecified, but is constant for a given map.
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        self.inner
            .data
            .iter_mut()
            .filter_map(|pair| pair.as_mut().map(|(k, v)| (k as &K, v)))
    }

    /// Iterate through keys.
    ///
    /// The iteration order is unspecified, but is constant for a given map.
    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.iter().map(|(k, _)| k)
    }

    /// Iterate through values.
    ///
    /// The iteration order is unspecified, but is constant for a given map.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }

    /// Iterate through values mutably.
    ///
    /// The iteration order is unspecified, but is constant for a given map.
    #[inline]
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.iter_mut().map(|(_, v)| v)
    }
}

/// Scope for `serde`-related code.
#[cfg(all(feature = "alloc", feature = "serde"))]
mod serde_support {
    use super::{ImperfectHasher, Map, MapInner};
    use displaydoc::Display;
    use thiserror::Error;

    /// Deserialization validation failures.
    #[derive(Debug, Display, Error)]
    pub enum Error {
        /// wrong data length
        WrongDataLength,

        /// wrong len
        WrongLen,

        /// misplaced entry
        MisplacedEntry,
    }

    impl<K, V, H: ImperfectHasher<K>> TryFrom<MapInner<K, V, H>> for Map<K, V, H> {
        type Error = Error;

        #[inline]
        fn try_from(inner: MapInner<K, V, H>) -> Result<Self, Error> {
            if inner.data.len() != inner.phf.capacity() {
                return Err(Error::WrongDataLength);
            }

            if inner.len != inner.data.iter().filter(|opt| opt.is_some()).count() {
                return Err(Error::WrongLen);
            }

            for (index, entry) in inner.data.iter().enumerate() {
                if let Some((key, _)) = entry {
                    if inner.phf.hash(key) != index {
                        return Err(Error::MisplacedEntry);
                    }
                }
            }

            Ok(Self { inner })
        }
    }
}

#[cfg(feature = "codegen")]
impl<K: super::codegen::Codegen, V: super::codegen::Codegen, H: super::codegen::Codegen>
    super::codegen::Codegen for Map<K, V, H>
{
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let map = gen.path("h::Map");
        let phf = gen.piece(&self.inner.phf);
        let data = gen.piece(&self.inner.data);
        let len = gen.piece(&self.inner.len);
        quote::quote!(#map::__from_raw_parts(#phf, #data, #len))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use rapidhash::RapidRng;

    fn generate_entries(rng: &mut RapidRng) -> Vec<(u64, usize)> {
        let n_elements: usize = (rng.next() % 10).try_into().unwrap();
        (0..n_elements).map(|i| (rng.next(), i)).collect()
    }

    #[test]
    fn map() {
        let mut rng = RapidRng::new(0x243f_6a88_85a3_08d3);
        for _ in 0..50 {
            let mut entries = generate_entries(&mut rng);
            let mut h_map: Map<u64, usize> = Map::from_entries(entries.clone());

            for (key, value) in &entries {
                assert_eq!(h_map.get_key_value(key), Some((key, value)));
                assert_eq!(h_map.get_key_value_mut(key), Some((key, &mut { *value })));
                assert_eq!(h_map.get(key), Some(value));
                assert_eq!(h_map.contains_key(key), true);

                let value_r = h_map.get_mut(key).unwrap();
                assert_eq!(*value_r, *value);
                *value_r += 1000;
                assert_eq!(h_map.get(key).unwrap(), &(value + 1000));
            }
            assert_eq!(h_map.len(), entries.len());
            assert_eq!(h_map.is_empty(), entries.is_empty());

            assert_eq!(h_map.get_key_value(&0), None);
            assert_eq!(h_map.get_key_value_mut(&0), None);
            assert_eq!(h_map.get(&0), None);
            assert_eq!(h_map.get_mut(&0), None);
            assert_eq!(h_map.contains_key(&0), false);

            for value in h_map.values_mut() {
                *value -= 1000;
            }

            let mut entries2: Vec<(u64, usize)> =
                h_map.iter().map(|(key, value)| (*key, *value)).collect();
            entries.sort_unstable();
            entries2.sort_unstable();
            assert_eq!(entries, entries2);

            let mut keys: Vec<u64> = entries.iter().map(|(key, _)| *key).collect();
            let mut keys2: Vec<u64> = h_map.keys().copied().collect();
            keys.sort_unstable();
            keys2.sort_unstable();
            assert_eq!(keys, keys2);

            let mut values: Vec<usize> = entries.iter().map(|(_, value)| *value).collect();
            let mut values2: Vec<usize> = h_map.values().copied().collect();
            values.sort_unstable();
            values2.sort_unstable();
            assert_eq!(values, values2);
        }
    }
}
