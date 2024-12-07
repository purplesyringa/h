use super::{
    const_vec::ConstVec,
    hash::{GenericHasher, ImperfectHasher},
    Phf,
};
use core::borrow::Borrow;

/// A perfect hash map.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(try_from = "MapInner<K, V, H>"))]
#[allow(
    clippy::unsafe_derive_deserialize,
    reason = "safety requirements are validated using TryFrom"
)]
pub struct Map<K, V, H = GenericHasher> {
    inner: MapInner<K, V, H>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct MapInner<K, V, H> {
    phf: Phf<K, H>,
    data: ConstVec<Option<(K, V)>>,
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
        Some(Self::from_raw_parts(phf, data.into(), len))
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
    pub const fn from_raw_parts(
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

#[cfg(feature = "serde")]
mod serde_support {
    use super::{Map, MapInner};
    use thiserror::Error;

    #[derive(Debug, Error)]
    pub enum Error {
        #[error("Wrong data length")]
        WrongDataLength,

        #[error("Wrong len")]
        WrongLen,
    }

    impl<K, V, H> TryFrom<MapInner<K, V, H>> for Map<K, V, H> {
        type Error = Error;

        #[inline]
        fn try_from(inner: MapInner<K, V, H>) -> Result<Self, Error> {
            if inner.data.len() != inner.phf.capacity() {
                return Err(Error::WrongDataLength);
            }

            if inner.len != inner.data.iter().filter(|opt| opt.is_some()).count() {
                return Err(Error::WrongLen);
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
        quote::quote!(#map::from_raw_parts(#phf, #data, #len))
    }
}
