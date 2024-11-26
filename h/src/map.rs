use super::{
    hash::{GenericHasher, ImperfectHasher},
    BorrowedOrOwnedSlice, Phf,
};
use core::borrow::Borrow;

/// A perfect hash map.
#[non_exhaustive]
pub struct Map<'a, K, V, H: ImperfectHasher<K> = GenericHasher> {
    phf: Phf<'a, K, H>,
    data: BorrowedOrOwnedSlice<'a, Option<(K, V)>>,
    len: usize,
}

impl<'a, K, V, H: ImperfectHasher<K>> Map<'a, K, V, H> {
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(
        phf: Phf<'a, K, H>,
        data: BorrowedOrOwnedSlice<'a, Option<(K, V)>>,
        len: usize,
    ) -> Self {
        Self { phf, data, len }
    }

    /// Try to generate a perfect hash map.
    ///
    /// There must not be duplicate keys in the input.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    #[cfg(feature = "build")]
    #[inline] // heavy, but monomorphized anyway
    #[must_use]
    pub fn try_from_entries(entries: alloc::vec::Vec<(K, V)>) -> Option<Self> {
        let len = entries.len();
        let phf = Phf::try_from_keys(entries.iter().map(|(key, _)| key))?;
        let mut data: alloc::vec::Vec<_> = (0..phf.capacity()).map(|_| None).collect();
        super::scatter::scatter(entries, |(key, _)| phf.hash(key), &mut data);
        Some(Self::from_raw_parts(
            phf,
            BorrowedOrOwnedSlice::Owned(data),
            len,
        ))
    }

    /// Generate a perfect hash map.
    ///
    /// There must not be duplicate keys in the input.
    ///
    /// # Panics
    ///
    /// Panics if the underlying imperfect hash function family is finite and generation didn't
    /// succeed.
    #[cfg(feature = "build")]
    #[inline]
    #[must_use]
    pub fn from_entries(entries: alloc::vec::Vec<(K, V)>) -> Self {
        Self::try_from_entries(entries).expect("ran out of imperfect hash family instances")
    }

    /// Produce a copy of [`Map`] that references this one instead of owning data.
    ///
    /// This is useful for code generation, so that references to slices are generated to statics
    /// instead of dynamically allocated vectors, so that [`Map`] can be initialized in a `const`
    /// context.
    #[cfg(feature = "build")]
    #[inline]
    pub fn borrow(&self) -> Map<'_, K, V, H> {
        Map {
            phf: self.phf.borrow(),
            data: BorrowedOrOwnedSlice::Borrowed(&*self.data),
            len: self.len,
        }
    }

    /// Get a key-value pair by key.
    #[inline]
    pub fn get_key_value<Q>(&self, key: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q, Instance = <H as ImperfectHasher<K>>::Instance>,
    {
        unsafe { self.data.get_unchecked(self.phf.hash(key)) }
            .as_ref()
            .filter(|(k, _)| k.borrow() == key)
            .map(|(k, v)| (k, v))
    }

    /// Get a value by key.
    #[inline]
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q, Instance = <H as ImperfectHasher<K>>::Instance>,
    {
        self.get_key_value(key).map(|(_, v)| v)
    }

    /// Check if the hashmap contains a key.
    #[inline]
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q, Instance = <H as ImperfectHasher<K>>::Instance>,
    {
        self.get_key_value(key).is_some()
    }

    /// Get number of entries.
    #[inline]
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Check if the hashmap is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Iterate through elements.
    ///
    /// The iteration order is unspecified, but is constant for a given map.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.data
            .iter()
            .filter_map(|pair| pair.as_ref().map(|(k, v)| (k, v)))
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
}

#[cfg(feature = "codegen")]
impl<'a, K, V, H: ImperfectHasher<K>> crate::codegen::Codegen for Map<'a, K, V, H>
where
    Phf<'a, K, H>: crate::codegen::Codegen,
    K: crate::codegen::Codegen,
    V: crate::codegen::Codegen,
{
    #[inline]
    fn generate_piece(&self, gen: &mut crate::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let map = gen.path("h::Map");
        let phf = gen.piece(&self.phf);
        let data = gen.piece(&self.data);
        let len = gen.piece(&self.len);
        quote::quote!(#map::from_raw_parts(#phf, #data, #len))
    }
}
