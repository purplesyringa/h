use super::hashed::Phf;
use crate::hash::{GenericHasher, ImperfectHasher};
use core::borrow::Borrow;
use core::marker::PhantomData;
use core::ops::Deref;

/// A perfect hash map.
///
/// This type supports most read-only methods that [`std::collections::HashMap`] supports.
///
/// By default, [`Map`] can be both built and used in runtime. Use [`StaticMap`] and [`static_map!`]
/// when the data is available in compile time, as this results in better performance.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Map<K, V, C, D, H: ImperfectHasher<K> = GenericHasher> {
    phf: Phf<K, D, H>,
    data: C,
    len: usize,
    marker: PhantomData<(K, V)>,
}

impl<K, V, C, D, H> Map<K, V, C, D, H>
where
    C: Deref<Target = [Option<(K, V)>]>,
    D: Deref<Target = [u16]>,
    H: ImperfectHasher<K>,
{
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(phf: Phf<K, D, H>, data: C, len: usize) -> Self {
        Self {
            phf,
            data,
            len,
            marker: PhantomData,
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
impl<K, V, C: Deref<Target = [Option<(K, V)>]>, H: ImperfectHasher<K>> crate::codegen::Codegen
    for Map<K, V, C, H>
where
    Phf<K, H>: crate::codegen::Codegen,
    K: crate::codegen::Codegen,
    V: crate::codegen::Codegen,
{
    #[inline]
    fn generate_piece(&self, gen: &mut crate::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let static_map = gen.path("h::StaticMap");
        let phf = gen.piece(&self.phf);
        let data = gen.piece(&&*self.data);
        let len = gen.piece(&self.len);
        quote::quote!(#static_map::from_raw_parts(#phf, #data, #len))
    }
}