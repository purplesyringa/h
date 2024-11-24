use super::{
    hash::{GenericHasher, ImperfectHasher},
    scatter::scatter,
    Phf,
};
use alloc::vec::Vec;
use core::borrow::Borrow;
use core::marker::PhantomData;
use core::ops::Deref;

/// A constant-time perfect hash map.
///
/// This type supports most read-only methods that [`std::collections::HashMap`] supports.
///
/// Construct with [`static_map!`].
pub type StaticMap<K, V, H = GenericHasher> = Map<K, V, H, &'static [Option<(K, V)>]>;

/// A perfect hash map.
///
/// This type supports most read-only methods that [`std::collections::HashMap`] supports.
///
/// By default, [`Map`] can be both built and used in runtime. Use [`StaticMap`] and [`static_map!`]
/// when the data is available in compile time, as this results in better performance.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Map<K, V, H: ImperfectHasher<K> = GenericHasher, C = Vec<Option<(K, V)>>> {
    phf: Phf<K, H>,
    data: C,
    len: usize,
    marker: PhantomData<(K, V)>,
}

impl<K, V, H, C> Map<K, V, H, C>
where
    H: ImperfectHasher<K>,
    C: Deref<Target = [Option<(K, V)>]>,
{
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(phf: Phf<K, H>, data: C, len: usize) -> Self {
        Self {
            phf,
            data,
            len,
            marker: PhantomData,
        }
    }

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
    pub fn try_from_entries(entries: Vec<(K, V)>) -> Option<Self>
    where
        Vec<Option<(K, V)>>: Into<C>,
    {
        let len = entries.len();
        let phf = Phf::try_from_keys(entries.iter().map(|(key, _)| key))?;
        let mut data: Vec<Option<(K, V)>> = (0..phf.capacity()).map(|_| None).collect();
        scatter(entries, |(key, _)| phf.hash(key), &mut data);
        Some(Self {
            phf,
            data: data.into(),
            len,
            marker: PhantomData,
        })
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
    pub fn from_entries(entries: Vec<(K, V)>) -> Self
    where
        Vec<Option<(K, V)>>: Into<C>,
    {
        Self::try_from_entries(entries).expect("ran out of imperfect hash family instances")
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
impl<K, V, H: ImperfectHasher<K>, C: Deref<Target = [Option<(K, V)>]>> super::codegen::Codegen
    for Map<K, V, H, C>
where
    Phf<K, H>: super::codegen::Codegen,
    K: super::codegen::Codegen,
    V: super::codegen::Codegen,
{
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let static_map = gen.path("h::StaticMap");
        let phf = gen.piece(&self.phf);
        let data = gen.piece(&&*self.data);
        let len = gen.piece(&self.len);
        quote::quote!(#static_map::from_raw_parts(#phf, #data, #len))
    }
}
