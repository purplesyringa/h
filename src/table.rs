use super::hashed::{HashFn, Phf};
use core::borrow::Borrow;

/// A perfect hash map.
#[derive(Clone, Debug)]
pub struct PhfMap<K, V, H: HashFn<K>> {
    pub(crate) phf: Phf<K, H>,
    pub(crate) data: Vec<Option<(K, V)>>,
    pub(crate) len: usize,
}

impl<K, V, H: HashFn<K>> PhfMap<K, V, H> {
    /// Try to generate a perfect hash map.
    ///
    /// There must not be duplicate keys in the input.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    pub fn try_new(entries: Vec<(K, V)>) -> Option<Self> {
        let len = entries.len();
        let phf = Phf::try_new(entries.iter().map(|(key, _)| key))?;
        let mut data: Vec<Option<(K, V)>> = (0..phf.capacity()).map(|_| None).collect();
        for (key, value) in entries {
            let hash = phf.hash(&key);
            data[hash] = Some((key, value));
        }
        Some(Self { phf, data, len })
    }

    /// Get a key-value pair by key.
    pub fn get_key_value<Q>(&self, key: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: HashFn<Q>,
        <H as HashFn<K>>::Fallback: HashFn<Q>,
    {
        unsafe { self.data.get_unchecked(self.phf.hash(key)) }
            .as_ref()
            .filter(|(k, _)| k.borrow() == key)
            .map(|(k, v)| (k, v))
    }

    /// Get a value by key.
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: HashFn<Q>,
        <H as HashFn<K>>::Fallback: HashFn<Q>,
    {
        self.get_key_value(key).map(|(_, v)| v)
    }

    /// Check if the hashmap contains a key.
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: ?Sized + Eq,
        H: HashFn<Q>,
        <H as HashFn<K>>::Fallback: HashFn<Q>,
    {
        self.get_key_value(key).is_some()
    }

    /// Get number of entries.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Check if the hashmap is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Iterate through elements.
    ///
    /// The iteration order is unspecified, but is constant for a given map.
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.data
            .iter()
            .filter_map(|pair| pair.as_ref().map(|(k, v)| (k, v)))
    }

    /// Iterate through keys.
    ///
    /// The iteration order is unspecified, but is constant for a given map.
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.iter().map(|(k, _)| k)
    }

    /// Iterate through values.
    ///
    /// The iteration order is unspecified, but is constant for a given map.
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }
}
