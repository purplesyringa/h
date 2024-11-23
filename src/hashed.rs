use super::{codegen::Codegen, unhashed::Phf as UnhashedPhf, GenericHasher, ImperfectHasher};
use core::fmt;

/// A perfect hash function.
///
/// A mapping from `T` to numbers from `0` to `N - 1`, injective over the training key set. `N`
/// might be larger than the size of the training key set.
///
/// By default, [`Phf`] can be both built and used in runtime. Use [`phf!`] when the data is
/// available in compile time, as this results in better performance.
#[derive(Clone, Debug)]
pub struct Phf<T, H: ImperfectHasher<T> = GenericHasher> {
    #[doc(hidden)]
    pub hash_instance: H::Instance,
    #[doc(hidden)]
    pub unhashed_phf: UnhashedPhf,
}

impl<T, H: ImperfectHasher<T>> Phf<T, H> {
    /// Try to generate a perfect hash function.
    ///
    /// `keys` must not contain duplicates. It's an exact-size cloneable iterator rather than
    /// a slice reference so that multiple underlying containers can be used.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    pub fn try_new<'a>(
        keys: impl Iterator<Item = &'a T> + ExactSizeIterator + Clone,
    ) -> Option<Self>
    where
        T: 'a,
    {
        let percent = keys.len().div_ceil(100);

        // Don't let hash_space grow beyond this
        let max_hash_space = (keys.len() + 5 * percent).next_power_of_two();

        // Start with different load factors for different sizes. This was tuned experimentally.
        let mut hash_space = keys.len() + percent * keys.len().div_ceil(1000000).min(5);

        // Increase hash_space exponentially by 1.01 on each iteration until reaching a power of two
        // size. For good hashes, this loop should terminate soon.
        for hash_instance in H::iter() {
            if let Some(unhashed_phf) = UnhashedPhf::try_new(
                keys.clone()
                    .map(|key| H::hash(&hash_instance, key))
                    .collect(),
                hash_space,
            ) {
                return Some(Self {
                    hash_instance,
                    unhashed_phf,
                });
            }
            // Both increase the hash space and change the hash function. This is especially
            // important for infinite families, which wouldn't progress otherwise.
            hash_space = (hash_space + hash_space.div_ceil(100)).min(max_hash_space);
        }

        None
    }

    /// Hash a key.
    ///
    /// The whole point. Guaranteed to return different indices for different keys from the training
    /// dataset. `key` is expected to already be hashed.
    ///
    /// May return arbitrary indices for keys outside the dataset.
    pub fn hash<U: ?Sized>(&self, key: &U) -> usize
    where
        H: ImperfectHasher<U, Instance = <H as ImperfectHasher<T>>::Instance>,
    {
        self.unhashed_phf.hash(H::hash(&self.hash_instance, key))
    }

    /// Get the boundary on indices.
    ///
    /// This is `N` such that all keys are within range `[0; N)`.
    ///
    /// The index returned by `hash` is guaranteed to *always* be less than `capacity()`, even for
    /// keys outside the training dataset.
    pub const fn capacity(&self) -> usize {
        self.unhashed_phf.capacity()
    }
}

impl<'a, T, H: ImperfectHasher<T>> fmt::Display for Codegen<'a, Phf<T, H>>
where
    Codegen<'a, H::Instance>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "::h::Phf {{ hash_instance: {}, unhashed_phf: {} }}",
            Codegen(&self.0.hash_instance),
            Codegen(&self.0.unhashed_phf),
        )
    }
}
