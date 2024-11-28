use super::{
    hash::{GenericHasher, ImperfectHasher},
    unhashed::Phf as UnhashedPhf,
};

/// A perfect hash function.
///
/// A mapping from `T` to numbers from `0` to `N - 1`, injective over the training key set. `N`
/// might be larger than the size of the training key set.
#[non_exhaustive]
pub struct Phf<T, H: ImperfectHasher<T> = GenericHasher> {
    hash_instance: H::Instance,
    unhashed_phf: UnhashedPhf,
}

impl<T, H: ImperfectHasher<T>> Phf<T, H> {
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(hash_instance: H::Instance, unhashed_phf: UnhashedPhf) -> Self {
        Self {
            hash_instance,
            unhashed_phf,
        }
    }

    /// Try to generate a perfect hash function.
    ///
    /// `keys` must not contain duplicates. It's an exact-size cloneable iterator rather than
    /// a slice reference so that multiple underlying containers can be used.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    ///
    /// # Panics
    ///
    /// Panics if `keys` contains more than `isize::MAX / 2` elements.
    #[cfg(feature = "build")]
    #[inline]
    #[allow(
        clippy::needless_pass_by_value,
        reason = "passing a reference here would complicate the API for no real gain, as a reference can implement this trait anyway"
    )]
    #[allow(clippy::arithmetic_side_effects, reason = "asserted")]
    pub fn try_from_keys<'b>(keys: impl ExactSizeIterator<Item = &'b T> + Clone) -> Option<Self>
    where
        T: 'b,
    {
        // Asserting this is enough to guarantee that `hash_space` never overflows.
        assert!(keys.len() <= isize::MAX as usize / 2, "Too many keys");

        let percent = keys.len().div_ceil(100);

        // Don't let hash_space grow beyond this
        let max_hash_space = (keys.len() + 5 * percent).next_power_of_two();

        // Start with different load factors for different sizes. This was tuned experimentally.
        let coeff = keys.len().div_ceil(1_000_000).min(5);
        let mut hash_space = keys.len() + coeff * percent;

        // Increase hash_space exponentially by 1.01 on each iteration until reaching a power of two
        // size. For good hashes, this loop should terminate soon.
        for hash_instance in H::iter() {
            if let Some(unhashed_phf) = UnhashedPhf::try_from_keys(
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

    /// Generate a perfect hash function.
    ///
    /// `keys` must not contain duplicates. It's an exact-size cloneable iterator rather than
    /// a slice reference so that multiple underlying containers can be used.
    ///
    /// # Panics
    ///
    /// Panics if `keys` contains more than `isize::MAX / 2` elements, or if the underlying
    /// imperfect hash function family is finite and generation didn't succeed.
    #[cfg(feature = "build")]
    #[inline]
    #[allow(
        clippy::needless_pass_by_value,
        reason = "passing a reference here would complicate the API for no real gain, as a reference can implement this trait anyway"
    )]
    pub fn from_keys<'b>(keys: impl ExactSizeIterator<Item = &'b T> + Clone) -> Self
    where
        T: 'b,
    {
        Self::try_from_keys(keys).expect("ran out of imperfect hash family instances")
    }

    /// Hash a key.
    ///
    /// The whole point. Guaranteed to return different indices for different keys from the training
    /// dataset. `key` is expected to already be hashed.
    ///
    /// May return arbitrary indices for keys outside the dataset.
    #[inline]
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
    #[inline]
    pub const fn capacity(&self) -> usize {
        self.unhashed_phf.capacity()
    }
}

#[cfg(feature = "codegen")]
impl<T, H: ImperfectHasher<T>> super::codegen::Codegen for Phf<T, H>
where
    H::Instance: super::codegen::Codegen,
{
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let phf = gen.path("h::Phf");
        let hash_instance = gen.piece(&self.hash_instance);
        let unhashed_phf = gen.piece(&self.unhashed_phf);
        quote::quote!(#phf::from_raw_parts(#hash_instance, #unhashed_phf))
    }
}
