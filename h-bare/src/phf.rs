//! Perfect hash functions with typed inputs.

use super::{
    hash::{GenericHasher, ImperfectHasher},
    untyped_phf::UntypedPhf,
};
use core::borrow::Borrow;
use core::marker::PhantomData;

/// A perfect hash function.
///
/// A mapping from `T` to numbers from `0` to `N - 1`, injective over the training key set. `N`
/// might be larger than the size of the training key set.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
pub struct Phf<T, H = GenericHasher> {
    /// The hasher, mapping `T` to a numeric imperfect hash.
    hash: H,

    /// The underlying untyped PHF.
    untyped_phf: UntypedPhf,

    /// Mark [`Phf`] as contravariant in `T`.
    _marker: PhantomData<fn(T)>,
}

#[cfg(feature = "build")]
impl<T, H: ImperfectHasher<T>> Phf<T, H> {
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
    #[inline]
    #[expect(
        clippy::needless_pass_by_value,
        reason = "passing a reference here would complicate the API for no real gain, as a reference can implement this trait anyway"
    )]
    #[expect(clippy::arithmetic_side_effects, reason = "asserted")]
    pub fn try_from_keys(
        keys: impl ExactSizeIterator<Item = impl Borrow<T>> + Clone,
    ) -> Option<Self> {
        // Asserting this is enough to guarantee that `hash_space` never overflows.
        assert!(keys.len() <= isize::MAX as usize / 2, "Too many keys");

        let percent = keys.len().div_ceil(100);

        // Don't let hash_space grow beyond this
        let max_hash_space = (keys.len() + 5 * percent).next_power_of_two();

        // Start with different load factors for different sizes. This was tuned experimentally.
        let coeff = keys.len().div_ceil(1_000_000).min(5);
        let mut hash_space = keys.len() + coeff * percent;

        // Increase hash_space by 0.5% on each iteration until reaching a power of size. For good
        // hashes, this loop should terminate soon.
        for hash in H::iter() {
            if let Some(untyped_phf) = UntypedPhf::try_from_keys(
                keys.clone().map(|key| hash.hash(key.borrow())).collect(),
                hash_space,
            ) {
                return Some(Self {
                    hash,
                    untyped_phf,
                    _marker: PhantomData,
                });
            }
            // Both increase the hash space and change the hash function. This is especially
            // important for infinite families, which wouldn't progress otherwise.
            hash_space = (hash_space + hash_space.div_ceil(200)).min(max_hash_space);
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
    #[inline]
    pub fn from_keys(keys: impl ExactSizeIterator<Item = impl Borrow<T>> + Clone) -> Self {
        Self::try_from_keys(keys).expect("ran out of imperfect hash family instances")
    }
}

impl<T, H> Phf<T, H> {
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn __from_raw_parts(hash: H, untyped_phf: UntypedPhf) -> Self {
        Self {
            hash,
            untyped_phf,
            _marker: PhantomData,
        }
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
        T: Borrow<U>,
        H: ImperfectHasher<U>,
    {
        self.untyped_phf.hash(self.hash.hash(key))
    }

    /// Get the boundary on indices.
    ///
    /// This is `N` such that all keys are within range `[0; N)`.
    ///
    /// The index returned by `hash` is guaranteed to *always* be less than `capacity()`, even for
    /// keys outside the training dataset.
    #[inline]
    pub const fn capacity(&self) -> usize {
        self.untyped_phf.capacity()
    }
}

#[cfg(feature = "codegen")]
impl<T, H: super::codegen::Codegen> super::codegen::Codegen for Phf<T, H> {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let phf = gen.path("h::Phf");
        let hash = gen.piece(&self.hash);
        let untyped_phf = gen.piece(&self.untyped_phf);
        quote::quote!(#phf::__from_raw_parts(#hash, #untyped_phf))
    }
}
