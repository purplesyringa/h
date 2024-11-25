use crate::{
    generic,
    hash::{GenericHasher, ImperfectHasher},
};
use alloc::vec::Vec;

/// A runtime perfect hash set.
pub type Set<T, H = GenericHasher> = generic::Set<T, Vec<Option<T>>, Vec<u16>, H>;

impl<T, H: ImperfectHasher<T>> Set<T, H> {
    /// Try to generate a perfect hash set.
    ///
    /// There must not be duplicate elements in the input.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    ///
    /// To instantiate [`StaticSet`], use [`static_set!`].
    #[inline] // heavy, but monomorphized anyway
    #[must_use]
    pub fn try_from_elements(elements: Vec<T>) -> Option<Self> {
        let len = elements.len();
        let phf = generic::Phf::try_from_keys(elements.iter())?;
        let mut data: Vec<Option<T>> = (0..phf.capacity()).map(|_| None).collect();
        super::scatter::scatter(elements, |element| phf.hash(element), &mut data);
        Some(Self::from_raw_parts(phf, data, len))
    }

    /// Generate a perfect hash set.
    ///
    /// There must not be duplicate elements in the input.
    ///
    /// To instantiate [`StaticSet`], use [`static_set!`].
    ///
    /// # Panics
    ///
    /// Panics if the underlying imperfect hash function family is finite and generation didn't
    /// succeed.
    #[inline]
    #[must_use]
    pub fn from_elements(elements: Vec<T>) -> Self {
        Self::try_from_elements(elements).expect("ran out of imperfect hash family instances")
    }
}
