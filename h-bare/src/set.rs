//! Perfect hash sets.

use super::{
    const_vec::ConstVec,
    hash::{GenericHasher, ImperfectHasher},
    Phf,
};
use core::borrow::Borrow;

/// A perfect hash set.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(
        bound(
            deserialize = "T: serde::Deserialize<'de>, H: serde::Deserialize<'de> + ImperfectHasher<T>"
        ),
        try_from = "SetInner<T, H>"
    )
)]
#[allow(
    clippy::unsafe_derive_deserialize,
    reason = "safety requirements are validated using TryFrom"
)]
pub struct Set<T, H = GenericHasher> {
    /// The actual set.
    inner: SetInner<T, H>,
}

/// The actual set.
///
/// This needs to be a separate type so that `serde` can convert from this type to [`Set`] with
/// [`TryFrom`] during deserialization, so that we can validate the set.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct SetInner<T, H> {
    /// A PHF mapping values to indices in [`data`](Self::data).
    phf: Phf<T, H>,

    /// The elements of the set, indexed by their perfect hashes. `None` means an element with such
    /// a hash is absent.
    data: ConstVec<Option<T>>,

    /// The number of elements in the set.
    ///
    /// This is equal to the number of `Some` values in [`data`](Self::data) and is used purely for
    /// optimization of [`len`](Set::len).
    len: usize,
}

#[cfg(feature = "build")]
impl<T, H: ImperfectHasher<T>> Set<T, H> {
    /// Try to generate a perfect hash set.
    ///
    /// There must not be duplicate elements in the input.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    #[inline] // heavy, but monomorphized anyway
    #[must_use]
    pub fn try_from_elements(elements: alloc::vec::Vec<T>) -> Option<Self> {
        let len = elements.len();
        let phf = Phf::try_from_keys(elements.iter())?;
        let mut data: alloc::vec::Vec<_> = (0..phf.capacity()).map(|_| None).collect();
        super::scatter::scatter(elements, |element| phf.hash(element), &mut data);
        Some(Self {
            inner: SetInner {
                phf,
                data: data.into(),
                len,
            },
        })
    }

    /// Generate a perfect hash set.
    ///
    /// There must not be duplicate elements in the input.
    ///
    /// # Panics
    ///
    /// Panics if the underlying imperfect hash function family is finite and generation didn't
    /// succeed.
    #[inline]
    #[must_use]
    pub fn from_elements(elements: alloc::vec::Vec<T>) -> Self {
        Self::try_from_elements(elements).expect("ran out of imperfect hash family instances")
    }
}

impl<T, H> Set<T, H> {
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn __from_raw_parts(phf: Phf<T, H>, data: ConstVec<Option<T>>, len: usize) -> Self {
        Self {
            inner: SetInner { phf, data, len },
        }
    }

    /// Get a reference to the element if present.
    #[inline]
    pub fn get<Q>(&self, value: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q>,
    {
        unsafe { self.inner.data.get_unchecked(self.inner.phf.hash(value)) }
            .as_ref()
            .filter(|&elem| elem.borrow() == value)
    }

    /// Check if the hashset contains a value.
    #[inline]
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q>,
    {
        self.get(value).is_some()
    }

    /// Get number of entries.
    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len
    }

    /// Check if the hashset is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Iterate through elements.
    ///
    /// The iteration order is unspecified, but is constant for a given set.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.inner.data.iter().filter_map(|opt| opt.as_ref())
    }
}

/// Scope for `serde`-related code.
#[cfg(feature = "serde")]
mod serde_support {
    use super::{ImperfectHasher, Set, SetInner};
    use displaydoc::Display;
    use thiserror::Error;

    /// Deserialization validation failures.
    #[derive(Debug, Display, Error)]
    pub enum Error {
        /// wrong data length
        WrongDataLength,

        /// wrong len
        WrongLen,

        /// misplaced element
        MisplacedElement,
    }

    impl<T, H: ImperfectHasher<T>> TryFrom<SetInner<T, H>> for Set<T, H> {
        type Error = Error;

        #[inline]
        fn try_from(inner: SetInner<T, H>) -> Result<Self, Error> {
            if inner.data.len() != inner.phf.capacity() {
                return Err(Error::WrongDataLength);
            }

            if inner.len != inner.data.iter().filter(|opt| opt.is_some()).count() {
                return Err(Error::WrongLen);
            }

            for (index, element) in inner.data.iter().enumerate() {
                if let Some(value) = element {
                    if inner.phf.hash(value) != index {
                        return Err(Error::MisplacedElement);
                    }
                }
            }

            Ok(Self { inner })
        }
    }
}

#[cfg(feature = "codegen")]
impl<T: super::codegen::Codegen, H: super::codegen::Codegen> super::codegen::Codegen for Set<T, H> {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let set = gen.path("h::Set");
        let phf = gen.piece(&self.inner.phf);
        let data = gen.piece(&self.inner.data);
        let len = gen.piece(&self.inner.len);
        quote::quote!(#set::from_raw_parts(#phf, #data, #len))
    }
}
