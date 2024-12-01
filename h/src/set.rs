use super::{
    const_vec::ConstVec,
    hash::{GenericHasher, ImperfectHasher},
    Phf,
};
use core::borrow::Borrow;

/// A perfect hash set.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(try_from = "SetInner<T, H>"))]
#[allow(
    clippy::unsafe_derive_deserialize,
    reason = "safety requirements are validated using TryFrom"
)]
#[non_exhaustive]
pub struct Set<T, H: ImperfectHasher<T> = GenericHasher> {
    inner: SetInner<T, H>,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct SetInner<T, H: ImperfectHasher<T>> {
    phf: Phf<T, H>,
    data: ConstVec<Option<T>>,
    len: usize,
}

impl<T, H: ImperfectHasher<T>> Set<T, H> {
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(phf: Phf<T, H>, data: ConstVec<Option<T>>, len: usize) -> Self {
        Self {
            inner: SetInner { phf, data, len },
        }
    }

    /// Try to generate a perfect hash set.
    ///
    /// There must not be duplicate elements in the input.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    #[cfg(feature = "build")]
    #[inline] // heavy, but monomorphized anyway
    #[must_use]
    pub fn try_from_elements(elements: alloc::vec::Vec<T>) -> Option<Self> {
        let len = elements.len();
        let phf = Phf::try_from_keys(elements.iter())?;
        let mut data: alloc::vec::Vec<_> = (0..phf.capacity()).map(|_| None).collect();
        super::scatter::scatter(elements, |element| phf.hash(element), &mut data);
        Some(Self::from_raw_parts(phf, data.into(), len))
    }

    /// Generate a perfect hash set.
    ///
    /// There must not be duplicate elements in the input.
    ///
    /// # Panics
    ///
    /// Panics if the underlying imperfect hash function family is finite and generation didn't
    /// succeed.
    #[cfg(feature = "build")]
    #[inline]
    #[must_use]
    pub fn from_elements(elements: alloc::vec::Vec<T>) -> Self {
        Self::try_from_elements(elements).expect("ran out of imperfect hash family instances")
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

#[cfg(feature = "serde")]
mod serde_support {
    use super::{ImperfectHasher, Set, SetInner};
    use thiserror::Error;

    #[derive(Debug, Error)]
    pub enum Error {
        #[error("Wrong data length")]
        WrongDataLength,

        #[error("Wrong len")]
        WrongLen,
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

            Ok(Self { inner })
        }
    }
}

#[cfg(feature = "codegen")]
impl<T, H: ImperfectHasher<T>> super::codegen::Codegen for Set<T, H>
where
    T: super::codegen::Codegen,
    H: super::codegen::Codegen,
{
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let set = gen.path("h::Set");
        let phf = gen.piece(&self.inner.phf);
        let data = gen.piece(&self.inner.data);
        let len = gen.piece(&self.inner.len);
        quote::quote!(#set::from_raw_parts(#phf, #data, #len))
    }
}
