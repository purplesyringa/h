//! Perfect hash sets.

use super::{const_vec::ConstVec, hash::PortableHash, Phf};
use core::borrow::Borrow;

/// A perfect hash set.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
#[cfg_attr(
    all(feature = "alloc", feature = "serde"),
    serde(
        bound(deserialize = "T: serde::Deserialize<'de> + PortableHash"),
        try_from = "SetInner<T>"
    )
)]
#[cfg_attr(
    all(feature = "alloc", feature = "serde"),
    expect(
        clippy::unsafe_derive_deserialize,
        reason = "safety requirements are validated using TryFrom"
    )
)]
pub struct Set<T> {
    /// The actual set.
    inner: SetInner<T>,
}

/// The actual set.
///
/// This needs to be a separate type so that `serde` can convert from this type to [`Set`] with
/// [`TryFrom`] during deserialization, so that we can validate the set.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
struct SetInner<T> {
    /// A PHF mapping values to indices in [`data`](Self::data).
    phf: Phf<T>,

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
impl<T: PortableHash> Set<T> {
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
        super::algorithms::scatter(elements, |element| phf.hash(element), &mut data);
        Some(Self {
            inner: SetInner {
                phf,
                data: ConstVec::from_vec(data),
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

impl<T> Set<T> {
    /// Initialize from saved data.
    ///
    /// Meant for codegen, not for public use.
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn __from_raw_parts(phf: Phf<T>, data: ConstVec<Option<T>>, len: usize) -> Self {
        Self {
            inner: SetInner { phf, data, len },
        }
    }

    /// Get a reference to the element if present.
    #[inline]
    pub fn get<Q>(&self, value: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: ?Sized + Eq + PortableHash,
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
        Q: ?Sized + Eq + PortableHash,
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
#[cfg(all(feature = "alloc", feature = "serde"))]
mod serde_support {
    use super::{PortableHash, Set, SetInner};
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

    impl<T: PortableHash> TryFrom<SetInner<T>> for Set<T> {
        type Error = Error;

        #[inline]
        fn try_from(inner: SetInner<T>) -> Result<Self, Error> {
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
impl<T: super::codegen::Codegen> super::codegen::Codegen for Set<T> {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let set = gen.path("h::Set");
        let phf = gen.piece(&self.inner.phf);
        let data = gen.piece(&self.inner.data);
        let len = gen.piece(&self.inner.len);
        quote::quote!(#set::__from_raw_parts(#phf, #data, #len))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use rapidhash::RapidRng;

    fn generate_elements(rng: &mut RapidRng) -> Vec<u64> {
        let n_elements: usize = (rng.next() % 10).try_into().unwrap();
        (0..n_elements).map(|_| rng.next()).collect()
    }

    #[test]
    fn set() {
        let mut rng = RapidRng::new(0x243f_6a88_85a3_08d3);
        for _ in 0..50 {
            let mut elements = generate_elements(&mut rng);
            let h_set: Set<u64> = Set::from_elements(elements.clone());

            for element in &elements {
                assert_eq!(h_set.get(element), Some(element));
                assert_eq!(h_set.contains(element), true);
            }
            assert_eq!(h_set.len(), elements.len());
            assert_eq!(h_set.is_empty(), elements.is_empty());

            assert_eq!(h_set.get(&0), None);
            assert_eq!(h_set.contains(&0), false);

            let mut elements2: Vec<u64> = h_set.iter().copied().collect();
            elements.sort_unstable();
            elements2.sort_unstable();
            assert_eq!(elements, elements2);
        }
    }
}
