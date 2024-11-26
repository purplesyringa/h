use super::{
    hash::{GenericHasher, ImperfectHasher},
    BorrowedOrOwnedSlice, Phf,
};
use core::borrow::Borrow;

/// A perfect hash set.
#[non_exhaustive]
pub struct Set<'a, T, H: ImperfectHasher<T> = GenericHasher> {
    phf: Phf<'a, T, H>,
    data: BorrowedOrOwnedSlice<'a, Option<T>>,
    len: usize,
}

impl<'a, T, H: ImperfectHasher<T>> Set<'a, T, H> {
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(
        phf: Phf<'a, T, H>,
        data: BorrowedOrOwnedSlice<'a, Option<T>>,
        len: usize,
    ) -> Self {
        Self { phf, data, len }
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
        Some(Self::from_raw_parts(
            phf,
            BorrowedOrOwnedSlice::Owned(data),
            len,
        ))
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

    /// Produce a copy of [`Set`] that references this one instead of owning data.
    ///
    /// This is useful for code generation, so that references to slices are generated to statics
    /// instead of dynamically allocated vectors, so that [`Set`] can be initialized in a `const`
    /// context.
    #[cfg(feature = "build")]
    #[inline]
    pub fn borrow(&self) -> Set<'_, T, H> {
        Set {
            phf: self.phf.borrow(),
            data: BorrowedOrOwnedSlice::Borrowed(&*self.data),
            len: self.len,
        }
    }

    /// Get a reference to the element if present.
    #[inline]
    pub fn get<Q>(&self, value: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q, Instance = <H as ImperfectHasher<T>>::Instance>,
    {
        unsafe { self.data.get_unchecked(self.phf.hash(value)) }
            .as_ref()
            .filter(|&elem| elem.borrow() == value)
    }

    /// Check if the hashset contains a value.
    #[inline]
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: ?Sized + Eq,
        H: ImperfectHasher<Q, Instance = <H as ImperfectHasher<T>>::Instance>,
    {
        self.get(value).is_some()
    }

    /// Get number of entries.
    #[inline]
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Check if the hashset is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Iterate through elements.
    ///
    /// The iteration order is unspecified, but is constant for a given set.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.data.iter().filter_map(|opt| opt.as_ref())
    }
}

#[cfg(feature = "codegen")]
impl<'a, T, H: ImperfectHasher<T>> super::codegen::Codegen for Set<'a, T, H>
where
    Phf<'a, T, H>: super::codegen::Codegen,
    T: super::codegen::Codegen,
{
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let set = gen.path("h::Set");
        let phf = gen.piece(&self.phf);
        let data = gen.piece(&self.data);
        let len = gen.piece(&self.len);
        quote::quote!(#set::from_raw_parts(#phf, #data, #len))
    }
}
