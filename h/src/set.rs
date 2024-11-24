use super::{
    hash::{GenericHasher, ImperfectHasher},
    scatter::scatter,
    Phf,
};
use alloc::vec::Vec;
use core::borrow::Borrow;
use core::marker::PhantomData;
use core::ops::Deref;

/// A constant-time perfect hash set.
///
/// This type supports most read-only methods that [`std::collections::HashSet`] supports.
///
/// Construct with [`static_set!`].
pub type StaticSet<T, H = GenericHasher> = Set<T, H, &'static [Option<T>]>;

/// A perfect hash set.
///
/// This type supports most read-only methods that [`std::collections::HashSet`] supports.
///
/// By default, [`Set`] can be both built and used in runtime. Use [`StaticSet`] and [`static_set!`]
/// when the data is available in compile time, as this results in better performance.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Set<T, H: ImperfectHasher<T> = GenericHasher, C = Vec<Option<T>>> {
    phf: Phf<T, H>,
    data: C,
    len: usize,
    marker: PhantomData<T>,
}

impl<T, H, C> Set<T, H, C>
where
    H: ImperfectHasher<T>,
    C: Deref<Target = [Option<T>]>,
{
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(phf: Phf<T, H>, data: C, len: usize) -> Self {
        Self {
            phf,
            data,
            len,
            marker: PhantomData,
        }
    }

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
    pub fn try_from_elements(elements: Vec<T>) -> Option<Self>
    where
        Vec<Option<T>>: Into<C>,
    {
        let len = elements.len();
        let phf = Phf::try_from_keys(elements.iter())?;
        let mut data: Vec<Option<T>> = (0..phf.capacity()).map(|_| None).collect();
        scatter(elements, |element| phf.hash(element), &mut data);
        Some(Self {
            phf,
            data: data.into(),
            len,
            marker: PhantomData,
        })
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
    pub fn from_elements(elements: Vec<T>) -> Self
    where
        Vec<Option<T>>: Into<C>,
    {
        Self::try_from_elements(elements).expect("ran out of imperfect hash family instances")
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
impl<T, H: ImperfectHasher<T>, C: Deref<Target = [Option<T>]>> super::codegen::Codegen
    for Set<T, H, C>
where
    Phf<T, H>: super::codegen::Codegen,
    T: super::codegen::Codegen,
{
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let static_set = gen.path("h::StaticSet");
        let phf = gen.piece(&self.phf);
        let data = gen.piece(&&*self.data);
        let len = gen.piece(&self.len);
        quote::quote!(#static_set::from_raw_parts(#phf, #data, #len))
    }
}
