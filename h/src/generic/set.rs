use super::hashed::Phf;
use crate::hash::{GenericHasher, ImperfectHasher};
use core::borrow::Borrow;
use core::marker::PhantomData;
use core::ops::Deref;

/// A perfect hash set.
///
/// This type supports most read-only methods that [`std::collections::HashSet`] supports.
///
/// By default, [`Set`] can be both built and used in runtime. Use [`StaticSet`] and [`static_set!`]
/// when the data is available in compile time, as this results in better performance.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Set<T, C, D, H: ImperfectHasher<T> = GenericHasher> {
    phf: Phf<T, D, H>,
    data: C,
    len: usize,
    marker: PhantomData<T>,
}

impl<T, C, D, H> Set<T, C, D, H>
where
    C: Deref<Target = [Option<T>]>,
    D: Deref<Target = [u16]>,
    H: ImperfectHasher<T>,
{
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(phf: Phf<T, D, H>, data: C, len: usize) -> Self {
        Self {
            phf,
            data,
            len,
            marker: PhantomData,
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
impl<T, C: Deref<Target = [Option<T>]>, H: ImperfectHasher<T>> super::codegen::Codegen
    for Set<T, C, H>
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
