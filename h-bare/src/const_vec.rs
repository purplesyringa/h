use core::ops::{Deref, DerefMut};

#[derive(Clone, Debug)]
#[doc(hidden)]
pub struct ConstVec<T> {
    inner: Inner<T>,
}

#[derive(Clone, Debug)]
enum Inner<T> {
    /// Semantically `&'static [T]`. That doesn't compile without `T: 'static`, and bounds cannot be
    /// placed on individual variants, so there's that.
    CompileTime(*const [T]),

    #[cfg(feature = "alloc")]
    RunTime(alloc::vec::Vec<T>),
}

unsafe impl<T: Send> Send for Inner<T> {}
unsafe impl<T: Sync> Sync for Inner<T> {}

impl<T> ConstVec<T> {
    #[inline]
    pub const fn from_static_ref(arr: &'static [T]) -> Self {
        Self {
            inner: Inner::CompileTime(arr),
        }
    }
}

impl<T> From<&'static [T]> for ConstVec<T> {
    #[inline]
    fn from(arr: &'static [T]) -> Self {
        Self::from_static_ref(arr)
    }
}

#[cfg(feature = "alloc")]
impl<T> From<alloc::vec::Vec<T>> for ConstVec<T> {
    #[inline]
    fn from(vec: alloc::vec::Vec<T>) -> Self {
        Self {
            inner: Inner::RunTime(vec),
        }
    }
}

impl<T> Deref for ConstVec<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &[T] {
        match self.inner {
            Inner::CompileTime(ptr) => unsafe { &*ptr },
            #[cfg(feature = "alloc")]
            Inner::RunTime(ref vec) => vec,
        }
    }
}

impl<T> DerefMut for ConstVec<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut [T] {
        match self.inner {
            Inner::CompileTime(_) => panic!("Cannot mutably borrow a compile-time vector"),
            #[cfg(feature = "alloc")]
            Inner::RunTime(ref mut vec) => vec,
        }
    }
}

#[cfg(feature = "codegen")]
impl<T: super::codegen::Codegen> super::codegen::Codegen for ConstVec<T> {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let const_vec = gen.path("h::low_level::ConstVec");
        let data = gen.piece(&**self);
        if gen.mutability() {
            let vec = gen.path("alloc::vec");
            quote::quote!(#const_vec::from(#vec!#data))
        } else {
            quote::quote!(#const_vec::from_static_ref(&#data))
        }
    }
}

#[cfg(feature = "serde")]
mod serde_support {
    use super::ConstVec;
    use alloc::vec::Vec;
    use serde::{
        ser::{Serialize, SerializeSeq, Serializer},
        Deserialize, Deserializer,
    };

    impl<T: Serialize> Serialize for ConstVec<T> {
        #[inline]
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            let mut seq = serializer.serialize_seq(Some(self.len()))?;
            for e in &**self {
                seq.serialize_element(e)?;
            }
            seq.end()
        }
    }

    #[cfg(feature = "alloc")]
    impl<'de, T: Deserialize<'de>> Deserialize<'de> for ConstVec<T> {
        #[inline]
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            Vec::<T>::deserialize(deserializer).map(Into::into)
        }
    }
}
