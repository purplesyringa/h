//! [`Vec`](alloc::vec::Vec), (somewhat) usable in a `const` context.

use core::ops::{Deref, DerefMut};

/// [`Vec`](alloc::vec::Vec) that can reference data in `static`s.
///
/// This is effectively a `Cow<'static, [T]>` that can be used in runtime for non-`'static` `T`s.
///
/// This type is used to support two hash table building environments:
///
/// - In runtime, when the space has to be allocated on the heap and then freed.
/// - In compile time, when the data is stored in a `static`.
///
/// There's two pitfalls with this type:
///
/// 1. It implements [`DerefMut`], even though this is only reasonable for runtime-constructed data.
///    There's no way to disable it for constant data, so it just panics. To enforce the absence of
///    errors in compile time, the users of [`ConstVec`] should only return immutable references to
///    the [`ConstVec`] via public-facing APIs. This is why macros like `map!` return `&Map` instead
///    of `Map`.
///
/// 2. As [`ConstVec`] contains a [`Vec`](alloc::vec::Vec), it has non-`const` drop glue. This makes
///    the type unusable in `const` functions. This is unlikely to change before [wide drop
///    elaboration is enabled](https://github.com/rust-lang/rust/issues/73255).
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum ConstVec<T> {
    /// Semantically `&'static [T]`. That doesn't compile without `T: 'static`, and bounds cannot be
    /// placed on individual variants, so we have to use pointers.
    CompileTime(*const [T]),

    /// Runtime-allocated data.
    #[cfg(feature = "alloc")]
    RunTime(alloc::vec::Vec<T>),
}

// Necessary due to use of `*const [T]`.
// SAFETY: For `RunTime`, `T: Send` implies `Vec<T>: Send`. `CompileTime` can only be constructed
// given `T: Sync`, so naturally `&'static [T]: Send`.
unsafe impl<T: Send> Send for ConstVec<T> {}
// SAFETY: For `RunTime`, `T: Sync` implies `Vec<T>: Sync`. For `CompileTime`, `T: Sync` implies
// `&'static [T]: Sync`.
unsafe impl<T: Sync> Sync for ConstVec<T> {}

impl<T> ConstVec<T> {
    /// Initialize the vector from static data.
    ///
    /// Note that this method requires `T` to be [`Sync`]. This is because:
    ///
    /// 1. `from_static_ref` can be called on one slice multiple times.
    /// 2. `ConstVec<T>: Send` as long as `T: Send` for compatibility with [`Vec`](alloc::vec::Vec).
    ///    (We want `ConstVec` to be at least as applicable as `Vec`.)
    /// 3. `T`s that implement `Send` but not `Sync` exist.
    /// 4. Creating two such `ConstVec<T>`s from one chunk, sending one of them to another thread,
    ///    and then simultaneously accessing the `T`s from two threads is safe, but unsound.
    ///
    /// To prevent this unsoundness, we require that `T: Sync`. `T: Send => T: Sync` would also
    /// work, but Rust cannot express this constraint.
    #[inline]
    pub const fn from_static_ref(arr: &'static [T]) -> Self
    where
        T: Sync,
    {
        Self::CompileTime(arr)
    }

    /// Initialize the vector from runtime data.
    #[cfg(feature = "alloc")]
    #[inline]
    pub const fn from_vec(vec: alloc::vec::Vec<T>) -> Self {
        Self::RunTime(vec)
    }

    /// Initialize the vector from runtime data.
    #[cfg(not(feature = "alloc"))]
    #[inline]
    pub const fn from_vec(_vec: impl Deref<Target = [T]>) -> Self {
        const {
            panic!("`h` feature `alloc` is not enabled");
        }
    }
}

impl<T> Deref for ConstVec<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &[T] {
        match self {
            // SAFETY: `ptr` is semantically `&'static [T]`.
            Self::CompileTime(ptr) => unsafe { &**ptr },
            #[cfg(feature = "alloc")]
            Self::RunTime(ref vec) => vec,
        }
    }
}

impl<T> DerefMut for ConstVec<T> {
    /// # Panics
    ///
    /// Panics if initialized in compile time.
    #[inline]
    fn deref_mut(&mut self) -> &mut [T] {
        match self {
            Self::CompileTime(_) => panic!("Cannot mutably borrow a compile-time vector"),
            #[cfg(feature = "alloc")]
            Self::RunTime(ref mut vec) => vec,
        }
    }
}

#[cfg(feature = "codegen")]
impl<T: super::codegen::Codegen> super::codegen::Codegen for ConstVec<T> {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let const_vec = gen.path("h::low_level::ConstVec");
        let data = gen.array(&**self);
        if gen.mutability() {
            let vec = gen.path("alloc::vec");
            // This is a method invocation instead of a `From::from` call so that we get a slightly
            // better diagnostic when `h` is compiled without `-F alloc`.
            quote::quote!(#const_vec::from_vec(#vec!#data))
        } else {
            quote::quote!(#const_vec::from_static_ref(&#data))
        }
    }
}

/// Scope for `serde`-related code.
#[cfg(feature = "serde")]
mod serde_support {
    use super::ConstVec;
    use serde::ser::{Serialize, SerializeSeq, Serializer};

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
    impl<'de, T: serde::Deserialize<'de>> serde::Deserialize<'de> for ConstVec<T> {
        #[inline]
        fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            alloc::vec::Vec::<T>::deserialize(deserializer).map(Into::into)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn consttime() {
        let values = const { ConstVec::from_static_ref(&["meow", "nya", "cutie"]) };
        assert_eq!(values.get(0), Some(&"meow"));
        assert_eq!(values.get(1), Some(&"nya"));
        assert_eq!(values.get(2), Some(&"cutie"));
        assert_eq!(values.get(3), None);
    }

    #[test]
    #[should_panic(expected = "Cannot mutably borrow a compile-time vector")]
    fn const_unmut() {
        let mut values = const { ConstVec::from_static_ref(&["meow", "nya", "cutie"]) };
        values[1] = "mrrrrp";
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn runtime() {
        let mut values = ConstVec::from_vec(alloc::vec!["meow", "nya", "cutie"]);
        assert_eq!(values.get(0), Some(&"meow"));
        assert_eq!(values.get(1), Some(&"nya"));
        assert_eq!(values.get(2), Some(&"cutie"));
        assert_eq!(values.get(3), None);
        values[1] = "mrrrrp";
        assert_eq!(values.get(0), Some(&"meow"));
        assert_eq!(values.get(1), Some(&"mrrrrp"));
        assert_eq!(values.get(2), Some(&"cutie"));
        assert_eq!(values.get(3), None);
    }
}
