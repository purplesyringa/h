//! Imperfect hashes.
//!
//! PHFs with a small output range, i.e. suitable for use as indices in hash tables, are built by
//! refining imperfect hash functions -- still collision-free, but only with a full 64-bit output.
//!
//! Note that using [`core::hash::Hash`] for this is incorrect, as it's not portable between
//! platforms. If that hash is used, generating a hash map with a macro and then using it in runtime
//! may fail.
//!
//! Instead, this cache provides [`PortableHash`], which requires portability. You can implement
//! this trait for your types and then use the default [`GenericHasher`] for hash tables.
//! Alternatively, you can provide your own [`ImperfectHasher`] tuned to your data.

use core::fmt;
use core::hash::Hasher;
use rapidhash::RapidRng;

/// An imperfect hash function, specialized for a particular type.
///
/// Unlike [`core::hash::Hash`], this hash *must* be portable between platforms. This means that,
/// generally speaking, you cannot create it by combining [`core::hash::Hash`] and
/// [`core::hash::Hasher`].
///
/// Hashes of two objects that compare equal with [`Eq`] must be equal too.
///
/// In addition, if `ImperfectHasher<T>` and `ImperfectHasher<U>` are implemented for one type and
/// `T: Borrow<U>`, the hashes must be equal between `x` and `x.borrow()`.
pub trait ImperfectHasher<T: ?Sized>: Clone + fmt::Debug {
    /// Hash a key.
    fn hash(&self, key: &T) -> u64;

    /// Iterate through the hash family.
    ///
    /// Constructs multiple instances of `Self`. The iterator may be finite if there's just a few
    /// different instances of the hash, or infinite.
    ///
    /// Generation is deterministic, ensuring rebuilding is a no-op.
    fn iter() -> impl Iterator<Item = Self>;
}

/// Generic imperfect hasher.
///
/// Can hash types that implement [`PortableHash`], which is like [`core::hash::Hash`] but with
/// a portability guarantee. No stability guarantees are provided regarding the resulting hashes.
///
/// Might be slower than necessary on structured data -- implement [`ImperfectHasher`] yourself if
/// this matters.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct GenericHasher {
    low: u64,
    high: u64,
}

impl GenericHasher {
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(low: u64, high: u64) -> Self {
        Self { low, high }
    }
}

/// Implementation details of the generic hasher.
///
/// # Informational
///
/// No stability guarantees are offered. This section is informational and can only be relied upon
/// for a given version of the crate.
///
/// For integers up to 64 bits long and `bool`, the hash is computed as
/// `(key as u64).wrapping_mul(self.low)`. This means that the hash value is invariant under
/// extending the length of the integer type up to 64 bits.
///
/// For 128-bit integers, `low.wrapping_mul(self.low) ^ high.wrapping_mul(self.high)` is computed,
/// where `low` and `high` are the low and the high 64 bits of the key. This means that if
/// `self.high == 0`, hashing a 128-bit integer is equivalent to hashing its low 64-bit half.
///
/// For other types, rapidhash is applied to the output of [`PortableHash`], with `self.low` used as
/// the seed.
impl<T: ?Sized + PortableHash> ImperfectHasher<T> for GenericHasher {
    #[inline]
    fn hash(&self, key: &T) -> u64 {
        key.hash_generic_exclusive(self)
    }

    #[inline]
    fn iter() -> impl Iterator<Item = Self> {
        // Hexadecimal digits of pi - 3
        let mut rng = RapidRng::new(0x243f_6a88_85a3_08d3);
        core::iter::repeat_with(move || Self {
            low: rng.next(),
            high: rng.next(),
        })
    }
}

/// Portable alternative to [`core::hash::Hash`].
///
/// Much like with [`core::hash::Hash`], `Eq`-equal objects must imply equal data passed to the
/// hasher, and the data must be prefix-free.
///
/// Similarly, if `T: Borrow<U>` and both `T` and `U` implement [`PortableHash`], the hashes must be
/// equal between a value and its borrowed counterpart.
///
/// In addition, the written data must be portable between platforms. For example, directly writing
/// `usize` into the hasher is a bad idea because of possible differences in pointer size.
pub trait PortableHash {
    /// Write a value into the hasher.
    fn hash<H: Hasher>(&self, state: &mut H);

    /// Write a slice values into the hasher.
    ///
    /// This does not write the length of the slice beforehand and is semantically equivalent to
    /// calling [`PortableHash::hash`] for each element of the slice. When hashing a variable-length
    /// collection, call [`Hasher::write_u64`] to emit the size before hashing the elements.
    #[inline]
    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for piece in data {
            piece.hash(state);
        }
    }

    /// Compute the hash of a value using [`GenericHasher`].
    ///
    /// The purpose of this method is to hash small primitives more efficiently than with
    /// a streaming hash. As such, hashing a value as part of a larger value is not supported.
    ///
    /// This method should not usually be overloaded, as [`GenericHasher`] has no useful public
    /// methods. However, if `T: Borrow<U>`, the implementations of `hash_generic_exclusive` for
    /// the borrowed and unborrowed values must match. For example, a hypothetical implementation of
    /// `hash_generic_exclusive` for [`String`](alloc::string::String) needs to forward the
    /// computation to `hash_generic_exclusive` for [`str`].
    #[inline]
    fn hash_generic_exclusive(&self, hasher: &GenericHasher) -> u64 {
        let mut state = rapidhash::RapidHasher::new(hasher.low);
        self.hash(&mut state);
        state.finish()
    }
}

impl<T: ?Sized + PortableHash> PortableHash for &T {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }

    #[inline]
    fn hash_generic_exclusive(&self, hasher: &GenericHasher) -> u64 {
        (**self).hash_generic_exclusive(hasher)
    }
}

impl<T: ?Sized + PortableHash> PortableHash for &mut T {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }

    #[inline]
    fn hash_generic_exclusive(&self, hasher: &GenericHasher) -> u64 {
        (**self).hash_generic_exclusive(hasher)
    }
}

macro_rules! impl_primitive {
    ($($ty:ty => $fn:ident,)*) => {
        $(
            impl PortableHash for $ty {
                #[inline]
                fn hash<H: Hasher>(&self, state: &mut H) {
                    state.$fn(*self);
                }

                #[inline]
                fn hash_generic_exclusive(&self, hasher: &GenericHasher) -> u64 {
                    #[allow(clippy::cast_lossless, reason = "generic code")]
                    #[allow(clippy::cast_sign_loss, reason = "intentional")]
                    (*self as u64).wrapping_mul(hasher.low)
                }
            }
        )*
    };
}
impl_primitive! {
    u8 => write_u8,
    u16 => write_u16,
    u32 => write_u32,
    u64 => write_u64,
    i8 => write_i8,
    i16 => write_i16,
    i32 => write_i32,
    i64 => write_i64,
}

macro_rules! impl_primitive_128 {
    ($($ty:ty => $fn:ident,)*) => {
        $(
            impl PortableHash for $ty {
                #[inline]
                fn hash<H: Hasher>(&self, state: &mut H) {
                    state.$fn(*self);
                }

                #[inline]
                #[allow(
                    clippy::cast_possible_truncation,
                    clippy::cast_sign_loss,
                    reason = "intentional"
                )]
                fn hash_generic_exclusive(&self, hasher: &GenericHasher) -> u64 {
                    let low = *self as u64;
                    let high = (*self >> 64i32) as u64;
                    low.wrapping_mul(hasher.low) ^ high.wrapping_mul(hasher.high)
                }
            }
        )*
    };
}
impl_primitive_128! {
    u128 => write_u128,
    i128 => write_i128,
}

impl PortableHash for usize {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(*self as u64);
    }

    #[inline]
    fn hash_generic_exclusive(&self, hasher: &GenericHasher) -> u64 {
        (*self as u64).wrapping_mul(hasher.low)
    }
}

impl PortableHash for isize {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_i64(*self as i64);
    }

    #[inline]
    fn hash_generic_exclusive(&self, hasher: &GenericHasher) -> u64 {
        #[allow(clippy::cast_sign_loss, reason = "intentional")]
        (*self as u64).wrapping_mul(hasher.low)
    }
}

impl PortableHash for bool {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u8(u8::from(*self));
    }
}

impl PortableHash for char {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u32(*self as u32);
    }
}

// Floats shouldn't implement `Hash` because they don't implement `Eq`

macro_rules! impl_str {
    ($ty:ty) => {
        impl PortableHash for $ty {
            #[inline]
            fn hash<H: Hasher>(&self, state: &mut H) {
                state.write(self.as_bytes());
                state.write(&[0xff]);
            }
        }
    };
}
impl_str!(str);
#[cfg(feature = "alloc")]
impl_str!(alloc::string::String);

#[cfg(feature = "alloc")]
impl<T: PortableHash> PortableHash for alloc::vec::Vec<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        <[T] as PortableHash>::hash(self, state);
    }
}

impl<T: PortableHash> PortableHash for [T] {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.len() as u64);
        T::hash_slice(self, state);
    }
}

#[cfg(feature = "codegen")]
impl super::codegen::Codegen for GenericHasher {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let generic_hasher = gen.path("h::hash::GenericHasher");
        let low = gen.piece(&self.low);
        let high = gen.piece(&self.high);
        quote::quote!(#generic_hasher::from_raw_parts(#low, #high))
    }
}
