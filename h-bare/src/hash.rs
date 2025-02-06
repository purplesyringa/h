//! Imperfect hashes.
//!
//! PHFs with a small output range, i.e. suitable for use as indices in hash tables, are built by
//! refining imperfect hash functions -- still collision-free, but only with a full 64-bit output.
//!
//! As the [`Hash`](core::hash::Hash) trait is not portable between platforms, and PHFs can be
//! generated in compile-time, this crate provides the [`PortableHash`] trait to guarantee
//! portability. It has a similar API and can be implemented for custom types by hand.
//!
//! There is one major difference from the standard hashing API. Instead of using a hasher-supplied
//! general-purpose [`BuildHasher::hash_one`](core::hash::BuildHasher::hash_one) method for hashing
//! elements, the type-specific method [`PortableHash::hash_one`] is used, which can be overloaded
//! on an individual basis for certain key types to use a faster hash function.

use core::hash::Hasher;
use rapidhash::RapidRng;

/// A random seed.
///
/// This is an opaque object of an unspecified bitness, but at least 64 pseudo-random bits can be
/// extracted with [`Seed::get_u64`].
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Seed {
    /// Multiplier for the low half for hashing a 128-bit value. Also dupes as a rapidhash seed.
    low: u64,
    /// Multiplier for the high half for hashing a 128-bit value.
    high: u64,
}

impl Seed {
    /// Initialize from saved data.
    ///
    /// Meant for codegen, not for public use.
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn __from_raw_parts(low: u64, high: u64) -> Self {
        Self { low, high }
    }

    /// Get 64 bits of the seed.
    ///
    /// This is not an infinite PRNG: this method returns always returns a fixed value when called
    /// on a given object.
    #[inline]
    pub fn get_u64(&self) -> u64 {
        self.low
    }

    /// Iterate through pseudo-random seeds.
    ///
    /// This returns an infinite iterator of different instances of `Self`. The returned values are
    /// deterministic.
    #[inline]
    pub fn iter() -> impl Iterator<Item = Self> {
        // Hexadecimal digits of pi - 3
        let mut rng = RapidRng::new(0x243f_6a88_85a3_08d3);
        core::iter::repeat_with(move || Self {
            low: rng.next(),
            high: rng.next(),
        })
    }
}

/// Portable alternative to [`Hash`](core::hash::Hash).
///
/// # Requirements
///
/// - Much like with [`core::hash::Hash`], `Eq`-equal objects must imply equal data passed to the
///   hasher. Similarly, if `T: Borrow<U>` and both `T` and `U` implement [`PortableHash`], the
///   data written when hashing the value and its borrowed counterpart must match. For example,
///   a hypothetical implementation of `hash` for [`String`](alloc::string::String) needs to forward
///   the computation to `hash` for [`str`].
///
/// - Unlike [`core::hash::Hash`], the written data must be portable between platforms. For example,
///   directly writing `usize` into the hasher is a bad idea because of possible differences in
///   pointer size.
///
/// - When hashing two objects that compare unequal, the sequence of `write_*` calls *must* be
///   different between the two objects. The *first* different `write_*` call must be an invocation
///   of *the same* method with different arguments. If that method is `write`, the byte string
///   written by one object must not be a prefix of the byte string written by another object.
///
/// If the `hash_one` method is overloaded, its implementation must also satisfy a few other
/// requirements listed in its documentation section.
pub trait PortableHash {
    /// Write a value into the hasher.
    fn hash<H: Hasher>(&self, state: &mut H);

    /// Write a slice of values into the hasher.
    ///
    /// This is semantically equivalent to calling [`PortableHash::hash`] for each element of the
    /// slice one by one, but is not guaranteed to produce an equivalent hash.
    ///
    /// This method does not necessarily write the length of the slice beforehand, so an invocation
    /// of `hash_slice` on a variable-length slice usually needs to be leaded by writing the length
    /// of the slice with [`Hasher::write_u64`].
    #[inline]
    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for piece in data {
            piece.hash(state);
        }
    }

    /// Compute the hash of a single value.
    ///
    /// By default, this method creates an unspecified general-purpose hasher from the seed, calls
    /// the `hash` method, and returns the finalized hash value. It can be overloaded to provide
    /// a custom, more efficient hasher, e.g. for small data.
    ///
    /// # Requirements
    ///
    /// The implementation must satisfy the following requirements:
    ///
    /// - `Eq`-equal objects must return equal hashes, and if `T: Borrow<U>` and both `T` and `U`
    ///   implement [`PortableHash`], the values returned by `hash_one` when hashing the value and
    ///   its borrowed counterpart must match.
    ///
    /// - The returned value must match across different platforms.
    ///
    /// - The hash must be almost universal. In other words, if `a != b`, `a` and `b` must have
    ///   different hashes for almost all seeds. Note that this explicitly forbids seed-independent
    ///   collisions, which would in practice lead to the PHF generation hanging.
    ///
    /// # Implementation details
    ///
    /// This section is purely informational and cannot be relied upon except for a particular
    /// version of the crate. No stability guarantees are offered.
    ///
    /// For integers up to 64 bits long, `bool`, and `char`, the hash is computed as
    /// `(key as u64).wrapping_mul(seed.low)`. For 128-bit integers,
    /// `low.wrapping_mul(seed.low) ^ high.wrapping_mul(seed.high)` is computed, where `low` and
    /// `high` are the low and the high 64 bits of the key.
    ///
    /// This means that as long as `seed.high == 0`, the hash of an integer is invariant under sign-
    /// or zero-extending it to 128 bits (like `as u128` does).
    ///
    /// For other types, the `hash` method is applied to [`RapidHasher`](rapidhash::RapidHasher),
    /// with `seed.low` used as the rapidhash seed.
    #[inline]
    fn hash_one(&self, seed: &Seed) -> u64 {
        let mut state = rapidhash::RapidHasher::new(seed.low);
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
    fn hash_one(&self, seed: &Seed) -> u64 {
        (**self).hash_one(seed)
    }
}

impl<T: ?Sized + PortableHash> PortableHash for &mut T {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }

    #[inline]
    fn hash_one(&self, seed: &Seed) -> u64 {
        (**self).hash_one(seed)
    }
}

/// Implement [`PortableHash`] for small integers by calling into [`Hasher`].
macro_rules! impl_primitive {
    ($($ty:ty => $method:ident,)*) => {
        $(
            impl PortableHash for $ty {
                #[inline]
                fn hash<H: Hasher>(&self, state: &mut H) {
                    state.$method(*self);
                }

                #[inline]
                fn hash_one(&self, seed: &Seed) -> u64 {
                    #[allow(clippy::cast_lossless, reason = "generic code")]
                    #[allow(clippy::cast_sign_loss, reason = "intentional")]
                    (*self as u64).wrapping_mul(seed.low)
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

/// Implement [`PortableHash`] for 128-bit integers by calling into [`Hasher`].
macro_rules! impl_primitive_128 {
    ($($ty:ty => $method:ident,)*) => {
        $(
            impl PortableHash for $ty {
                #[inline]
                fn hash<H: Hasher>(&self, state: &mut H) {
                    state.$method(*self);
                }

                #[inline]
                #[expect(clippy::cast_possible_truncation, reason = "intentional")]
                #[allow(clippy::cast_sign_loss, reason = "intentional")]
                fn hash_one(&self, seed: &Seed) -> u64 {
                    let low = *self as u64;
                    let high = (*self >> 64i32) as u64;
                    low.wrapping_mul(seed.low) ^ high.wrapping_mul(seed.high)
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
    fn hash_one(&self, seed: &Seed) -> u64 {
        (*self as u64).wrapping_mul(seed.low)
    }
}

impl PortableHash for isize {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_i64(*self as i64);
    }

    #[inline]
    fn hash_one(&self, seed: &Seed) -> u64 {
        #[expect(clippy::cast_sign_loss, reason = "intentional")]
        (*self as u64).wrapping_mul(seed.low)
    }
}

impl PortableHash for bool {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u8(u8::from(*self));
    }

    #[inline]
    fn hash_one(&self, seed: &Seed) -> u64 {
        u64::from(*self).wrapping_mul(seed.low)
    }
}

impl PortableHash for char {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u32(*self as u32);
    }

    #[inline]
    fn hash_one(&self, seed: &Seed) -> u64 {
        u64::from(*self).wrapping_mul(seed.low)
    }
}

// Floats shouldn't implement `Hash` because they don't implement `Eq`

/// Implement [`PortableHash`] for types that represent UTF-8 strings.
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
impl super::codegen::Codegen for Seed {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let seed = gen.path("h::hash::Seed");
        let low = gen.piece(&self.low);
        let high = gen.piece(&self.high);
        quote::quote!(#seed::__from_raw_parts(#low, #high))
    }
}
