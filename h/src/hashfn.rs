use super::codegen::Codegen;
use alloc::vec::Vec;
use core::fmt;
use core::hash::Hasher;
use fastrand::Rng;

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
pub trait ImperfectHasher<T: ?Sized> {
    /// Type of instance of the hash function. Usually just a seed.
    type Instance: Clone + fmt::Debug;

    /// Hash a key.
    fn hash(instance: &Self::Instance, key: &T) -> u64;

    /// Iterate through the hash family.
    ///
    /// Constructs multiple instances of `Self`. The iterator may be finite if there's just a few
    /// different instances of the hash, or infinite.
    ///
    /// Generation is deterministic, ensuring rebuilding is a no-op.
    fn iter() -> impl Iterator<Item = Self::Instance>;
}

/// Generic imperfect hasher.
///
/// Can hash types that implement [`PortableHash`], which is like [`core::hash::Hash`] but with
/// a portability guarantee. No stability guarantees are provided regarding the resulting hashes.
///
/// Might be slower than necessary on structured data -- implement [`ImperfectHasher`] yourself if
/// this matters.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct GenericHasher;

/// Implementation details of the generic hasher.
///
/// # Informational
///
/// No stability guarantees are offered. This section is informational and can only be relied upon
/// for a given version of the crate.
///
/// For integers up to 64 bits long and `bool`, the hash is computed as
/// `(key as u64).wrapping_mul(seed)`, where `s`eed is stored in the low 64 bits of the instance.
/// This means that the hash value is invariant under extending the length of the integer type up to
/// 64 bits.
///
/// For 128-bit integers, `key_low.wrapping_mul(seed_low) ^ key_high.wrapping_mul(seed_high)` is
/// computed, where `low` and `high` are the low and the high 64 bits of the key/instance.
///
/// For other types, rapidhash is applied to the output of [`PortableHash`]. The seed is stored in
/// the low 64 bits of the instance.
impl<T: ?Sized + PortableHash> ImperfectHasher<T> for GenericHasher {
    type Instance = u128;

    #[inline]
    fn hash(instance: &Self::Instance, key: &T) -> u64 {
        #[allow(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            reason = "intentional"
        )]
        if let Some(x) = reinterpret_scalar_up_to_64bit(key) {
            x.wrapping_mul(*instance as u64)
        } else if let Some(&x) = unsafe { reinterpret_scalar::<_, u128>(key) } {
            (x as u64).wrapping_mul(*instance as u64)
                ^ ((x >> 64i32) as u64).wrapping_mul((*instance >> 64i32) as u64)
        } else if let Some(&x) = unsafe { reinterpret_scalar::<_, i128>(key) } {
            (x as u64).wrapping_mul(*instance as u64)
                ^ ((x >> 64i32) as u64).wrapping_mul((*instance >> 64i32) as u64)
        } else {
            let mut state = rapidhash::RapidHasher::new(*instance as u64);
            key.hash(&mut state);
            state.finish()
        }
    }

    #[inline]
    fn iter() -> impl Iterator<Item = Self::Instance> {
        // Hexadecimal digits of pi - 3
        let mut rng = Rng::with_seed(0x243f_6a88_85a3_08d3);
        core::iter::repeat_with(move || rng.u128(..))
    }
}

/// Portable alternative to [`core::hash::Hash`].
///
/// Much like with [`core::hash::Hash`], `Eq`-equal objects must imply equal data passed to the
/// hasher, and the data must be prefix-free.
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
    /// collection, call [`Hasher::write_usize`] to emit the size before hashing the elements.
    #[inline]
    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for piece in data {
            piece.hash(state);
        }
    }
}

impl<T: ?Sized + PortableHash> PortableHash for &T {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T: ?Sized + PortableHash> PortableHash for &mut T {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

macro_rules! impl_write {
    ($($ty:ty => $fn:ident,)*) => {
        $(
            impl PortableHash for $ty {
                #[inline]
                fn hash<H: Hasher>(&self, state: &mut H) {
                    state.$fn(*self);
                }
            }
        )*
    };
}
impl_write! {
    u8 => write_u8,
    u16 => write_u16,
    u32 => write_u32,
    u64 => write_u64,
    i8 => write_i8,
    i16 => write_i16,
    i32 => write_i32,
    i64 => write_i64,
}

impl PortableHash for usize {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(*self as u64);
    }
}

impl PortableHash for isize {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_i64(*self as i64);
    }
}

macro_rules! impl_str {
    ($($ty:ty),*) => {
        $(
            impl PortableHash for $ty {
                #[inline]
                fn hash<H: Hasher>(&self, state: &mut H) {
                    state.write(self.as_bytes());
                    state.write(&[0xff]);
                }
            }
        )*
    };
}
impl_str!(str, alloc::string::String);

impl<T: PortableHash> PortableHash for Vec<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        <[T] as PortableHash>::hash(self, state);
    }
}

impl<T: PortableHash> PortableHash for [T] {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.len());
        T::hash_slice(self, state);
    }
}

impl fmt::Display for Codegen<'_, GenericHasher> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "::h::GenericHasher")
    }
}

/// Cast `&T` to `&U` if the types are statically known to be equal.
///
/// # Safety
///
/// The cast-to type `U` must not contain lifetimes, not even `'static`.
unsafe fn reinterpret_scalar<T: ?Sized, U: ?Sized + 'static>(x: &T) -> Option<&U> {
    let ty = typeid::of::<T>();
    if ty == typeid::of::<U>() {
        return Some(unsafe { core::mem::transmute_copy::<&T, &U>(&x) });
    }
    // Scalars only implement Borrow for one level of references, so it's sound to just check &U and
    // not &&U and so on.
    if ty == typeid::of::<&U>() {
        return Some(*unsafe { core::mem::transmute_copy::<&T, &&U>(&x) });
    }
    None
}

fn reinterpret_scalar_up_to_64bit<T: ?Sized>(x: &T) -> Option<u64> {
    macro_rules! imp {
        ($($ty:ty),*) => {
            $(
                if let Some(&x) = unsafe { reinterpret_scalar::<T, $ty>(x) } {
                    #[allow(clippy::cast_lossless, reason = "generic code")]
                    #[allow(clippy::cast_sign_loss, reason = "intentional")]
                    return Some(x as u64);
                }
            )*
        };
    }
    imp!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, bool);
    None
}
