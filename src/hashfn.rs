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
///
/// Currrently uses wyhash, specialized with raw multiplication for scalar types.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct GenericHasher;

impl<T: ?Sized + PortableHash> ImperfectHasher<T> for GenericHasher {
    type Instance = u64;

    #[inline]
    fn hash(instance: &u64, key: &T) -> u64 {
        if let Some(x) = reinterpret_scalar(key) {
            x.wrapping_mul(*instance)
        } else {
            let mut state = wyhash::WyHash::with_seed(*instance);
            key.hash(&mut state);
            state.finish()
        }
    }

    #[inline]
    fn iter() -> impl Iterator<Item = u64> {
        // Hexadecimal digits of pi - 3
        let mut rng = Rng::with_seed(0x243f_6a88_85a3_08d3);
        core::iter::repeat_with(move || rng.u64(..))
    }
}

/// Portable alternative to [`core::hash::Hash`].
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
            piece.hash(state)
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
    usize => write_usize,
    i8 => write_i8,
    i16 => write_i16,
    i32 => write_i32,
    i64 => write_i64,
    isize => write_isize,
}

macro_rules! impl_bytes {
    ($($ty:ty),*) => {
        $(
            impl PortableHash for $ty {
                #[inline]
                fn hash<H: Hasher>(&self, state: &mut H) {
                    state.write(self.as_bytes());
                }
            }
        )*
    };
}
impl_bytes!(str, alloc::string::String);

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

fn reinterpret_scalar<T: ?Sized>(x: &T) -> Option<u64> {
    let ty = typeid::of::<T>();

    macro_rules! imp {
        ($($ty:ty),*) => {
            $(
                if ty == typeid::of::<$ty>() {
                    let x = unsafe { core::mem::transmute_copy::<&T, &$ty>(&x) };
                    return Some(*x as u64);
                }
                // Scalars only implement Borrow for one level of references, so it's sound to just
                // check &$ty and not &&$ty and so on.
                if ty == typeid::of::<&$ty>() {
                    let x = unsafe { core::mem::transmute_copy::<&T, &&$ty>(&x) };
                    return Some(**x as u64);
                }
            )*
        };
    }
    imp!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, bool);

    None
}
