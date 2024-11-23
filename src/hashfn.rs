use super::codegen::Codegen;
use core::fmt;
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
    /// Type of instance of the hash function.
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

impl<'a, T: ?Sized, H> ImperfectHasher<&'a T> for H
where
    Self: ImperfectHasher<T>,
{
    type Instance = <Self as ImperfectHasher<T>>::Instance;

    fn hash(instance: &Self::Instance, key: &&T) -> u64 {
        <Self as ImperfectHasher<T>>::hash(instance, *key)
    }

    fn iter() -> impl Iterator<Item = Self::Instance> {
        <Self as ImperfectHasher<T>>::iter()
    }
}

/// Generic imperfect hasher.
///
/// Can hash most scalar types. No stability guarantees are provided regarding the resulting hashes.
/// Might be slower than necessary on structured data -- implement [`ImperfectHasher`] yourself if
/// this matters.
#[derive(Clone, Debug)]
pub struct GenericHasher;

macro_rules! impl_with_multiplication {
    ($($ty:ty)*) => {
        $(
            /// Multiplication by a random factor.
            impl ImperfectHasher<$ty> for GenericHasher {
                type Instance = u64;

                fn hash(instance: &u64, key: &$ty) -> u64 {
                    (*key as u64).wrapping_mul(*instance)
                }

                fn iter() -> impl Iterator<Item = u64> {
                    let mut rng = Rng::with_seed(0x29601eb394f0d178);
                    core::iter::repeat_with(move || rng.u64(..))
                }
            }
        )*
    };
}

impl_with_multiplication!(u8 u16 u32 u64 usize i8 i16 i32 i64 isize bool);

impl fmt::Display for Codegen<'_, GenericHasher> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "::h::GenericHasher")
    }
}
