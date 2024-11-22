use super::unhashed::Phf as UnhashedPhf;
use core::borrow::Borrow;
use core::fmt::Debug;

/// A perfect hash function.
#[derive(Clone, Debug)]
pub struct Phf<T, H: HashFn<T>> {
    pub(crate) hash_fn: HashOrFallback<T, H>,
    pub(crate) unhashed_phf: UnhashedPhf,
}

impl<T, H: HashFn<T>> Phf<T, H> {
    /// Try to generate a perfect hash function.
    ///
    /// `keys` must not contain duplicates. It's an exact-size cloneable iterator rather than
    /// a slice reference so that multiple underlying containers can be used.
    ///
    /// Generation is not guaranteed to succeed for bad or small hash families. `None` is returned
    /// in this case. For infinite hash families, this function either hangs or returns `Some`.
    pub fn try_new<'a>(
        keys: impl Iterator<Item = &'a T> + ExactSizeIterator + Clone,
    ) -> Option<Self>
    where
        T: 'a,
    {
        let percent = keys.len().div_ceil(100);

        // Don't let hash_space grow beyond this
        let max_hash_space = (keys.len() + 5 * percent).next_power_of_two();

        // Increase hash_space exponentially by 1.01 on each iteration. For good hashes, this loop
        // should terminate soon.
        let mut hash_space = keys.len() + percent;
        for hash_fn in H::iter() {
            if hash_space > max_hash_space {
                break;
            }
            if let Some(unhashed_phf) = UnhashedPhf::try_new(
                keys.clone().map(|key| hash_fn.hash(key)).collect(),
                hash_space,
            ) {
                return Some(Self {
                    hash_fn: HashOrFallback::Hash(hash_fn),
                    unhashed_phf,
                });
            }
            // Both increase the hash space and change the hash function. This is especially
            // important for infinite families, which wouldn't progress otherwise.
            hash_space += hash_space.div_ceil(100);
        }

        // Switch to fallback
        for hash_fn in H::Fallback::iter() {
            if let Some(unhashed_phf) = UnhashedPhf::try_new(
                keys.clone().map(|key| hash_fn.hash(key)).collect(),
                max_hash_space,
            ) {
                return Some(Self {
                    hash_fn: HashOrFallback::Fallback(hash_fn),
                    unhashed_phf,
                });
            }
        }

        None
    }

    /// Hash a key.
    ///
    /// The whole point. Guaranteed to return different indices for different keys from the training
    /// dataset. `key` is expected to already be hashed.
    ///
    /// May return arbitrary indices for keys outside the dataset.
    pub fn hash<U: ?Sized>(&self, key: &U) -> usize
    where
        H: HashFn<U>,
        <H as HashFn<T>>::Fallback: HashFn<U>,
    {
        let key = match &self.hash_fn {
            HashOrFallback::Hash(hash_fn) => hash_fn.hash(key),
            HashOrFallback::Fallback(hash_fn) => hash_fn.hash(key),
        };
        self.unhashed_phf.hash(key)
    }

    /// Get the boundary on indices.
    ///
    /// The index returned by `hash` is guaranteed to be less than `capacity()`, even for keys
    /// outside the training dataset.
    pub fn capacity(&self) -> usize {
        self.unhashed_phf.capacity()
    }
}

#[derive(Clone, Debug)]
pub(crate) enum HashOrFallback<T: ?Sized, H: HashFn<T>> {
    Hash(H),
    Fallback(H::Fallback),
}

/// A hash function, specialized for a particular type.
///
/// Unlike [`core::hash::Hash`], this hash *must* be portable between platforms. This means that,
/// generally speaking, you cannot create it by combining [`core::hash::Hash`] and
/// [`core::hash::Hasher`].
///
/// In addition, if `HashFn<T>` and `HashFn<U>` are implemented for one type, their results must be
/// equal between "equivalent" instances of `T` and `U`, which usually means borrowed objects must
/// hash exactly as owned objects.
pub trait HashFn<T: ?Sized>: Clone + Debug {
    /// Hash a key.
    fn hash(&self, key: &T) -> u64;

    /// Iterate through the hash family.
    ///
    /// Constructs multiple instances of `Self`. The iterator may be finite if there's just a few
    /// different instances of the hash, or infinite.
    ///
    /// Generation is deterministic, ensuring rebuilding is a no-op.
    fn iter() -> impl Iterator<Item = Self>;

    /// A fallback hash function.
    ///
    /// For a "cheap" finite hash family, this specifies an "expensive" hash family to try if
    /// generating a PHF fails. To counteract the price of the fallback family, certain operations
    /// of the PHF are optimized at expense of the PHF size.
    ///
    /// For infinite hash families, this field is ignored. The fallback of a fallback is also
    /// ignored. Specify `Fallback = Self` if there's no other clear fallback or if the hash family
    /// is infinite.
    type Fallback: HashFn<T>;
}

/// Multiplication by a random factor.
#[derive(Clone, Debug)]
pub struct MultiplicativeU64Hash {
    pub(crate) factor: u64,
}

impl HashFn<u64> for MultiplicativeU64Hash {
    fn hash(&self, key: &u64) -> u64 {
        key.borrow().wrapping_mul(self.factor)
    }

    fn iter() -> impl Iterator<Item = Self> {
        use rand::{distributions::Standard, rngs::SmallRng, Rng, SeedableRng};
        core::iter::once(1)
            .chain(SmallRng::seed_from_u64(0x29601eb394f0d178).sample_iter(Standard))
            .map(|factor| Self { factor })
    }

    type Fallback = Self;
}
