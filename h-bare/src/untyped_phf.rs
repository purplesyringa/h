//! Perfect hash functions with untyped inputs.

#![expect(clippy::arithmetic_side_effects, reason = "many false positives")]

use super::const_vec::ConstVec;
#[cfg(feature = "build")]
use {
    super::{algorithms::group_by_key, bitmap::BitMap},
    alloc::{vec, vec::Vec},
    const_dispatch::prelude::*,
};

/// We assume that usize is at most 64-bit in many places.
const _: () = assert!(
    size_of::<usize>() <= size_of::<u64>(),
    "I want your computer."
);

/// A perfect hash function.
///
/// This PHF does not hash keys for you.
#[derive(Clone, Debug)]
#[cfg_attr(
    feature = "serde",
    expect(
        clippy::unsafe_derive_deserialize,
        reason = "safety requirements are validated using TryFrom"
    )
)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
#[cfg_attr(
    all(feature = "alloc", feature = "serde"),
    serde(try_from = "UntypedPhfInner")
)]
pub struct UntypedPhf {
    /// The actual PHF.
    inner: UntypedPhfInner,
}

/// The actual PHF.
///
/// This needs to be a separate type so that `serde` can convert from this type to [`UntypedPhf`]
/// with [`TryFrom`] during deserialization, so that we can validate the PHF.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(all(feature = "alloc", feature = "serde"), derive(serde::Deserialize))]
struct UntypedPhfInner {
    /// Size of the hash table, without taking out-of-bounds displacements into account
    hash_space: usize,

    /// Size of the hash table, taking out-of-bounds displacements into account
    hash_space_with_oob: usize,

    /// `64 - ilog2(bucket_count)`
    bucket_shift: u32,

    /// Per-bucket displacement values
    displacements: ConstVec<u16>,

    /// How the displacement is mixed into the approximate position
    mixer: Mixer,
}

impl UntypedPhf {
    /// Initialize from saved data.
    ///
    /// Meant for codegen, not for public use.
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn __from_raw_parts(
        hash_space: usize,
        hash_space_with_oob: usize,
        bucket_shift: u32,
        displacements: ConstVec<u16>,
        mixer: Mixer,
    ) -> Self {
        Self {
            inner: UntypedPhfInner {
                hash_space,
                hash_space_with_oob,
                bucket_shift,
                displacements,
                mixer,
            },
        }
    }

    /// Generate a perfect hash function.
    ///
    /// `hash_space` must be at least somewhat higher than `keys.len()`. `keys` are expected to
    /// already be hashed; may easily fail for bad hashes or just if it feels like it. Try to retry
    /// with other parameters in this case.
    ///
    /// If there are duplicates in `keys`, returns `None`.
    #[cfg(feature = "build")]
    #[expect(
        clippy::missing_inline_in_public_items,
        reason = "very heavy, we'd rather not copy it to every crate"
    )]
    #[must_use]
    pub fn try_from_keys(
        keys: impl ExactSizeIterator<Item = u64> + Clone,
        hash_space: usize,
    ) -> Option<Self> {
        if keys.len() == 0 {
            return Some(Self {
                inner: UntypedPhfInner {
                    hash_space: 0,
                    hash_space_with_oob: 1,
                    bucket_shift: 63,
                    displacements: ConstVec::from_static_ref(&[0, 0]),
                    mixer: Mixer::Add,
                },
            });
        }

        let buckets = Buckets::try_from_keys(keys, hash_space)?;
        // Add is faster to generate when successful -- try it first
        buckets
            .try_generate_phf(Mixer::Add)
            .or_else(|| buckets.try_generate_phf(Mixer::Xor))
    }

    /// Hash a key.
    ///
    /// The whole point. Guaranteed to return different indices for different keys from the training
    /// dataset. `key` is expected to already be hashed.
    ///
    /// May return arbitrary indices for keys outside the dataset.
    #[inline]
    #[must_use]
    pub fn hash(&self, key: u64) -> usize {
        let (approx, bucket) =
            to_approx_bucket(key, self.inner.hash_space, self.inner.bucket_shift);
        let displacement = unsafe { *self.inner.displacements.get_unchecked(bucket) };
        self.inner.mixer.mix(approx, displacement)
    }

    /// Get the boundary on indices.
    ///
    /// The index returned by `hash` is guaranteed to be less than `capacity()`, even for keys
    /// outside the training dataset.
    #[inline]
    #[must_use]
    pub const fn capacity(&self) -> usize {
        self.inner.hash_space_with_oob
    }
}

/// Scope for `serde`-related code.
#[cfg(all(feature = "alloc", feature = "serde"))]
mod serde_support {
    use super::{UntypedPhf, UntypedPhfInner};
    use displaydoc::Display;
    use thiserror::Error;

    /// Deserialization validation failures.
    #[derive(Debug, Display, Error)]
    pub enum Error {
        /// too large `bucket_shift` value
        TooLargeBucketShift,

        /// wrong displacement count
        WrongDisplacementCount,

        /// wrong capacity
        WrongCapacity,
    }

    impl TryFrom<UntypedPhfInner> for UntypedPhf {
        type Error = Error;

        #[inline]
        fn try_from(inner: UntypedPhfInner) -> Result<Self, Error> {
            if inner.bucket_shift >= 64 {
                return Err(Error::TooLargeBucketShift);
            }

            let bucket_count = 1 << (64 - inner.bucket_shift);
            if bucket_count != inner.displacements.len() as u64 {
                return Err(Error::WrongDisplacementCount);
            }

            let expected_hash_space_with_oob = inner
                .mixer
                .get_hash_space_with_oob(inner.hash_space, &inner.displacements);
            if inner.hash_space_with_oob != expected_hash_space_with_oob {
                return Err(Error::WrongCapacity);
            }

            Ok(Self { inner })
        }
    }
}

/// Keys, split into buckets.
#[cfg(feature = "build")]
struct Buckets {
    /// Approx values of keys, ordered such that all buckets are consecutive
    approxs: Vec<u64>,

    /// Buckets, grouped by size. The tuple is `(Bucket, left)`.
    by_size: Vec<Vec<(usize, usize)>>,

    /// The `hash_space` parameter this object is valid under
    hash_space: usize,

    /// The number of buckets.
    bucket_count: usize,

    /// How much a 64-bit hash needs to be shifted to the right to produce a bucket ID.
    bucket_shift: u32,
}

#[cfg(feature = "build")]
impl Buckets {
    /// Expected bucket size. We follow CHD here.
    const LAMBDA: usize = 5;

    /// Split keys into buckets under the given `hash_space` parameter.
    ///
    /// If a `(Approx, Bucket)` collision is found, returns `None`. Otherwise, returns `Some`.
    ///
    /// Consumes `keys`.
    ///
    /// # Panics
    ///
    /// Panics if `keys` is empty, if `hash_space < keys.len()`, or if `hash_space` is close to
    /// `usize::MAX`.
    fn try_from_keys(
        keys: impl ExactSizeIterator<Item = u64> + Clone,
        hash_space: usize,
    ) -> Option<Self> {
        assert!(keys.len() > 0, "Cannot create buckets from empty keys");
        assert!(hash_space >= keys.len(), "Hash space too small");
        assert!(
            hash_space.checked_add(u16::MAX as usize).is_some(),
            "hash_space too large"
        );

        // At least two buckets are required so that bucket_shift < 64
        let bucket_count = keys.len().div_ceil(Self::LAMBDA).next_power_of_two().max(2);
        let bucket_shift = 64 - bucket_count.ilog2();

        // Iterate over buckets
        let keys_len = keys.len();
        let mut approxs = Vec::with_capacity(keys.len());
        let mut by_size = Vec::new();
        group_by_key(
            keys,
            keys_len,
            &mut |key| key.wrapping_mul(hash_space as u64),
            bucket_count.ilog2(),
            bucket_shift,
            &mut |keys_for_bucket| {
                let left = approxs.len();
                let mut bucket = 0;
                for key in keys_for_bucket {
                    let approx;
                    (approx, bucket) = to_approx_bucket(key, hash_space, bucket_shift);
                    approxs.push(approx);
                }
                let approx_for_bucket = &mut approxs[left..];

                // Ensure that Approx values don't collide inside the bucket. The bucket size is
                // expected to be very small, so quadratic approach is faster than sorting.
                if approx_for_bucket
                    .iter()
                    .enumerate()
                    .any(|(i, a)| approx_for_bucket[..i].contains(a))
                {
                    return Err(());
                }

                // Add bucket to its per-size list
                let size = approx_for_bucket.len();
                while by_size.len() <= size {
                    by_size.push(Vec::new());
                }
                by_size[size].push((bucket, left));
                Ok(())
            },
        )
        .ok()?;

        Some(Self {
            approxs,
            by_size,
            hash_space,
            bucket_count,
            bucket_shift,
        })
    }

    /// Iterate over buckets in decreasing size order.
    ///
    /// Yields `(Bucket, [Approx])`.
    fn iter(&self) -> impl Iterator<Item = (usize, &[u64])> {
        self.by_size
            .iter()
            .enumerate()
            .rev()
            .flat_map(move |(size, buckets)| {
                buckets
                    .iter()
                    .map(move |&(bucket, start)| (bucket, &self.approxs[start..start + size]))
            })
    }

    /// Attempt to generate a PHF with the given mixer. May fail.
    ///
    /// Hash space must be at least somewhat larger than the number of keys.
    fn try_generate_phf(&self, mixer: Mixer) -> Option<UntypedPhf> {
        // Reserve space for elements, plus 2^16 - 1 for out-of-bounds displacements
        let mut free = BitMap::new_ones(self.hash_space + u16::MAX as usize);

        // We'll fill this per-bucket array during the course of the algorithm
        let mut displacements = vec![0; self.bucket_count];

        // Handle buckets
        for (bucket, approx_for_bucket) in self.iter() {
            // Find non-colliding displacement. On failure, return None.
            let displacement = unsafe { mixer.find_valid_displacement(approx_for_bucket, &free) }?;
            displacements[bucket] = displacement;

            for approx in approx_for_bucket {
                let index = mixer.mix(*approx, displacement);
                // SAFETY: `index < hash_space_with_oob <= hash_space + (2^16 - 1)`
                unsafe {
                    free.reset_unchecked(index);
                }
            }
        }

        let hash_space_with_oob = mixer.get_hash_space_with_oob(self.hash_space, &displacements);

        Some(UntypedPhf {
            inner: UntypedPhfInner {
                hash_space: self.hash_space,
                hash_space_with_oob,
                bucket_shift: self.bucket_shift,
                displacements: ConstVec::from_vec(displacements),
                mixer,
            },
        })
    }
}

/// Convert a 64-bit hash to `(Approx, Bucket)`.
const fn to_approx_bucket(hash: u64, hash_space: usize, bucket_shift: u32) -> (u64, usize) {
    let product = hash as u128 * hash_space as u128;
    #[expect(clippy::cast_possible_truncation, reason = "intentional")]
    let (high, low) = ((product >> 64i32) as u64, product as u64);
    #[expect(
        clippy::cast_possible_truncation,
        reason = "Bucket is guaranteed to fill in usize"
    )]
    (high, (low >> bucket_shift) as usize)
}

/// Algorithm for mixing displacement with the approximate position
#[derive(Copy, Clone, Debug)]
#[cfg_attr(
    feature = "serde",
    expect(clippy::unsafe_derive_deserialize, reason = "plain old data")
)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "build", derive(ConstDispatch))]
#[non_exhaustive]
#[doc(hidden)]
pub enum Mixer {
    Add,
    Xor,
}

impl Mixer {
    /// Mix `Approx` with a displacement to get the final hash.
    const fn mix(self, approx: u64, displacement: u16) -> usize {
        #[expect(
            clippy::cast_possible_truncation,
            reason = "approx + 65535 < hash_space + 65535 <= usize::MAX"
        )]
        match self {
            Self::Add => approx as usize + displacement as usize,
            Self::Xor => approx as usize ^ displacement as usize,
        }
    }

    /// Find a valid displacement.
    ///
    /// Returns `displacement` such that `mix(approx, displacement)` is marked as free in the
    /// bitmap for each `approx` in the list. On failure, returns `None`.
    ///
    /// # Safety
    ///
    /// `free` must contain at least `approx + 65535` bits.
    #[cfg(feature = "build")]
    unsafe fn find_valid_displacement(
        self,
        approx_for_bucket: &[u64],
        free: &BitMap,
    ) -> Option<u16> {
        const_dispatch!(self, |const MIXER: Mixer| {
            let displacement_bases = match MIXER {
                Self::Add => {
                    // We don't iterate through a few of the top 65536 displacements to avoid going
                    // out of bounds, but that's noise
                    (0..=u16::MAX - 57).step_by(57)
                }
                Self::Xor => (0..=u16::MAX).step_by(8),
            };

            for displacement_base in displacement_bases {
                let mut global_free_mask = u64::MAX;

                // Iterate over keys
                for approx in approx_for_bucket {
                    let start = MIXER.mix(*approx, displacement_base);
                    let p = free.as_byte_ptr().wrapping_add(start / 8);

                    // `local_free_mask[i]` indicates whether `mix(approx, displacement_base + i)`
                    // is free. The bottom 57/8 bits need to be exact; the top bits may contain
                    // false negatives.
                    let local_free_mask = match MIXER {
                        Self::Add => (unsafe { p.cast::<u64>().read_unaligned() }) >> (start % 8),
                        Self::Xor => {
                            BIT_INDEX_XOR_LUT[start % 8][unsafe { p.read() } as usize].into()
                        }
                    };

                    global_free_mask &= local_free_mask;
                }

                // If `approx_for_bucket` is empty, this immediately returns `Some(0)`.
                if global_free_mask != 0 {
                    #[expect(clippy::cast_possible_truncation, reason = "false positive")]
                    let displacement_offset = global_free_mask.trailing_zeros() as u16;
                    return Some(displacement_base + displacement_offset);
                }
            }

            None
        })
    }

    /// Compute the range of indices that can be accessed by absent keys.
    ///
    /// Given the range of Approx values (`[0; hash_space)`) and the table of displacements,
    /// computes the limit `hash_space_with_oob` such that for any key, including a key not from the
    /// training set, the computed `hash` is below the limit.
    #[cfg(any(feature = "build", all(feature = "alloc", feature = "serde")))]
    fn get_hash_space_with_oob(self, hash_space: usize, displacements: &[u16]) -> usize {
        let max_displacement = *displacements.iter().max().unwrap_or(&0) as usize;
        match self {
            Self::Add => hash_space + max_displacement,
            Self::Xor => {
                let x = hash_space - 1;
                let y = max_displacement;
                let bound = (x & y).checked_ilog2().unwrap_or(0);
                let max_xor = x | y | ((1 << bound) - 1);
                max_xor + 1
            }
        }
    }
}

/// Bit permutation LUT.
///
/// `lut[control][byte]` is computed by taking the individual bits of `byte` and moving each bit
/// from position `i` to `i ^ control`. For example, `lut[0][byte]` is just `byte`, while
/// `lut[7][byte]` reverses the bits in `byte`.
///
/// This is used to optimize the search for valid displacements for the xor mixer.
#[cfg(feature = "build")]
const BIT_INDEX_XOR_LUT: [[u8; 256]; 8] = {
    let mut lut = [[0; 256]; 8];
    let mut xor = 0;
    // For loops are unsupported in const, smh my head
    while xor < 8 {
        let mut bit_mask = 0;
        while bit_mask < 256 {
            let mut bit_index = 0;
            #[expect(clippy::cast_possible_truncation, reason = "bit_mask < 256")]
            while bit_index < 8 {
                lut[xor][bit_mask] |= ((bit_mask as u8 >> bit_index) & 1) << (bit_index ^ xor);
                bit_index += 1;
            }
            bit_mask += 1;
        }
        xor += 1;
    }
    lut
};

#[cfg(feature = "codegen")]
impl super::codegen::Codegen for UntypedPhf {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let untyped_phf = gen.path("h::low_level::UntypedPhf");
        let hash_space = gen.piece(&self.inner.hash_space);
        let hash_space_with_oob = gen.piece(&self.inner.hash_space_with_oob);
        let bucket_shift = gen.piece(&self.inner.bucket_shift);
        let displacements = gen.piece(&self.inner.displacements);
        let mixer = gen.piece(&self.inner.mixer);
        quote::quote!(
            #untyped_phf::__from_raw_parts(
                #hash_space,
                #hash_space_with_oob,
                #bucket_shift,
                #displacements,
                #mixer,
            )
        )
    }
}

#[cfg(feature = "codegen")]
impl super::codegen::Codegen for Mixer {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let mixer = gen.path("h::low_level::Mixer");
        match self {
            Mixer::Add => quote::quote!(#mixer::Add),
            Mixer::Xor => quote::quote!(#mixer::Xor),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_approx_bucket() {
        assert_eq!(to_approx_bucket(0x1289f3da73209aad, 12345, 53), (893, 2035));
    }

    #[test]
    fn mix() {
        assert_eq!(Mixer::Add.mix(0x123, 0x456), 0x579);
        assert_eq!(Mixer::Xor.mix(0x123, 0x456), 0x575);
    }

    #[test]
    fn get_hash_space_with_oob() {
        assert_eq!(Mixer::Add.get_hash_space_with_oob(0x123, &[0x456]), 0x579);

        for hash_space in 1..0x10 {
            for displacement1 in 0..0x10 {
                for displacement2 in 0..0x10 {
                    let answer = Mixer::Xor
                        .get_hash_space_with_oob(hash_space, &[displacement1, displacement2]);
                    let expected = (0..hash_space)
                        .map(|approx| {
                            (approx ^ displacement1 as usize).max(approx ^ displacement2 as usize)
                        })
                        .max()
                        .unwrap()
                        + 1;
                    assert_eq!(answer, expected);
                }
            }
        }
    }

    #[test]
    fn find_valid_displacement() {
        for mixer in [Mixer::Add, Mixer::Xor] {
            for approx in 0..8 {
                let mut free = BitMap::new_ones(approx as usize + 65535);
                for n in 0..512 {
                    // SAFETY: `free` contains exactly as many bits as required
                    assert_eq!(
                        unsafe { mixer.find_valid_displacement(&[approx], &free) },
                        Some(n),
                    );
                    // SAFETY: `approx + n < free.len()`
                    unsafe {
                        free.reset_unchecked(mixer.mix(approx, n));
                    }
                }
            }
        }
    }

    #[test]
    fn mixer_no_oob() {
        for mixer in [Mixer::Add, Mixer::Xor] {
            let mut free = BitMap::new_ones(65535);
            // SAFETY: `free` contains exactly as many bits as required
            while let Some(n) = unsafe { mixer.find_valid_displacement(&[0], &free) } {
                // SAFETY: `n < free.len()`
                unsafe {
                    free.reset_unchecked(n as usize);
                }
            }
        }
    }
}
