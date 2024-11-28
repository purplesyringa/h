#![allow(clippy::arithmetic_side_effects, reason = "many false positives")]

use super::const_vec::ConstVec;

#[cfg(feature = "build")]
use alloc::{vec, vec::Vec};

// We assume that usize is at most 64-bit in many places.
const _: () = assert!(
    size_of::<usize>() <= size_of::<u64>(),
    "I want your computer."
);

/// A perfect hash function.
///
/// This PHF does not hash keys for you.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Phf {
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

impl Phf {
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn from_raw_parts(
        hash_space: usize,
        hash_space_with_oob: usize,
        bucket_shift: u32,
        displacements: &'static [u16],
        mixer: Mixer,
    ) -> Self {
        Self {
            hash_space,
            hash_space_with_oob,
            bucket_shift,
            displacements: ConstVec::from_static_ref(displacements),
            mixer,
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
    #[allow(
        clippy::missing_inline_in_public_items,
        reason = "very heavy, we'd rather not copy it to every crate"
    )]
    #[must_use]
    pub fn try_from_keys(keys: Vec<u64>, hash_space: usize) -> Option<Self> {
        if keys.is_empty() {
            return Some(Self {
                hash_space: 0,
                hash_space_with_oob: 1,
                bucket_shift: 31,
                displacements: ConstVec::from(&[0, 0][..]),
                mixer: Mixer::Add,
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
        let product = multiply_scale(key, self.hash_space);
        let approx = (product >> 64i32) as u64;
        #[allow(clippy::cast_possible_truncation, reason = "intentional")]
        let bucket = (product as u64) >> self.bucket_shift;
        #[allow(
            clippy::cast_possible_truncation,
            reason = "bucket < displacements.len() <= usize::MAX"
        )]
        let displacement = unsafe { *self.displacements.get_unchecked(bucket as usize) };
        self.mixer.mix(approx, displacement)
    }

    /// Get the boundary on indices.
    ///
    /// The index returned by `hash` is guaranteed to be less than `capacity()`, even for keys
    /// outside the training dataset.
    #[inline]
    #[must_use]
    pub const fn capacity(&self) -> usize {
        self.hash_space_with_oob
    }
}

/// Keys, split into buckets.
#[cfg(feature = "build")]
struct Buckets {
    /// Approx values of keys, ordered such that all buckets are consecutive
    approxs: Vec<u64>,

    /// Buckets, grouped by size. The tuple is `(Bucket, size)`.
    by_size: Vec<Vec<(u64, usize)>>,

    /// The `hash_space` parameter this object is valid under
    hash_space: usize,

    bucket_count: usize,
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
    fn try_from_keys(mut keys: Vec<u64>, hash_space: usize) -> Option<Self> {
        assert!(!keys.is_empty(), "Cannot create buckets from empty keys");
        assert!(hash_space >= keys.len(), "Hash space too small");
        assert!(
            hash_space.checked_add(u16::MAX as usize).is_some(),
            "hash_space too large"
        );

        // At least two buckets are required so that bucket_shift < 64
        let bucket_count = keys.len().div_ceil(Self::LAMBDA).next_power_of_two().max(2);
        let bucket_shift = 64 - bucket_count.ilog2();

        // Sort keys by bucket using a cache-friendly algorithm
        let key_to_bucket = |key: u64| key.wrapping_mul(hash_space as u64) >> bucket_shift;
        // Reduce the number of iterations
        if bucket_count <= 1 << 16i32 {
            #[allow(
                clippy::cast_possible_truncation,
                reason = "bucket < bucket_size <= 2^16"
            )]
            radsort::sort_by_key(&mut keys, |key| key_to_bucket(*key) as u16);
        } else if bucket_count <= 1 << 32i32 {
            #[allow(
                clippy::cast_possible_truncation,
                reason = "bucket < bucket_size <= 2^32"
            )]
            radsort::sort_by_key(&mut keys, |key| key_to_bucket(*key) as u32);
        } else {
            radsort::sort_by_key(&mut keys, |key| key_to_bucket(*key));
        }

        // We'll store per-size bucket lists here
        let mut by_size: Vec<Vec<(u64, usize)>> = Vec::new();

        // A manual group_by implementation, just two pointers. `product` always stores the product
        // for the currently processed element.
        let mut product = multiply_scale(keys[0], hash_space);
        let mut left = 0;
        while left < keys.len() {
            #[allow(clippy::cast_possible_truncation, reason = "intentional")]
            let bucket = product as u64 >> bucket_shift;

            // Replace the key with its Approx value in-place for future use. We have already
            // computed the product, so this is cheap.
            keys[left] = (product >> 64i32) as u64;

            // Keep going while the key has the same bucket as the previous one
            let mut right = left + 1;
            #[allow(clippy::cast_possible_truncation, reason = "intentional")]
            while right < keys.len() && {
                product = multiply_scale(keys[right], hash_space);
                bucket == product as u64 >> bucket_shift
            } {
                keys[right] = (product >> 64i32) as u64;
                right += 1;
            }

            let approx_for_bucket = &mut keys[left..right];

            // Ensure that Approx values don't collide inside the bucket
            approx_for_bucket.sort_unstable();
            if approx_for_bucket
                .windows(2)
                .any(|window| window[0] == window[1])
            {
                return None;
            }

            // Add bucket to its per-size list
            let size = approx_for_bucket.len();
            while by_size.len() <= size {
                by_size.push(Vec::new());
            }
            by_size[size].push((bucket, left));
            left = right;
        }

        Some(Self {
            approxs: keys,
            by_size,
            hash_space,
            bucket_count,
            bucket_shift,
        })
    }

    /// Iterate over buckets in decreasing size order.
    ///
    /// Yields `(Bucket, [Approx])`.
    fn iter(&self) -> impl Iterator<Item = (u64, &[u64])> {
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
    fn try_generate_phf(&self, mixer: Mixer) -> Option<Phf> {
        // Reserve space for elements, plus 2^16 - 1 for out-of-bounds displacements
        let alloc = self.hash_space + u16::MAX as usize;
        let mut free = vec![u8::MAX; alloc.div_ceil(8)];

        // We'll fill this per-bucket array during the course of the algorithm
        let mut displacements = vec![0; self.bucket_count];

        // Handle buckets
        for (bucket, approx_for_bucket) in self.iter() {
            // Find non-colliding displacement. On failure, return None.
            let displacement =
                unsafe { mixer.find_valid_displacement(approx_for_bucket, free.as_ptr()) }?;

            #[allow(
                clippy::cast_possible_truncation,
                reason = "bucket < displacements.len() <= usize::MAX"
            )]
            {
                displacements[bucket as usize] = displacement;
            }

            for approx in approx_for_bucket {
                let index = mixer.mix(*approx, displacement);
                *unsafe { free.get_unchecked_mut(index / 8) } &= !(1 << (index % 8));
            }
        }

        let hash_space_with_oob = mixer.get_hash_space_with_oob(self.hash_space, &displacements);

        Some(Phf {
            hash_space: self.hash_space,
            hash_space_with_oob,
            bucket_shift: self.bucket_shift,
            displacements: displacements.into(),
            mixer,
        })
    }
}

const fn multiply_scale(a: u64, b: usize) -> u128 {
    a as u128 * b as u128
}

/// Algorithm for mixing displacement with the approximate position
#[derive(Copy, Clone, Debug)]
#[non_exhaustive]
#[doc(hidden)]
pub enum Mixer {
    Add,
    Xor,
}

impl Mixer {
    const fn mix(self, approx: u64, displacement: u16) -> usize {
        #[allow(
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
    /// `free` as a bitmap must be large enough to fit `approx + 65535`.
    #[cfg(feature = "build")]
    unsafe fn find_valid_displacement(
        self,
        approx_for_bucket: &[u64],
        free: *const u8,
    ) -> Option<u16> {
        match self {
            Self::Add => {
                // SAFETY: Safety requirements forwarded.
                unsafe { Self::find_valid_displacement_add(approx_for_bucket, free) }
            }
            Self::Xor => {
                // SAFETY: Safety requirements forwarded.
                unsafe { Self::find_valid_displacement_xor(approx_for_bucket, free) }
            }
        }
    }

    /// Find a valid displacement for the [`Self::Add`] mixer.
    ///
    /// Returns `displacement` such that `mix(approx, displacement)` is marked as free in the
    /// bitmap for each `approx` in the list. On failure, returns `None`.
    ///
    /// # Safety
    ///
    /// `free` as a bitmap must be large enough to fit `approx + 65535`.
    #[cfg(feature = "build")]
    unsafe fn find_valid_displacement_add(
        approx_for_bucket: &[u64],
        free: *const u8,
    ) -> Option<u16> {
        // Outer unrolled loop
        for displacement_base_index in 0..u16::MAX / 57 {
            // We don't iterate through a few of the top 65536 displacements, but that's noise
            let displacement_base = displacement_base_index * 57;

            // Can't trust bits farther than the first 57, because we shift out up to 7 bits,
            // shifting in meaningless zeros
            let mut global_bit_mask = (1 << 57i32) - 1;

            // Iterate over keys
            for approx in approx_for_bucket {
                // Inner unrolled loop, aka bitmask logic
                #[allow(
                    clippy::cast_possible_truncation,
                    reason = "approx + 65535 < hash_space + 65535 <= usize::MAX"
                )]
                let start = *approx as usize + displacement_base as usize;
                let bit_mask =
                    unsafe { free.wrapping_add(start / 8).cast::<u64>().read_unaligned() }
                        >> (start % 8);
                global_bit_mask &= bit_mask;
            }

            if global_bit_mask != 0 {
                #[allow(clippy::cast_possible_truncation, reason = "false positive")]
                let displacement_offset = global_bit_mask.trailing_zeros() as u16;
                return Some(displacement_base + displacement_offset);
            }
        }

        None
    }

    /// Find a valid displacement for the [`Self::Xor`] mixer.
    ///
    /// Returns `displacement` such that `mix(approx, displacement)` is marked as free in the
    /// bitmap for each `approx` in the list. On failure, returns `None`.
    ///
    /// # Safety
    ///
    /// `free` as a bitmap must be large enough to fit `approx + 65535`.
    #[cfg(feature = "build")]
    unsafe fn find_valid_displacement_xor(
        approx_for_bucket: &[u64],
        free: *const u8,
    ) -> Option<u16> {
        // Outer unrolled loop
        for displacement_base in (0..=u16::MAX).step_by(8) {
            let mut global_bit_mask = u8::MAX;

            // Iterate over keys
            for approx in approx_for_bucket {
                #[allow(
                    clippy::cast_possible_truncation,
                    reason = "approx + 65535 < hash_space + 65535 <= usize::MAX"
                )]
                let approx = *approx as usize;
                // Inner unrolled loop, aka bitmask logic
                let bit_mask = unsafe {
                    free.wrapping_add((approx ^ displacement_base as usize) / 8)
                        .read()
                };
                global_bit_mask &= BIT_INDEX_XOR_LUT[approx % 8][bit_mask as usize];
            }

            // Find the first applicable displacement (i.e. such that all `free` values are 1)
            if global_bit_mask != 0 {
                #[allow(clippy::cast_possible_truncation, reason = "false positive")]
                let displacement_offset = global_bit_mask.trailing_zeros() as u16;
                return Some(displacement_base + displacement_offset);
            }
        }

        None
    }

    #[cfg(feature = "build")]
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

#[cfg(feature = "build")]
const BIT_INDEX_XOR_LUT: [[u8; 256]; 8] = {
    let mut lut = [[0; 256]; 8];
    let mut xor = 0;
    // For loops are unsupported in const, smh my head
    while xor < 8 {
        let mut bit_mask = 0;
        while bit_mask < 256 {
            let mut bit_index = 0;
            #[allow(clippy::cast_possible_truncation, reason = "bit_mask < 256")]
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
impl super::codegen::Codegen for Phf {
    #[inline]
    fn generate_piece(&self, gen: &mut super::codegen::CodeGenerator) -> proc_macro2::TokenStream {
        let unhashed_phf = gen.path("h::low_level::UnhashedPhf");
        let hash_space = gen.piece(&self.hash_space);
        let hash_space_with_oob = gen.piece(&self.hash_space_with_oob);
        let bucket_shift = gen.piece(&self.bucket_shift);
        let displacements = gen.piece(&&*self.displacements);
        let mixer = gen.piece(&self.mixer);
        quote::quote!(
            #unhashed_phf::from_raw_parts(
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
