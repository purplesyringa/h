use super::codegen::Codegen;
use alloc::borrow::Cow;
use alloc::{vec, vec::Vec};
use core::fmt;

/// A perfect hash function.
///
/// This PHF does not hash keys for you.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Phf {
    /// Size of the hash table, without taking out-of-bounds displacements into account
    #[doc(hidden)]
    pub hash_space: usize,

    /// Size of the hash table, taking out-of-bounds displacements into account
    #[doc(hidden)]
    pub hash_space_with_oob: usize,

    /// `64 - ilog2(bucket_count)`
    #[doc(hidden)]
    pub bucket_shift: u32,

    /// Per-bucket displacement values
    #[doc(hidden)]
    pub displacements: Cow<'static, [u16]>,

    /// How the displacement is mixed into the approximate position
    #[doc(hidden)]
    pub mixer: Mixer,
}

impl Phf {
    /// Generate a perfect hash function.
    ///
    /// `hash_space` must be at least somewhat higher than `keys.len()`. `keys` are expected to
    /// already be hashed; may easily fail for bad hashes or just if it feels like it. Try to retry
    /// with other parameters in this case.
    ///
    /// If there are duplicates in `keys`, returns `None`.
    #[allow(
        clippy::missing_inline_in_public_items,
        reason = "very heavy, we'd rather not copy it to every crate"
    )]
    pub fn try_new(keys: Vec<u64>, hash_space: usize) -> Option<Self> {
        if keys.is_empty() {
            return Some(Self {
                hash_space: 0,
                hash_space_with_oob: 1,
                bucket_shift: 31,
                displacements: Cow::Borrowed(&[0, 0]),
                mixer: Mixer::Add,
            });
        }

        let buckets = Buckets::try_new(keys, hash_space)?;
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
    pub fn hash(&self, key: u64) -> usize {
        let product = key as u128 * self.hash_space as u128;
        let approx = (product >> 64i32) as u64;
        let bucket = (product as u64) >> self.bucket_shift;
        let displacement = unsafe { *self.displacements.get_unchecked(bucket as usize) };
        self.mixer.mix(approx, displacement)
    }

    /// Get the boundary on indices.
    ///
    /// The index returned by `hash` is guaranteed to be less than `capacity()`, even for keys
    /// outside the training dataset.
    #[inline]
    pub const fn capacity(&self) -> usize {
        self.hash_space_with_oob
    }
}

/// Keys, split into buckets.
struct Buckets {
    /// Approx values of keys, ordered such that all buckets are consecutive
    approxs: Vec<u64>,

    /// Buckets, groups by size. The tuple is `(Bucket, size)`.
    buckets_by_size: Vec<Vec<(u64, usize)>>,

    /// The `hash_space` parameter this object is valid under
    hash_space: usize,

    bucket_count: usize,
    bucket_shift: u32,
}

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
    /// Panics if `keys` is empty.
    fn try_new(mut keys: Vec<u64>, hash_space: usize) -> Option<Self> {
        assert!(!keys.is_empty(), "Cannot create buckets from empty keys");

        // At least two buckets are required so that bucket_shift < 64
        let bucket_count = keys.len().div_ceil(Self::LAMBDA).next_power_of_two().max(2);
        let bucket_shift = 64 - bucket_count.ilog2();

        // Sort keys by bucket using a cache-friendly algorithm
        let key_to_bucket = |key: u64| key.wrapping_mul(hash_space as u64) >> bucket_shift;
        // Reduce the number of iterations
        if bucket_count <= u16::MAX as usize {
            radsort::sort_by_key(&mut keys, |key| key_to_bucket(*key) as u16);
        } else if bucket_count <= u32::MAX as usize {
            radsort::sort_by_key(&mut keys, |key| key_to_bucket(*key) as u32);
        } else {
            radsort::sort_by_key(&mut keys, |key| key_to_bucket(*key));
        }

        // We'll store per-size bucket lists here
        let mut buckets_by_size: Vec<Vec<(u64, usize)>> = Vec::new();

        // A manual group_by implementation, just two pointers. `product` always stores the product
        // for the currently processed element.
        let mut product = keys[0] as u128 * hash_space as u128;
        let mut left = 0;
        while left < keys.len() {
            let bucket = product as u64 >> bucket_shift;

            // Replace the key with its Approx value in-place for future use. We have already
            // computed the product, so this is cheap.
            keys[left] = (product >> 64i32) as u64;

            // Keep going while the key has the same bucket as the previous one
            let mut right = left + 1;
            while right < keys.len() && {
                product = keys[right] as u128 * hash_space as u128;
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
            while buckets_by_size.len() <= size {
                buckets_by_size.push(Vec::new());
            }
            buckets_by_size[size].push((bucket, left));
            left = right;
        }

        Some(Self {
            approxs: keys,
            buckets_by_size,
            hash_space,
            bucket_count,
            bucket_shift,
        })
    }

    /// Iterate over buckets in decreasing size order.
    ///
    /// Yields `(Bucket, [Approx])`.
    fn iter(&self) -> impl Iterator<Item = (u64, &[u64])> {
        self.buckets_by_size
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
        let alloc = self.hash_space.checked_add(u16::MAX as usize).unwrap();
        let mut free = vec![u8::MAX; alloc.div_ceil(8)];

        // We'll fill this per-bucket array during the course of the algorithm
        let mut displacements = vec![0; self.bucket_count];

        // Handle buckets
        for (bucket, approx_for_bucket) in self.iter() {
            // Find non-colliding displacement. On failure, return None.
            let displacement =
                unsafe { mixer.find_valid_displacement(approx_for_bucket, free.as_ptr()) }?;

            displacements[bucket as usize] = displacement;
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
            displacements: Cow::Owned(displacements),
            mixer,
        })
    }
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
    fn mix(&self, approx: u64, displacement: u16) -> usize {
        match self {
            Self::Add => approx as usize + displacement as usize,
            Self::Xor => approx as usize ^ displacement as usize,
        }
    }

    // SAFETY: `free` as a bitmap must be large enough to fit `approx + 65535`.
    unsafe fn find_valid_displacement(
        &self,
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

    // SAFETY: As for `find_valid_displacement`.
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
                let start = *approx as usize + displacement_base as usize;
                let bit_mask =
                    unsafe { free.add(start / 8).cast::<u64>().read_unaligned() } >> (start % 8);
                global_bit_mask &= bit_mask;
            }

            if global_bit_mask != 0 {
                let displacement_offset = global_bit_mask.trailing_zeros() as u16;
                return Some(displacement_base + displacement_offset);
            }
        }

        None
    }

    // SAFETY: As for `find_valid_displacement`.
    unsafe fn find_valid_displacement_xor(
        approx_for_bucket: &[u64],
        free: *const u8,
    ) -> Option<u16> {
        // Outer unrolled loop
        for displacement_base in (0..=u16::MAX).step_by(8) {
            let mut global_bit_mask = u8::MAX;

            // Iterate over keys
            for approx in approx_for_bucket {
                let approx = *approx as usize;
                // Inner unrolled loop, aka bitmask logic
                let bit_mask =
                    unsafe { free.add((approx ^ displacement_base as usize) / 8).read() };
                global_bit_mask &= BIT_INDEX_XOR_LUT[approx % 8][bit_mask as usize];
            }

            // Find the first applicable displacement (i.e. such that all `free` values are 1)
            if global_bit_mask != 0 {
                let displacement_offset = global_bit_mask.trailing_zeros() as u16;
                return Some(displacement_base + displacement_offset);
            }
        }

        None
    }

    fn get_hash_space_with_oob(&self, hash_space: usize, displacements: &[u16]) -> usize {
        let max_displacement = *displacements.iter().max().unwrap_or(&0) as usize;
        match self {
            Self::Add => hash_space.checked_add(max_displacement).unwrap(),
            Self::Xor => {
                let x = hash_space.checked_sub(1).unwrap();
                let y = max_displacement;
                let bound = (x & y).checked_ilog2().unwrap_or(0);
                let max_xor = x | y | ((1 << bound) - 1);
                max_xor + 1
            }
        }
    }
}

const BIT_INDEX_XOR_LUT: [[u8; 256]; 8] = {
    let mut lut = [[0; 256]; 8];
    let mut xor = 0;
    // For loops are unsupported in const, smh my head
    while xor < 8 {
        let mut bit_mask = 0;
        while bit_mask < 256 {
            let mut bit_index = 0;
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

impl fmt::Display for Codegen<'_, Phf> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "::h::low_level::UnhashedPhf {{ hash_space: {}, hash_space_with_oob: {}, bucket_shift: {}, displacements: {}, mixer: {} }}",
            Codegen(&self.0.hash_space),
            Codegen(&self.0.hash_space_with_oob),
            Codegen(&self.0.bucket_shift),
            Codegen(&self.0.displacements),
            Codegen(&self.0.mixer),
        )
    }
}

impl fmt::Display for Codegen<'_, Mixer> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Mixer::Add => write!(f, "::h::low_level::Mixer::Add"),
            Mixer::Xor => write!(f, "::h::low_level::Mixer::Xor"),
        }
    }
}
