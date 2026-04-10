use super::{
    Phf,
    phf::{get_capacity, to_approx_bucket},
    state::{Displacements, State},
};
use alloc::{vec, vec::Vec};

mod group_by;
use group_by::group_by_key;

mod bitmap;
use bitmap::BitMap;

impl Phf {
    /// Generate a perfect hash function.
    ///
    /// `seeds` should be an infinite set of seeds -- PHF generation seldom succeeds with just one
    /// seed, so it iterates through many. The hasher should be of high quality and shouldn't have
    /// seed-independent collisions.
    ///
    /// `keys` must not contain duplicates, otherwise generation will hang.
    ///
    /// # Panics
    ///
    /// Panics if `keys` contains more than `usize::MAX / 2` elements, or if `seeds` is finite and
    /// runs out.
    #[must_use]
    pub fn build<T, Seed>(
        keys: &[T],
        mut seeds: impl Iterator<Item = Seed>,
        hasher: impl Fn(&T, &Seed) -> u64,
    ) -> (Seed, Phf) {
        if keys.is_empty() {
            return (seeds.next().expect("no seeds"), Self::new());
        }

        // Asserting this is enough to guarantee that `approx_range` is far from `usize::MAX`.
        assert!(keys.len() <= usize::MAX / 2, "Too many keys");

        // Use different load factors for different sizes. This was tuned experimentally. We used to
        // increase `approx_range` after each failure to improve the chances of success, but that
        // doesn't seem necessary.
        let percent = keys.len().div_ceil(100);
        let coeff = keys.len().div_ceil(1_000_000).min(5);
        let approx_range = keys.len() + coeff * percent;

        for seed in seeds {
            if let Some(buckets) =
                Buckets::try_from_keys(keys.iter().map(|key| hasher(key, &seed)), approx_range)
            {
                if let Some(phf) = buckets.try_generate_phf() {
                    return (seed, phf);
                }
            }
        }

        panic!("ran out of imperfect hash family instances")
    }
}

/// Keys, split into buckets.
struct Buckets {
    /// Approx values of keys, ordered such that all buckets are consecutive
    approxs: Vec<u64>,

    /// Buckets, grouped by size. The tuple is `(Bucket, left)`.
    by_size: Vec<Vec<(usize, usize)>>,

    /// The `approx_range` parameter this object is valid under
    approx_range: usize,

    /// The number of buckets.
    bucket_count: usize,

    /// How much a 64-bit hash needs to be shifted to the right to produce a bucket ID.
    bucket_shift: u32,
}

impl Buckets {
    /// Expected bucket size. We follow CHD here.
    const LAMBDA: usize = 5;

    /// Split keys into buckets under the given `approx_range` parameter.
    ///
    /// If a `(Approx, Bucket)` collision is found, returns `None`. Otherwise, returns `Some`.
    ///
    /// Consumes `keys`.
    ///
    /// # Panics
    ///
    /// Panics if `keys` is empty, if `approx_range < keys.len()`, or if `approx_range` is close to
    /// `usize::MAX`.
    fn try_from_keys(
        keys: impl ExactSizeIterator<Item = u64> + Clone,
        approx_range: usize,
    ) -> Option<Self> {
        assert!(keys.len() > 0, "Cannot create buckets from empty keys");
        assert!(approx_range >= keys.len(), "Hash space too small");
        assert!(
            approx_range.checked_add(u16::MAX as usize).is_some(),
            "approx_range too large"
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
            &mut |key| key.wrapping_mul(approx_range as u64),
            bucket_count.ilog2(),
            bucket_shift,
            &mut |keys_for_bucket| {
                let left = approxs.len();
                let mut bucket = 0;
                for key in keys_for_bucket {
                    let approx;
                    (approx, bucket) = to_approx_bucket(key, approx_range, bucket_shift);
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
            approx_range,
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

    /// Attempt to generate a PHF. May fail.
    ///
    /// Hash space must be at least somewhat larger than the number of keys.
    fn try_generate_phf(&self) -> Option<Phf> {
        // Reserve space for elements, plus 2^16 - 1 for out-of-bounds displacements
        let mut free = BitMap::new_ones(self.approx_range + u16::MAX as usize);

        // We'll fill this per-bucket array during the course of the algorithm
        let mut displacements = vec![0; self.bucket_count];

        // Handle buckets
        for (bucket, approx_for_bucket) in self.iter() {
            // Find non-colliding displacement. On failure, return None.
            let displacement = unsafe { find_valid_displacement(approx_for_bucket, &free) }?;
            displacements[bucket] = displacement;

            for approx in approx_for_bucket {
                let index = *approx as usize + displacement as usize;
                // SAFETY: `index < capacity <= approx_range + (2^16 - 1)`
                unsafe {
                    free.reset_unchecked(index);
                }
            }
        }

        let capacity = get_capacity(self.approx_range, &displacements);

        Some(Phf {
            state: State {
                approx_range: self.approx_range,
                bucket_shift: self.bucket_shift,
                displacements: Displacements::Owned(displacements),
                capacity,
            },
        })
    }
}

/// Find a valid displacement.
///
/// Returns `displacement` such that `approx + displacement` is marked as free in the bitmap for
/// each `approx` in the list. On failure, returns `None`.
///
/// # Safety
///
/// `free` must contain at least `approx + 65535` bits.
unsafe fn find_valid_displacement(approx_for_bucket: &[u64], free: &BitMap) -> Option<u16> {
    // We don't iterate through a few of the top 65536 displacements to avoid going out of
    // bounds, but that's noise
    for displacement_base in (0..=u16::MAX - 57).step_by(57) {
        let mut global_free_mask = u64::MAX;

        // Iterate over keys
        for approx in approx_for_bucket {
            let start = *approx as usize + displacement_base as usize;
            let p = free.as_byte_ptr().wrapping_add(start / 8);
            // Bit `i` in the mask indicates whether `approx + displacement_base + i` is free. The
            // bottom 57 bits are assumed to be exact; the top bits may contain false negatives.
            global_free_mask &= (unsafe { p.cast::<u64>().read_unaligned() }) >> (start % 8);
        }

        // If `approx_for_bucket` is empty, this immediately returns `Some(0)`.
        if global_free_mask != 0 {
            return Some(displacement_base + global_free_mask.trailing_zeros() as u16);
        }
    }

    None
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn test_find_valid_displacement() {
        for approx in 0..8 {
            let mut free = BitMap::new_ones(approx as usize + 65535);
            for n in 0..512 {
                // SAFETY: `free` contains exactly as many bits as required
                assert_eq!(
                    unsafe { find_valid_displacement(&[approx], &free) },
                    Some(n),
                );
                // SAFETY: `approx + n < free.len()`
                unsafe {
                    free.reset_unchecked(approx as usize + n as usize);
                }
            }
        }
    }

    #[test]
    fn mixer_no_oob() {
        let mut free = BitMap::new_ones(65535);
        let mut i = 0;
        // SAFETY: `free` contains exactly as many bits as required
        while let Some(n) = unsafe { find_valid_displacement(&[0], &free) } {
            assert_eq!(n, i);
            i += 1;
            // SAFETY: `n < free.len()`
            unsafe {
                free.reset_unchecked(n as usize);
            }
        }
    }
}
