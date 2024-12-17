//! Utilities.

#![cfg(feature = "build")]

use alloc::{vec, vec::Vec};
use core::mem::MaybeUninit;
use fixed_slice_vec::FixedSliceVec;

/// Move elements from `x = source[i]` to `destination[permutation(x)]`.
pub fn scatter<T>(
    source: Vec<T>,
    mut permutation: impl FnMut(&T) -> usize,
    destination: &mut [Option<T>],
) {
    // Ideally, this would be implemented with a cache-aware approach or with non-temporal stores.
    // Unfortunately, I haven't managed to produce an algorithm with better performance than this
    // simple loop.
    for element in source {
        let index = permutation(&element);
        destination[index] = Some(element);
    }
}

/// A faster alternative to [`core::iter::Chain`] for simple iterators.
#[derive(Clone)]
struct StupidChain<I>(I, I);

impl<I: Iterator> Iterator for StupidChain<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().or_else(|| self.1.next())
    }
}

/// A radix sort bucket.
///
/// The bucket is split into two parts: `reserved`, which is a fixed-capacity vector that won't ever
/// be reallocated, and `overflow`, which is used if there isn't enough space in `reserved`.
///
/// This way, only a small percentage of elements are stored in vectors, making reallocations cheap.
/// In addition, the total size of `overflow` is small enough that accesses to `overflow` can be
/// resolved from cache.
struct Bucket<'buffer, T> {
    /// Reserved space.
    reserved: FixedSliceVec<'buffer, T>,
    /// Elements that didn't fit in the reserved space.
    overflow: Vec<T>,
}

/// Split elements into groups by key.
///
/// Groups elements from the iterator based on the key and invokes `callback` for each group. Unlike
/// [`[T]::chunk_by`], non-consecutive elements are merged, too. The returned groups are sorted by
/// the key.
///
/// The key is computed by taking the bottom `key_bitness` bits of `key(element) >> base_shift`.
///
/// `elements_len` should be equal to the length of `elements`, although this is not a safety
/// requirement.
///
/// # Errors
///
/// If the callback returns [`Err`], iteration stops immediately and the error is returned.
///
/// # Panics
///
/// Might panic if `key_bitness + base_shift > 64`.
#[expect(clippy::arithmetic_side_effects, reason = "false positives")]
pub fn group_by_key<T: Copy, E>(
    elements: impl Iterator<Item = T>,
    elements_len: usize,
    key_bitness: u32,
    base_shift: u32,
    key: &mut impl FnMut(T) -> u64,
    callback: &mut impl FnMut(&mut dyn Iterator<Item = T>) -> Result<(), E>,
) -> Result<(), E> {
    /// The step at which `key` is consumed. `2 ** BITS` buckets are allocated.
    const BITS: u32 = 8;

    let n_buckets = 1 << key_bitness.min(BITS);
    let reserved_capacity = elements_len / n_buckets;

    // Partitioning a single allocation is more efficient than allocating multiple times
    let mut buffer = vec![MaybeUninit::uninit(); elements_len];
    let mut reserved = buffer.chunks_mut(reserved_capacity);
    let mut buckets: [Bucket<T>; 1 << BITS] = core::array::from_fn(|_| Bucket {
        reserved: FixedSliceVec::new(reserved.next().unwrap_or(&mut [])),
        overflow: Vec::new(),
    });

    let shift = key_bitness.saturating_sub(BITS) + base_shift;
    for element in elements {
        #[expect(
            clippy::cast_possible_truncation,
            reason = "we mask out the higher bits anyway"
        )]
        let bucket = &mut buckets[(key(element) >> shift) as usize & (n_buckets - 1)];
        if bucket.reserved.try_push(element).is_err() {
            bucket.overflow.push(element);
        }
    }

    for bucket in buckets {
        let bucket_len = bucket.reserved.len() + bucket.overflow.len();
        if bucket_len == 0 {
            continue;
        }
        let mut bucket_elements =
            StupidChain(bucket.reserved.iter(), bucket.overflow.iter()).copied();

        if key_bitness <= BITS {
            callback(&mut bucket_elements)?;
        } else {
            group_by_key(
                bucket_elements,
                bucket_len,
                key_bitness - BITS,
                base_shift,
                key,
                callback,
            )?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    #[test]
    fn scatter_inverse() {
        let source = vec![5, 0, 4, 2, 3, 1, 6];
        let mut destination = vec![None; 7];
        scatter(source, |&element| element, &mut destination);
        assert_eq!(destination, (0..=6).map(Some).collect::<Vec<_>>());
    }
}
