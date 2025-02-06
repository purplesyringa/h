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

impl<'buffer, T> Bucket<'buffer, T> {
    /// Create a bucket from a fixed-capacity reservation.
    const fn new(reserved: FixedSliceVec<'buffer, T>) -> Self {
        Self {
            reserved,
            overflow: Vec::new(),
        }
    }

    /// Add an element to the end of the bucket.
    ///
    /// # Panics
    ///
    /// Panics on OOM or if there are too many elements when `T` is a ZST.
    fn push(&mut self, element: T) {
        if let Err(element) = self.reserved.try_push(element) {
            self.overflow.push(element.0);
        }
    }

    /// Get the number of elements stored in the bucket.
    ///
    /// # Panics
    ///
    /// Panics if there are more than `usize::MAX` elements, which can occur when `T` is a ZST.
    fn len(&self) -> usize {
        self.reserved
            .len()
            .checked_add(self.overflow.len())
            .expect("more than `usize::MAX` elements in a bucket")
    }

    /// Iterate over the pushed elements.
    fn iter(&self) -> core::iter::Chain<core::slice::Iter<T>, core::slice::Iter<T>> {
        self.reserved.iter().chain(self.overflow.iter())
    }
}

/// Split elements into groups by key.
///
/// This is a variation of [`group_by_key`] that works efficiently for small arrays.
///
/// # Errors
///
/// If the callback returns [`Err`], iteration stops immediately and the error is returned.
///
/// # Panics
///
/// Might panic if `elements_len` does not match the number of elements in `elements`.
#[expect(
    clippy::cast_possible_truncation,
    reason = "we mask out higher bits anyway"
)]
#[expect(
    clippy::arithmetic_side_effects,
    reason = "the various additions here can only overflow if `elements_len` is wrong"
)]
fn group_by_key_fallback<T: Copy, E>(
    elements: impl Iterator<Item = T> + Clone,
    elements_len: usize,
    key: &mut impl FnMut(T) -> u64,
    key_bitness: u32,
    base_shift: u32,
    callback: &mut impl FnMut(&mut dyn Iterator<Item = T>) -> Result<(), E>,
) -> Result<(), E> {
    let n_groups = 1 << key_bitness;

    let mut counts: Vec<usize> = vec![0; n_groups];
    for element in elements.clone() {
        counts[(key(element) >> base_shift) as usize & (n_groups - 1)] += 1;
    }

    let mut group_ptrs: Vec<usize> = vec![0; n_groups];
    for i in 1..n_groups {
        group_ptrs[i] = group_ptrs[i - 1] + counts[i - 1];
    }

    let mut buffer = vec![MaybeUninit::uninit(); elements_len];
    for element in elements {
        let group_ptr = &mut group_ptrs[(key(element) >> base_shift) as usize & (n_groups - 1)];
        buffer[*group_ptr].write(element);
        *group_ptr += 1;
    }

    let mut end_ptr = 0;
    for i in 0..n_groups {
        let start_ptr = end_ptr;
        end_ptr += counts[i];
        if counts[i] > 0 {
            // This safety check ensures the whole group is initialized. This is valid even if
            // `group_ptr` overflows at some point.
            assert_eq!(
                end_ptr, group_ptrs[i],
                "`elements` iterator is non-deterministic",
            );
            let group = &buffer[start_ptr..end_ptr];
            // XXX: This is a stable implementation of `MaybeUninit::slice_assume_init_ref`. Worth
            // updating once the method is stabilized.
            let group = unsafe { &*(group as *const [MaybeUninit<T>] as *const [T]) };
            callback(&mut group.iter().copied())?;
        }
    }

    Ok(())
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
/// Might panic if `key_bitness + base_shift > 64` or if `elements_len` does not match the number of
/// elements in `elements`.
#[expect(clippy::arithmetic_side_effects, reason = "false positives")]
pub fn group_by_key<T: Copy, E>(
    elements: impl Iterator<Item = T> + Clone,
    elements_len: usize,
    key: &mut impl FnMut(T) -> u64,
    key_bitness: u32,
    base_shift: u32,
    callback: &mut impl FnMut(&mut dyn Iterator<Item = T>) -> Result<(), E>,
) -> Result<(), E> {
    /// The step at which `key` is consumed. `2 ** BITS` buckets are allocated.
    const BITS: u32 = 8;

    /// The element count at which a straightforward algorithm is used.
    const CUTOFF: usize = 200_000;

    if elements_len < CUTOFF || key_bitness <= BITS {
        return group_by_key_fallback(
            elements,
            elements_len,
            key,
            key_bitness,
            base_shift,
            callback,
        );
    }

    let shift = base_shift + (key_bitness - BITS);

    let reserved_capacity = (elements_len >> BITS).max(1); // 0 breaks `chunks_mut`

    // Partitioning a single allocation is more efficient than allocating multiple times
    let mut buffer = vec![MaybeUninit::uninit(); reserved_capacity << BITS];
    let mut buckets = buffer
        .chunks_mut(reserved_capacity)
        .map(|chunk| Bucket::new(FixedSliceVec::new(chunk)));
    let mut buckets: [Bucket<T>; 1 << BITS] = core::array::from_fn(|_| buckets.next().unwrap());

    for element in elements {
        #[expect(
            clippy::cast_possible_truncation,
            reason = "we mask out the higher bits anyway"
        )]
        buckets[(key(element) >> shift) as usize & ((1 << BITS) - 1)].push(element);
    }

    for bucket in buckets {
        group_by_key(
            bucket.iter().copied(),
            bucket.len(),
            key,
            key_bitness - BITS,
            base_shift,
            callback,
        )?;
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
