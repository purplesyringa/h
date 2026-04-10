//! An efficient specialized `group_by` implementation.

use alloc::{vec, vec::Vec};
use core::mem::MaybeUninit;
use fixed_slice_vec::FixedSliceVec;

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
    fn iter(&self) -> core::iter::Chain<core::slice::Iter<'_, T>, core::slice::Iter<'_, T>> {
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
fn group_by_key_fallback<E>(
    elements: impl Iterator<Item = u64> + Clone,
    elements_len: usize,
    key_bitness: u32,
    acc: u64,
    acc_bitness: u32,
    callback: &mut impl FnMut(&[u64], u64) -> Result<(), E>,
) -> Result<(), E> {
    let n_groups = 1 << key_bitness;

    let mut counts: Vec<usize> = vec![0; n_groups];
    for element in elements.clone() {
        counts[element as usize & (n_groups - 1)] += 1;
    }

    let mut group_ptrs: Vec<usize> = vec![0; n_groups];
    for i in 1..n_groups {
        group_ptrs[i] = group_ptrs[i - 1] + counts[i - 1];
    }

    let mut buffer = vec![MaybeUninit::uninit(); elements_len];
    for element in elements {
        let group_ptr = &mut group_ptrs[element as usize & (n_groups - 1)];
        buffer[*group_ptr].write(element >> key_bitness);
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
            // XXX: This is ad-hoc `assume_init_ref`, since our MSRV doesn't include it.
            let group = unsafe { &*(core::ptr::from_ref(group) as *const [u64]) };
            callback(group, acc | ((i as u64) << acc_bitness))?;
        }
    }

    Ok(())
}

/// Split elements into groups by key.
///
/// Groups elements from the iterator based on the key and invokes `callback` for each group. Unlike
/// [`[T]::chunk_by`], non-consecutive elements are merged, too. The groups are emitted in order of
/// increasing key.
///
/// The key is computed by taking the bottom `key_bitness` bits of `element`. The callback is
/// invoked with the key stripped by right-shift.
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
/// Might panic if `key_bitness + acc_bitness >= 64` or if `elements_len` does not match the number
/// of elements in `elements`.
pub fn group_by_key<E>(
    elements: impl Iterator<Item = u64> + Clone,
    elements_len: usize,
    key_bitness: u32,
    acc: u64,
    acc_bitness: u32,
    callback: &mut impl FnMut(&[u64], u64) -> Result<(), E>,
) -> Result<(), E> {
    /// The step at which `key` is consumed. `2 ** BITS` buckets are allocated.
    const BITS: u32 = 8;

    /// The element count at which a straightforward algorithm is used.
    const CUTOFF: usize = 200_000;

    if elements_len < CUTOFF || key_bitness <= BITS {
        return group_by_key_fallback(
            elements,
            elements_len,
            key_bitness,
            acc,
            acc_bitness,
            callback,
        );
    }

    // Partitioning a single allocation is more efficient than allocating multiple times
    let reserved_capacity = (elements_len >> BITS).max(1); // 0 breaks `chunks_mut`
    let mut buffer = vec![MaybeUninit::uninit(); reserved_capacity << BITS];
    let mut buckets = buffer
        .chunks_mut(reserved_capacity)
        .map(|chunk| Bucket::new(FixedSliceVec::new(chunk)));
    let mut buckets: [Bucket<u64>; 1 << BITS] = core::array::from_fn(|_| buckets.next().unwrap());

    for element in elements {
        buckets[element as usize & ((1 << BITS) - 1)].push(element >> BITS);
    }

    for (i, bucket) in buckets.into_iter().enumerate() {
        group_by_key(
            bucket.iter().copied(),
            bucket.len(),
            key_bitness - BITS,
            acc | ((i as u64) << acc_bitness),
            acc_bitness + BITS,
            callback,
        )?;
    }

    Ok(())
}
