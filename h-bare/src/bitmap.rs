//! Bitmap API.

#![cfg(feature = "build")]

use alloc::{vec, vec::Vec};

/// Bit-compressed [`Vec<bool>`].
pub struct BitMap {
    /// Underlying container.
    ///
    /// Bit `index` is stored in byte `index / 8` at bit `index % 8`, counting from LSB.
    data: Vec<u8>,
}

impl BitMap {
    /// Create a bitmap of a given length, filled with one bits.
    pub fn new_ones(len: usize) -> Self {
        Self {
            data: vec![u8::MAX; len.div_ceil(8)],
        }
    }

    /// Set the bit at `index` to zero.
    ///
    /// # Safety
    ///
    /// `index` must be less than the length of the bitmap.
    pub unsafe fn reset_unchecked(&mut self, index: usize) {
        // SAFETY: `index / 8` <= `(len - 1) / 8` = `len.div_ceil(8) - 1`
        *unsafe { self.data.get_unchecked_mut(index / 8) } &= !(1 << (index % 8));
    }

    /// Get a byte pointer to the underlying data.
    ///
    /// The bit at index `index` is guaranteed to be stored in bit `index % 8` (counting from LSB)
    /// of byte `index / 8` of the returned pointer.
    ///
    /// The returned pointer is guaranteed to be valid for reads of size `len.div_ceil(8)`.
    pub fn as_byte_ptr(&self) -> *const u8 {
        self.data.as_ptr()
    }
}
