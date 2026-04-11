use super::state::{Displacements, State};
use thiserror::Error;

/// Perfect hash function.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(into = "State", try_from = "State"))]
#[cfg_attr(
    feature = "serde",
    expect(
        clippy::unsafe_derive_deserialize,
        reason = "safety requirements are validated using TryFrom"
    )
)]
pub struct Phf {
    pub(crate) state: State,
}

/// Validation failure.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum LoadError {
    /// invalid `bucket_mask` value
    #[error("invalid `bucket_mask` value")]
    InvalidBucketMask,

    /// wrong displacement count
    #[error("wrong displacement count")]
    WrongDisplacementCount,

    /// wrong capacity
    #[error("wrong capacity")]
    WrongCapacity,
}

impl Phf {
    /// Create an empty PHF.
    ///
    /// The resulting PHF can be used with any hasher and any data types.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            state: State {
                approx_range: 1,
                bucket_mask: 0,
                displacements: Displacements::Borrowed(&[0]),
                capacity: 1,
            },
        }
    }

    /// Load [`State`] as [`Phf`], performing validation.
    ///
    /// # Errors
    ///
    /// Returns an error if the state is malformed.
    pub fn load(state: State) -> Result<Self, LoadError> {
        let bucket_count = state.bucket_mask.wrapping_add(1);
        if !bucket_count.is_power_of_two() {
            return Err(LoadError::InvalidBucketMask);
        }

        if state.displacements.len() != bucket_count {
            return Err(LoadError::WrongDisplacementCount);
        }

        if state.capacity != get_capacity(state.approx_range, &state.displacements) {
            return Err(LoadError::WrongCapacity);
        }

        Ok(Self { state })
    }

    /// Load [`State`] as [`Phf`] without performing validation.
    ///
    /// # Safety
    ///
    /// This is safe to use if the [`State`] is obtained directly from [`Phf::state`] or has
    /// previously been validated by [`Phf::load`].
    #[must_use]
    pub const unsafe fn load_unchecked(state: State) -> Self {
        Self { state }
    }

    /// Extract [`State`] from [`Phf`].
    #[must_use]
    pub const fn state(&self) -> &State {
        &self.state
    }
}

impl Default for Phf {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Phf> for State {
    fn from(phf: Phf) -> Self {
        phf.state
    }
}

impl TryFrom<State> for Phf {
    type Error = LoadError;
    fn try_from(state: State) -> Result<Self, LoadError> {
        Self::load(state)
    }
}

impl Phf {
    /// Get the index of a key.
    ///
    /// The whole point. Returns different indices for all keys from the training dataset.
    /// Collisions may be present for other keys. Indices are always below
    /// [`capacity()`](Self::capacity), regardless of whether the key was present in the dataset.
    ///
    /// The input should be pre-hashed with the seed that [`Phf::build`] returned, with the same
    /// hasher.
    #[inline]
    #[must_use]
    pub fn get(&self, hash: u64) -> usize {
        let (approx, bucket) =
            to_approx_bucket(hash, self.state.approx_range, self.state.bucket_mask);
        let displacement = unsafe { *self.state.displacements.get_unchecked(bucket) };
        approx + displacement as usize
    }

    /// Get the upper bound on indices.
    ///
    /// The index returned by [`get`](Self::get) is guaranteed to be less than `capacity()`, even
    /// for keys outside of the training dataset.
    ///
    /// This is not the number of keys stored in the [`Phf`] -- it's usually slightly larger. The
    /// PHF doesn't store the exact count.
    #[must_use]
    pub const fn capacity(&self) -> usize {
        self.state.capacity
    }
}

/// Convert a 64-bit hash to `(Approx, Bucket)`.
pub(crate) const fn to_approx_bucket(
    hash: u64,
    approx_range: usize,
    bucket_mask: usize,
) -> (usize, usize) {
    (
        ((hash as u128 * approx_range as u128) >> 64i32) as usize,
        hash as usize & bucket_mask,
    )
}

/// Calculate the upper bound on indices.
///
/// Given the range of `Approx` values and the table of displacements, computes the limit on hashes
/// valid for absent keys.
///
/// # Panics
///
/// Panics if `displacements` is empty.
pub(crate) fn get_capacity(approx_range: usize, displacements: &[u16]) -> usize {
    approx_range
        + displacements
            .iter()
            .copied()
            .max()
            .expect("no displacements") as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_capacity() {
        assert_eq!(get_capacity(0x123, &[1, 0x456, 2]), 0x579);
    }

    #[test]
    fn test_to_approx_bucket() {
        assert_eq!(
            to_approx_bucket(0x1289f3da73209aad, 12345, 0xff),
            (893, 0xad)
        );
    }
}
