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
    /// Persisted state.
    pub(crate) state: State,

    /// Bit mask for the `Bucket`.
    bucket_mask: usize,

    /// The bound on produced indices.
    capacity: usize,
}

/// Validation failure.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum LoadError {
    /// invalid displacement count
    #[error("invalid displacement count")]
    InvalidDisplacementCount,
}

impl Phf {
    /// Create an empty PHF.
    ///
    /// The resulting PHF can be used with any hasher and any data types.
    #[must_use]
    pub const fn new() -> Self {
        unsafe {
            Self::load_unchecked(State {
                approx_range: 1,
                displacements: Displacements::Borrowed(&[0]),
            })
        }
    }

    /// Load [`State`] as [`Phf`], performing validation.
    ///
    /// # Errors
    ///
    /// Returns an error if the state is malformed.
    pub fn load(state: State) -> Result<Self, LoadError> {
        if !state.displacements.len().is_power_of_two() {
            return Err(LoadError::InvalidDisplacementCount);
        }
        Ok(unsafe { Self::load_unchecked(state) })
    }

    /// Load [`State`] as [`Phf`] without performing validation.
    ///
    /// # Safety
    ///
    /// This is safe to use if the [`State`] is obtained directly from [`Phf::state`] or has
    /// previously been validated by [`Phf::load`].
    #[must_use]
    pub const unsafe fn load_unchecked(state: State) -> Self {
        let bucket_mask = state.displacements.as_slice().len() - 1;
        let capacity = get_capacity(state.approx_range, state.displacements.as_slice());
        Self {
            state,
            bucket_mask,
            capacity,
        }
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
        let (approx, bucket) = to_approx_bucket(hash, self.state.approx_range, self.bucket_mask);
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
        self.capacity
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
/// Panics if the capacity is out of bounds.
pub(crate) const fn get_capacity(approx_range: usize, displacements: &[u16]) -> usize {
    let mut i = 0;
    let mut max_displacement = 0;
    while i < displacements.len() {
        if displacements[i] > max_displacement {
            max_displacement = displacements[i];
        }
        i += 1;
    }

    approx_range.strict_add(max_displacement as usize)
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
