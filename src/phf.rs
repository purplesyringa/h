use super::state::{Displacements, Mixer, State};
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
    /// too large `bucket_shift` value
    #[error("too large `bucket_shift` value")]
    TooLargeBucketShift,

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
                bucket_shift: 63,
                displacements: Displacements::Borrowed(&[0, 0]),
                mixer: Mixer::Add,
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
        if state.bucket_shift >= 64 {
            return Err(LoadError::TooLargeBucketShift);
        }

        let bucket_count = 1 << (64 - state.bucket_shift);
        if state.displacements.len() as u64 != bucket_count {
            return Err(LoadError::WrongDisplacementCount);
        }

        if state.capacity
            != state
                .mixer
                .capacity(state.approx_range, &state.displacements)
        {
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
            to_approx_bucket(hash, self.state.approx_range, self.state.bucket_shift);
        let displacement = unsafe { *self.state.displacements.get_unchecked(bucket) };
        self.state.mixer.mix(approx, displacement)
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
    bucket_shift: u32,
) -> (u64, usize) {
    let product = hash as u128 * approx_range as u128;
    #[expect(clippy::cast_possible_truncation, reason = "intentional")]
    let (high, low) = ((product >> 64i32) as u64, product as u64);
    #[expect(
        clippy::cast_possible_truncation,
        reason = "Bucket is guaranteed to fill in usize"
    )]
    (high, (low >> bucket_shift) as usize)
}

impl Mixer {
    /// Mix `Approx` with a displacement to get the final index.
    pub(crate) const fn mix(self, approx: u64, displacement: u16) -> usize {
        #[expect(
            clippy::cast_possible_truncation,
            reason = "approx + 65535 < approx_range + 65535 <= usize::MAX"
        )]
        match self {
            Self::Add => approx as usize + displacement as usize,
            Self::Xor => approx as usize ^ displacement as usize,
        }
    }

    /// Get the upper bound on indices.
    ///
    /// Given the range of `Approx` values and the table of displacements, computes the limit on
    /// hashes valid for absent keys.
    pub(crate) fn capacity(self, approx_range: usize, displacements: &[u16]) -> usize {
        let max_displacement = displacements.iter().copied().max().unwrap_or(0) as usize;
        match self {
            Self::Add => approx_range + max_displacement,
            Self::Xor => {
                let x = approx_range - 1;
                let y = max_displacement;
                let bound = (x & y).checked_ilog2().unwrap_or(0);
                let max_xor = x | y | ((1 << bound) - 1);
                max_xor + 1
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_capacity() {
        assert_eq!(Mixer::Add.capacity(0x123, &[0x456]), 0x579);

        for approx_range in 1..0x10 {
            for displacement1 in 0..0x10 {
                for displacement2 in 0..0x10 {
                    let answer = Mixer::Xor.capacity(approx_range, &[displacement1, displacement2]);
                    let expected = (0..approx_range)
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
    fn test_to_approx_bucket() {
        assert_eq!(to_approx_bucket(0x1289f3da73209aad, 12345, 53), (893, 2035));
    }

    #[test]
    fn test_mix() {
        assert_eq!(Mixer::Add.mix(0x123, 0x456), 0x579);
        assert_eq!(Mixer::Xor.mix(0x123, 0x456), 0x575);
    }
}
