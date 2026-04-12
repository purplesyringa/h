//! Internal state of the PHF.
//!
//! Types in this module don't have type invariants, and their fields are considered stable. Feel
//! free to read and populate them by hand. [`State`] can be converted to a usable [`Phf`] with
//! [`Phf::load`] and extracted with [`Phf::state`].
//!
//! [`Phf`]: crate::Phf
//! [`Phf::load`]: crate::Phf::load
//! [`Phf::state`]: crate::Phf::state

/// The PHF state.
///
/// See [module-level documentation](self).
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct State {
    /// The upper bound on `Approx`.
    pub approx_range: usize,

    /// Per-bucket displacement values.
    pub displacements: Displacements,
}

/// Displacement array.
///
/// This is effectively `Cow<'static, [u16]>`, but compatible with `no_alloc`.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Displacements {
    /// Static data.
    Borrowed(&'static [u16]),
    /// Dynamically allocated data.
    #[cfg(feature = "alloc")]
    Owned(alloc::vec::Vec<u16>),
}

impl Displacements {
    /// Convert to slice.
    ///
    /// Equivalent to `&*displacements`, provided for use in `const` contexts.
    #[inline]
    #[must_use]
    pub const fn as_slice(&self) -> &[u16] {
        match self {
            Self::Borrowed(slice) => slice,
            #[cfg(feature = "alloc")]
            Self::Owned(vec) => vec.as_slice(),
        }
    }
}

impl core::ops::Deref for Displacements {
    type Target = [u16];

    #[inline]
    fn deref(&self) -> &[u16] {
        self.as_slice()
    }
}

#[cfg(feature = "serde")]
mod serde_support {
    use super::Displacements;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    impl Serialize for Displacements {
        #[inline]
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            (**self).serialize(serializer)
        }
    }

    impl<'de> Deserialize<'de> for Displacements {
        #[inline]
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            Deserialize::deserialize(deserializer).map(Displacements::Owned)
        }
    }
}
