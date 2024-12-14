//! Permutation utilities.

#![cfg(feature = "build")]

use alloc::vec::Vec;

/// Move elements from `x = source[i]` to `destination[permutation(x)]`.
pub(crate) fn scatter<T>(
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

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    #[test]
    fn inverse() {
        let source = vec![5, 0, 4, 2, 3, 1, 6];
        let mut destination = vec![None; 7];
        scatter(source, |&element| element, &mut destination);
        assert_eq!(destination, (0..=6).map(Some).collect::<Vec<_>>());
    }
}
