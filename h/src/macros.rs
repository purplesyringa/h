use super::*;

#[doc(hidden)]
pub use h_macros;

// Two reasons for `macro_rules!` instead of a direct reexport:
// 1. Document the accepted syntax.
// 2. Pass `$crate` to the macro.

/// Create a [`Map`]() in compile time.
///
/// Unlike [`Map::from_entries`], this macro is zero-cost in runtime. Use it whenever the keys are
/// fixed and can be embedded in the source code (otherwise, use [`codegen`]).
///
/// # Example
///
/// Most maps should be put in a `const`:
///
/// ```rust
/// const MAP: h::Map<&str, i32> = h::map! {
///     "hello" => 1,
///     "world" => 2,
/// };
///
/// assert_eq!(MAP.get("hello"), Some(&1));
/// assert_eq!(MAP.get("world"), Some(&2));
/// assert_eq!(MAP.get("other"), None);
/// ```
///
/// Alternatively, if you have values that, for some reason, need to be initialized in runtime, you
/// can use `let` or [`LazyLock`](std::sync::LazyLock), just like you would with `std` types:
///
/// ```rust
/// use std::sync::Mutex;
///
/// let map = h::map! {
///     mut;
///     "hello" => Mutex::new(1),
///     "world" => Mutex::new(2),
/// };
///
/// let mutex = map.get("hello").unwrap();
/// let guard = mutex.lock().unwrap();
/// assert_eq!(*guard, 1);
/// ```
///
/// Here, `mut;` indicates that the values are constructed outside the `const` context. If you omit
/// `mut;`, you might see errors like "temporary value dropped while borrowed".
#[cfg(doc)]
#[macro_export]
macro_rules! map {
    // Not actually valid/usable macro rules, but close enough for docs.
    (
		$(for $key_type:ty;)?
		$(mut;)?
		$($key:expr => $value:expr),* $(,)?
	) => {
        // Doesn't need to typecheck, but needs to parse.
        ()
    };
}

#[cfg(not(doc))]
#[macro_export]
macro_rules! map {
	($($tt:tt)*) => {
		$crate::h_macros::map!(crate $crate; $($tt)*)
	};
}

// pub use h_macros::phf;
// pub use h_macros::set;

#[must_use]
#[doc(hidden)]
#[inline]
pub fn conjure<T>() -> T {
    unreachable!();
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex;

    #[test]
    fn macros() {
        let map: h::Map<_, usize> = map! {
            for &u64;
            &123 => 0,
            &456 => 1,
        };
        assert_eq!(map.get(&123), Some(&0));
        assert_eq!(map.get(&456), Some(&1));
        assert_eq!(map.get(&789), None);

        let map = h::map! {
            mut;
            "hello" => Mutex::new(1),
            "world" => Mutex::new(2),
        };

        let mutex = map.get("hello").unwrap();
        let guard = mutex.lock().unwrap();
        assert_eq!(*guard, 1);
    }
}
