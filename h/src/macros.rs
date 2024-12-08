//! Macros for compile time generation.
//!
//! Unlike [`Map::from_entries`] and similar methods, these macros are zero-cost in runtime. Use
//! them whenever the keys are fixed, can be embedded in the source code, and are of simple types.
//!
//!
//! # Types
//!
//! Simple types include:
//!
//! - Integers, `str`, `bool`, `char`
//! - References, tuples, arrays, and slices to/of the above, including recursion
//!
//! Notably, this does not include most types provided by `std` or the user. If such a key type
//! needs to be used, use [`codegen`] instead of macros.
//!
//! This limitation applies only to keys of [`Map`]s and values of [`Set`]s and [`Phf`]s. Notably,
//! it doesn't apply to values of [`Map`]s, as the values don't need to be analyzed in compile time.
//!
//!
//! # Type inference
//!
//! The macros try to infer the key types from the provided keys, but this is not always possible.
//! For example, in the following context:
//!
//! ```compile_fail
//! const MAP: &h::Map<(u64, &str), ()> = h::map! {
//!     (1, "a") => (),
//!     (2, "b") => (),
//! };
//! ```
//!
//! ...the macro cannot guess the correct key type, as macros cannot "look outside" or participate
//! in type inference. In this case, the type has to be specified explicitly:
//!
//! ```rust
//! const MAP: &h::Map<(u64, &str), ()> = h::map! {
//!     for (u64, _);  // type annotation
//!     (1, "a") => (),
//!     (2, "b") => (),
//! };
//! ```
//!
//! Note that it's only necessary to specify types that could not be inferred otherwise, although
//! `for (u64, &str);` would work too.
use super::*;

#[doc(hidden)]
pub use h_macros;

// Two reasons for `macro_rules!` instead of a direct reexport:
// 1. Document the accepted syntax.
// 2. Pass `$crate` to the macro.

/// Create a [`Map`] in compile time.
///
/// See [module-level documentation](self) for more information.
///
///
/// # Example
///
/// [`map!`] usually returns a reference to a map. Most maps should be put in a `const`:
///
/// ```rust
/// const MAP: &h::Map<&str, i32> = h::map! {
///     "hello" => 1,
///     "world" => 2,
/// };
///
/// assert_eq!(MAP.get("hello"), Some(&1));
/// assert_eq!(MAP.get("world"), Some(&2));
/// assert_eq!(MAP.get("other"), None);
/// ```
///
///
/// # Mutability
///
/// Sometimes you have values that need to be initialized or even mutated in runtime. In this case,
/// add `mut;` to the beginning of the macro arguments. This enables the macro to return [`Map`]
/// instead of `&Map`.
///
/// You can now put the map in a `let`/`let mut` binding or initialize it in a `const` with
/// [`LazyLock`](std::sync::LazyLock):
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
/// ```rust
/// let mut map = h::map! {
///     mut;
///     "hello" => 1,
///     "world" => 2,
/// };
///
/// assert_eq!(map.get_mut("hello"), Some(&mut 1));
/// ```
///
/// `mut;` does not work in a `const` context.
///
/// If you accidentally omit `mut;`, you might see errors like:
///
/// > temporary value dropped while borrowed
///
/// or:
///
/// > cannot borrow `*map` as mutable, as it is behind a `&` reference
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

/// Create a [`Set`] in compile time.
///
/// See [module-level documentation](self) for more information.
///
///
/// # Example
///
/// [`set!`] returns a reference to a set. Sets should be put in a `const`:
///
/// ```rust
/// const SET: &h::Set<&str> = h::set!("hello", "world");
///
/// assert!(SET.contains("hello"));
/// assert!(SET.contains("world"));
/// assert!(!SET.contains("other"));
/// ```
#[cfg(doc)]
#[macro_export]
macro_rules! set {
    // Not actually valid/usable macro rules, but close enough for docs.
    (
        $(for $element_type:ty;)?
        $($element:expr),* $(,)?
    ) => {
        // Doesn't need to typecheck, but needs to parse.
        ()
    };
}

/// Create a [`Phf`] in compile time.
///
/// See [module-level documentation](self) for more information.
///
///
/// # Example
///
/// [`phf!`] returns a reference to a PHF. PHFs should be put in a `const`:
///
/// ```rust
/// const PHF: &h::Phf<&str> = h::phf!("hello", "world");
///
/// assert_ne!(PHF.hash("hello"), PHF.hash("world"));
/// ```
#[cfg(doc)]
#[macro_export]
macro_rules! phf {
    // Not actually valid/usable macro rules, but close enough for docs.
    (
        $(for $key_type:ty;)?
        $($key:expr),* $(,)?
    ) => {
        // Doesn't need to typecheck, but needs to parse.
        ()
    };
}

#[cfg(not(doc))]
#[macro_export]
macro_rules! map {
    ($($tt:tt)*) => {
        $crate::macros::h_macros::map!(crate $crate; $($tt)*)
    };
}

#[cfg(not(doc))]
#[macro_export]
macro_rules! set {
    ($($tt:tt)*) => {
        $crate::macros::h_macros::set!(crate $crate; $($tt)*)
    };
}

#[cfg(not(doc))]
#[macro_export]
macro_rules! phf {
    ($($tt:tt)*) => {
        $crate::macros::h_macros::phf!(crate $crate; $($tt)*)
    };
}

pub use map;
pub use phf;
pub use set;

#[cfg(test)]
mod tests {
    use std::sync::Mutex;

    #[test]
    fn macros() {
        const MAP: &h::Map<u64, usize> = map! {
            for u64;
            123 => 0,
            456 => 1,
        };
        assert_eq!(MAP.get(&123), Some(&0));
        assert_eq!(MAP.get(&456), Some(&1));
        assert_eq!(MAP.get(&789), None);

        let map = h::map! {
            mut;
            "hello" => Mutex::new(1),
            "world" => Mutex::new(2),
        };

        let mutex = map.get("hello").unwrap();
        let guard = mutex.lock().unwrap();
        assert_eq!(*guard, 1);

        const SET: &h::Set<u64> = set!(for u64; 123, 456);
        assert!(SET.contains(&123));
        assert!(SET.contains(&456));
        assert!(!SET.contains(&789));

        const PHF: &h::Phf<u64> = phf!(for u64; 123, 456);
        assert_ne!(PHF.hash(&123), PHF.hash(&456));
    }

    #[test]
    fn ui() {
        let t = trybuild::TestCases::new();
        t.compile_fail("tests/ui/*.rs");
    }
}
