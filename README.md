Work in progress.

<!--

# hyble

Practical, efficient [perfect hash functions](https://en.wikipedia.org/wiki/Perfect_hash_function).

A PHF is a function that maps a pre-determined set of keys to indices without collisions. Static hash tables built upon such hashes can be much simpler than usual, speeding up access time at the expense of build time.

This crate does not provide hashmaps or hashsets based on this PHF, and instead focuses on perfecting the low-level mechanism. Hopefully [rust-phf](https://docs.rs/phf) supports this crate as the base one day. In the meantime, hashmaps can be implemented manually.

See [docs.rs](https://docs.rs/hyble) for documentation and examples.


### Contributing

Random stuff to be aware of:

- hyble has features, any combinations of which should compile. `cargo check` doesn't enable any of them. Features can be enabled individually with `cargo check -F serde -F build`. Same for Clippy.

- hyble is `#![no_std]` and gates methods that require allocation behind `#[cfg(feature = "alloc")]` or equivalent.

- hyble has stress-tests that need hundreds of megabytes of memory to run. They are ignored by default and can be enabled with `cargo test -- --include-ignored`.

- hyble is data-oriented and tuned for large datasets. We cache data that can be reused between seeds, use cache-aware algorithms, profile code, and monitor build times and memory utilization.

- hyble started back when rust-phf wasn't well-maintained, and I wanted to provide a powerful alternative with a much broader scope than hyble is today. That code can be found on the `legacy` branch, if you ever need to `git blame` it.

-->
