Thank you for deciding to contribute to `h`! We appreciate it.

There's many ways to contribute to `h`:

- Find bugs. We all know they're there. Feel free to open an issue or a PR.
- Add documentation. There's never enough of it.
- Port APIs from other hash tables to `h` tables. This includes various methods on `HashSet` that we don't currently implement.
- Look for datasets for which generation hangs or takes more time than expected.
- Speed up the building process. This is difficult, but very rewarding.
- Improve `GenericHasher` performance for specific types, while keeping it a quality hash.
- Add `PortableHash` implementations for commonly used types.


## Testing

For the `h` crate, `cargo test` works.

Some tests that need gigabytes of RAM are ignored by default; run them with `cargo test -- --include-ignored`. These tests are *very* slow in debug mode, so we enable `opt-level = 3` for tests. Make sure not to turn that off manually.

There's also a separate `h-test` crate in the workspace, specifically for stress testing and comparison with other hash table implementations. To prevent Cargo/rust-analyzer from rerunning its slow `build.rs` script on each unrelated code change, the crate is behind the `enabled` feature. Use `cargo run --release -F enabled`.


## Code contributions

This is a small Rust project, so it's quite simple code-wise. Private items are documented, so you might want to use `cargo doc --document-private-items` to navigate around the code.

There's a few important details to keep in mind, though.


### Performance

`h` is performance-oriented and tuned for large datasets, so please consider the impact of your changes on build times amd memory utilization. This is less important for codegen (`h::static_map!` and similar) and more important for runtime building (`Map::from_entries` and similar).

The PHF building process is probabilistic and needs to be restarted with different parameters on failure. As such, it's important to cache data that can be reused between runs rather than regenerate it from scratch. For example, separating elements into buckets is independent from the choice of the mixer, so we share this data structure between mixers.

Consider using cache-aware/cache-oblivious algorithms. As data often doesn't fit into cache, it's necessary to consider the performance loss from cache misses. Iterate in a sequential order when possible. This includes using sorted containers instead unsorted containers when both are accessible.


### Hash choice

For the PHF build to success, the underlying imperfect hash has to be of a good enough quality -- in particular, entropy needs to be concentrated in the top few bits. This is not very rigorous (although possible to state formally), but at least it's intuitive.

The point here is that the hash being collision free is *not* enough for the build to be successful. You might want to take a look at SMHasher results to see if the hash finalizer is good enough. This is not *necessary* as long as you know what you're doing, but it's a simple indicator otherwise.

Finally, make sure that all building tests pass and don't take considerably more time than before, which would indicate worse hash quality. If you suspect that a hash might have a weak point, please state that in the PR. Ideally, add a test for that kind of data.


### Codegen

In macros, we generate code that manually fills field of the PHF. We don't want to stabilize those fields, and they're `unsafe` to touch anyway, so instead we declare them as `#[doc(hidden)] pub`. Semantically, these fields are private.
