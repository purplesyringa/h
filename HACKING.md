## Instructions

This section cover certain rules of thumb you should keep in mind while contributing.


### Testing

Run `cargo test` in `h-bare` and `h`. The tests in `h-bare` test everything but macros, the tests in `h` test macros.

Some tests that need gigabytes of RAM are ignored by default; run them with `cargo test -- --include-ignored`. These tests are *very* slow in debug mode, so we enable `opt-level = 3` for tests. Make sure not to turn that off manually.

There's also a separate `h-test` crate in the workspace, specifically for stress testing and comparison with other hash table implementations. To prevent Cargo/rust-analyzer from rerunning its slow `build.rs` script on each unrelated code change, the crate is behind the `enabled` feature. Use `cargo run --release -F enabled` to actually run it.


### Performance

`h` is performance-oriented and tuned to large datasets, so please consider the impact of your changes on build times amd memory utilization. This is less important for codegen (`h::static_map!` and similar) and more important for runtime building (`Map::from_entries` and similar).

The PHF building process is probabilistic and needs to be restarted with different parameters on failure. As such, it's important to cache data that can be reused between runs rather than regenerate it from scratch. For example, separating elements into buckets is independent from the choice of the mixer, so we share this data structure between mixers.

Consider using cache-aware/cache-oblivious algorithms. As data often doesn't fit into cache, it's necessary to consider the performance loss from cache misses. Iterate in a sequential order when possible. For example, this includes using sorted containers instead unsorted containers when both are accessible.


### Hash choice

For the PHF build to success, the underlying imperfect hash has to be of a good enough quality -- in particular, entropy needs to be concentrated in the top few bits. This is not very rigorous (although possible to state formally), but at least it's intuitive.

The point here is that the hash being collision free is *not* enough for the build to be successful. You might want to take a look at SMHasher results to see if the hash finalizer is good enough. This is not *necessary* as long as you know what you're doing, but it's a simple indicator otherwise.

Finally, make sure that all building tests pass and don't take considerably more time than before, which would indicate worse hash quality. If you suspect that a hash might have a weak point, please state that in the PR. Ideally, add a test for that kind of data.


## Architecture

This section covers the architecture of `h` and the reasons behind some unusual design decisions.


### Basics

This is supposed to be a small Rust project, but it's not without pitfalls. Let's get the basics out of the way first:

- We enable many [Clippy](https://github.com/rust-lang/rust-clippy) diagnostics, perhaps more than necessary sometimes. Feel free to `#[allow]` diagnostics that get in the way. Prefer doing that as locally as possible, unless that's too intrusive.

- We support `#![no_std]` and no-alloc environments. Try to get this separation straight: if a certain type is available in both `std` and `core`, import it from `core`. Clippy will help you here. Hiding methods that require std/alloc behind `#[cfg(feature = "std/alloc")]` is perfectly fine, but if a method can reasonably be implemented without std/alloc, prefer doing that. (Macros always require std/alloc, so following this rule in `h-macros` is not critical.)

- We enforce documentation, including on private items. Try using `cargo doc --document-private-items` to find your way around the code.


### Crate separation

We publish three crates:

- `h-bare`, which contains everything except for macros.
- `h-macros`, which implements proc macros and depends on `h-bare`.
- `h`, which reexports both.

`h` is considered public API, everything else is effectively private. Sadly, we can't merge any of these crates due to cyclic dependencies.


### Macros

The main logic is implemented in `h-macros` crate, but `h` reexports the procedural macros through another delarative macro abstraction. There's two reasons to these shenanigans:

- We can pass various information the decl-macro knows but the proc-macro doesn't, such as `$crate`. This is necessary so that `h` macros can be embedded in other crates' declarative macros.

- `rustdoc` renders the arms of declarative macros in documentation, allowing us to document the syntax easily.

Please keep the inputs to the decl- and proc-macros as close as possible. Decl-macros shouldn't transform the input more than required by the two points above.

We deliberately keep the key format as loose as possible. The goal is to accept all reasonable Rust code that we can parse and interpret, even if that's complicated. The reason the `h-macros` crate is so big is that it's effectively a handicapped Rust interpreter. It can't parse most expressions, but we do have type inference. We haven't needed Hindley-Milner so far, but we'll switch to it if push comes to shove. Currently, we support:

- Tuples, arrays, and slices ([unlike `phf`](https://github.com/rust-phf/rust-phf/issues/198))
- Suffix-less integer literals, as long as the type is specified in one suffix or in the type annotation ([unlike `phf`](https://github.com/rust-phf/rust-phf/issues/78))
- Byte strings ([unlike `phf`](https://github.com/rust-phf/rust-phf/issues/151))


### Codegen

We generate code that manually fills fields by calling the `from_raw_parts` methods on `h` types. We don't want to stabilize these methods, and semantically they are `unsafe`, so instead we declare them as `#[doc(hidden)]`.


### Imperfect hashes

We generally use `rapidhash` to hash data, as it's pretty fast. However, it doesn't support streaming, so it's going to be subefficient for, say, hashing `&[u8]`. This is a big problem, and it should really be solved at some point. However, it's the best thing we have currently.

For hashing *certain primitives*, we use a more optimal approach. In particular, we use faster hashes for integer types from `u8` to `u128`, as well as their signed counterparts.

Now here's the problem. If `K: Borrow<Q>` is the key type and `Q` is the type used to access a hashmap, `K` and `Q` must have equivalent `PortableHash` implementation. This is required so that, say, `Map<String, _>` can be addressed with `&str`. As we overload the behavior for primitives, we also need to overload the behavior for all references to primitives, including nested ones. This requires an additional method on the hashing trait, which we call `hash_generic_exclusive`.
