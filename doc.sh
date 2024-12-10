#!/usr/bin/env sh
RUSTDOCFLAGS="-Z unstable-options --document-hidden-items --cfg devdoc" cargo +nightly doc --document-private-items --all-features --workspace --exclude h-test
