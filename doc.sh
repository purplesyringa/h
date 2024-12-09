#!/usr/bin/env sh
RUSTDOCFLAGS="-Z unstable-options --document-hidden-items" cargo +nightly doc --document-private-items --all-features
