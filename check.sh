#!/usr/bin/env sh
set -e
for features in "" "-F alloc" "-F build" "-F codegen" "-F macros" "-F serde" "-F std" "--all-features"; do
    echo "Checking on feature set: $features"
    cargo check --package h --no-default-features $features
done
