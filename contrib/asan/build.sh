#!/bin/bash
# This file is a part of Julia. License is MIT: https://julialang.org/license

#
# Usage:
#     contrib/asan/build.sh <path> [<make_targets>...]
#
# Build ASAN-enabled julia in <path>/asan. Required tools are installed under
# <path>/toolchain. Optional <make_targets> are passed to `make`; the default
# target is `release`.

set -ue

# `$WORKSPACE` is a directory in which we create `toolchain` and `asan`
# sub-directories.
WORKSPACE="$1"
shift
if [ "$WORKSPACE" = "" ]; then
    echo "Workspace directory must be specified as the first argument" >&2
    exit 2
fi

mkdir -pv "$WORKSPACE"
WORKSPACE="$(cd "$WORKSPACE" && pwd)"
if [ "$WORKSPACE" = "" ]; then
    echo "Failed to create the workspace directory." >&2
    exit 2
fi

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JULIA_HOME="$HERE/../../"

echo
echo "Installing toolchain..."

TOOLCHAIN="$WORKSPACE/toolchain"
if [ ! -d "$TOOLCHAIN" ]; then
    make -C "$JULIA_HOME" configure O=$TOOLCHAIN
    cp "$HERE/Make.user.tools"  "$TOOLCHAIN/Make.user"
fi

make -C "$TOOLCHAIN/deps" install-clang install-llvm-tools install-patchelf

echo
echo "Building Julia..."

BUILD="$WORKSPACE/asan"
if [ ! -d "$BUILD" ]; then
    make -C "$JULIA_HOME" configure O="$BUILD"
    cp "$HERE/Make.user.asan"  "$BUILD/Make.user"
fi

make -C "$BUILD" "$@"
