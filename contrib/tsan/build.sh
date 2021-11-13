#!/bin/bash
# This file is a part of Julia. License is MIT: https://julialang.org/license

#
# Usage:
#     contrib/tsan/build.sh <path> [<make_targets>...]
#
# Build TSAN-enabled julia.  Given a workspace directory <path>, build
# TSAN-enabled julia in <path>/tsan.  Required toolss are install under
# <path>/toolchain.  Note that the same <path> passed to `contrib/asan/build.sh`
# can be used to share the toolchain used for ASAN.  This scripts also takes
# optional <make_targets> arguments which are passed to `make`.  The default
# make target is `debug`.

set -ue

# `$WORKSPACE` is a directory in which we create `toolchain` and `tsan`
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
    cp "$HERE/../asan/Make.user.tools"  "$TOOLCHAIN/Make.user"
fi

make -C "$TOOLCHAIN/deps" install-clang install-llvm-tools

echo
echo "Building Julia..."

BUILD="$WORKSPACE/tsan"
if [ ! -d "$BUILD" ]; then
    make -C "$JULIA_HOME" configure O="$BUILD"
    cp "$HERE/Make.user.tsan"  "$BUILD/Make.user"
fi

cd "$BUILD"  # so that we can pass `-C src` to `make`
make "$@"
