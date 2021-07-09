#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license
#
# Usage:
#     contrib/asan.sh <path> [<make_targets>...]
#
# Build ASAN-enabled julia checked out in the current directory.  Given a
# workspace directory <path>, build ASAN-enabled julia in <path>/asan.  Required
# toolss are install under <path>/toolchain.  This scripts also takes optional
# <make_targets> arguments which are passed to `make`.  The default make target
# is `debug`.

set -ue

# `$WORKSPACE` is a directory in which we careate `toolchain` and `asan`
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

echo
echo "Installing toolchain..."

TOOLCHAIN="$WORKSPACE/toolchain"
if [ ! -d "$TOOLCHAIN" ]; then
    make configure O=$TOOLCHAIN

    cat <<EOD | sed 's/^ *//' > "$TOOLCHAIN/Make.user"
        USE_BINARYBUILDER_LLVM=1
        BUILD_LLVM_CLANG=1
EOD
fi

make -C "$TOOLCHAIN/deps" install-clang install-llvm-tools

echo
echo "Building Julia..."

BUILD="$WORKSPACE/asan"
if [ ! -d "$BUILD" ]; then
    make configure O="$BUILD"

    cat <<'EOD' | sed 's/^ *//' > "$BUILD/Make.user"
    TOOLCHAIN=$(BUILDROOT)/../toolchain/usr/tools

    # use our new toolchain
    USECLANG=1
    override CC=$(TOOLCHAIN)/clang
    override CXX=$(TOOLCHAIN)/clang++
    export ASAN_SYMBOLIZER_PATH=$(TOOLCHAIN)/llvm-symbolizer

    USE_BINARYBUILDER_LLVM=1

    override SANITIZE=1
    override SANITIZE_ADDRESS=1

    # make the GC use regular malloc/frees, which are hooked by ASAN
    override WITH_GC_DEBUG_ENV=1

    # default to a debug build for better line number reporting
    override JULIA_BUILD_MODE=debug

    # make ASAN consume less memory
    export ASAN_OPTIONS=detect_leaks=0:fast_unwind_on_malloc=0:allow_user_segv_handler=1:malloc_context_size=2

    JULIA_PRECOMPILE=1

    # tell libblastrampoline to not use RTLD_DEEPBIND
    export LBT_USE_RTLD_DEEPBIND=0
EOD
fi

make -C "$BUILD" "$@"
