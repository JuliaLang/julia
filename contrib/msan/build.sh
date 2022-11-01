#!/bin/bash
# This file is a part of Julia. License is MIT: https://julialang.org/license

#
# Usage:
#     contrib/asan/build.sh <path> [<make_targets>...]
#
# Build MSAN-enabled julia.  Given a workspace directory <path>, build
# MSAN-enabled julia in <path>/msan.  Required toolss are install under
# <path>/toolchain.  This scripts also takes optional <make_targets> arguments
# which are passed to `make`.  The default make target is `debug`.

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
JULIA_HOME="$HERE/../.."

echo
echo "Installing toolchain..."

TOOLCHAIN="$WORKSPACE/toolchain"
if [ ! -d "$TOOLCHAIN" ]; then
    make -C "$JULIA_HOME" -j10 configure O=$TOOLCHAIN
    cp "$HERE/Make.user.tools"  "$TOOLCHAIN/Make.user"
fi

make -C "$TOOLCHAIN/deps" install-clang install-llvm-tools

echo
echo "Building Julia..."

BUILD="$WORKSPACE/msan"
if [ ! -d "$BUILD" ]; then
    make -C "$JULIA_HOME" -j10 configure O="$BUILD"
    cp "$HERE/Make.user.msan"  "$BUILD/Make.user"
fi

make -C "$BUILD" "$@" julia-src-debug -j10

rm -f "$BUILD/usr/lib/libstdc++.so" "$BUILD/usr/lib/libstdc++.so.6" "$BUILD/usr/lib/libstdc++.so.6.0.29"

STDCXXURL=https://github.com/JuliaBinaryWrappers/LibStdCxx_jll.jl/releases/download/LibStdCxx-v12.1.0%2B1/LibStdCxx.v12.1.0.x86_64-linux-gnu-sanitize+memory.tar.gz
curl -LJ $STDCXXURL | tar -xzf - --directory "$BUILD/usr" lib/libstdc++.so lib/libstdc++.so.6 lib/libstdc++.so.6.0.30

LIBFLANGURL=https://github.com/JuliaBinaryWrappers/FlangClassic_RTLib_jll.jl/releases/download/FlangClassic_RTLib-v13.0.0%2B0/FlangClassic_RTLib.v13.0.0.x86_64-linux-gnu.tar.gz

curl -JL $LIBFLANGURL | tar -xzf - --directory "$BUILD/usr" lib/libflang.so lib/libflangrti.so lib/libomp.so lib/libompstub.so lib/libpgmath.so

JULIA_SYSIMG_BUILD_FLAGS="--emit-sanitizer=msan" make -C "$JULIA_HOME" julia-sysimg-bc -j10

ar -x --output $JULIA_HOME/usr/lib/julia/ $JULIA_HOME/usr/lib/julia/sys-bc.a
$TOOLCHAIN/usr/tools/opt --passes=msan $JULIA_HOME/usr/lib/julia/text.bc -o $JULIA_HOME/usr/lib/julia/msan-text.bc
$TOOLCHAIN/usr/tools/clang $JULIA_HOME/usr/lib/julia/msan-text.bc $JULIA_HOME/usr/lib/julia/data.bc -fPIC  -shared -o $BUILD/usr/lib/julia/sys-debug.so