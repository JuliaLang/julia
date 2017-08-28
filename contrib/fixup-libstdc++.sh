#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Run as: fixup-libstdc++.sh <libdir> <private_libdir>

if [ -z "$1" ]; then
    echo "Usage: $0 <libdir> <private_libdir>"
    exit 1
fi

libdir="$1"
private_libdir="$2"

if [ ! -f "$libdir/libjulia.so" ]; then
    echo "ERROR: Could not open $libdir/libjulia.so" >&2
    exit 2
fi

find_shlib ()
{
    if [ -f "$1" ]; then
        ldd "$1" | grep $2 | cut -d' ' -f3 | xargs
    fi
}

# Discover libstdc++ location and name
LIBSTD=$(find_shlib "$libdir/libjulia.so" "libstdc++.so")
LIBSTD_NAME=$(basename $LIBSTD)
LIBSTD_DIR=$(dirname $LIBSTD)

if [ ! -f "$private_libdir/$LIBSTD_NAME" ] && [ -f "$LIBSTD_DIR/$LIBSTD_NAME" ]; then
    cp -v "$LIBSTD_DIR/$LIBSTD_NAME" "$private_libdir"
    chmod 755 "$private_libdir/$LIBSTD_NAME"
fi
