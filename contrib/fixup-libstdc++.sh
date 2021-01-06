#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Run as: fixup-libstdc++.sh <libdir> <private_libdir>

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <libdir> <private_libdir>"
    exit 1
fi

libdir="$1"
private_libdir="$2"

if [ ! -f "$private_libdir/libjulia-internal.so" ]; then
    echo "ERROR: Could not open $private_libdir/libjulia-internal.so" >&2
    exit 2
fi

find_shlib ()
{
    if [ -f "$1" ]; then
        ldd "$1" | grep $2 | cut -d' ' -f3 | xargs
    fi
}

# Discover libstdc++ location and name
LIBSTD=$(find_shlib "$private_libdir/libjulia-internal.so" "libstdc++.so")
LIBSTD_NAME=$(basename $LIBSTD)
LIBSTD_DIR=$(dirname $LIBSTD)

if [ ! -f "$private_libdir/$LIBSTD_NAME" ] && [ -f "$LIBSTD_DIR/$LIBSTD_NAME" ]; then
    cp -v "$LIBSTD_DIR/$LIBSTD_NAME" "$private_libdir"
    chmod 755 "$private_libdir/$LIBSTD_NAME"
fi
