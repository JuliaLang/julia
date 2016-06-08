#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

# Usage: very similar to `install`
#   install.sh 755 src1 src2 ... dest

PERMS=$1
shift

ARGS=""
while [ $# -gt 1 ]; do
    ARGS="$ARGS $1"
    shift
done
DEST=$1

for SRC in $ARGS; do
    cp -a $SRC $DEST

    # Do the chmod dance, and ignore errors on platforms that don't like setting permissions of symlinks
    chmod $PERMS $DEST 2>/dev/null
done
