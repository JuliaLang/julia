#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

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

    if [ -d "$DEST" ]; then
        DESTFILE="$DEST/$(basename "$SRC")"
    else
        DESTFILE="$DEST"
    fi

    # Do the chmod dance.
    # Symlinks to system libraries are ignored because Julia shouldn't modify
    # permission of those libraries. If we tried, some platforms would cause
    # an error.
    if [ ! -L "$DESTFILE" ]; then
        chmod $PERMS "$DESTFILE"
    fi
done

exit 0
