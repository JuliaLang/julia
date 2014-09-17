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

    if [ -d "$DEST" ]; then
        DESTFILE="$DEST/$(basename "$SRC")"
    else
        DESTFILE="$DEST"
    fi

    # Do the chmod dance, and ignore errors on platforms that don't like setting permissions of symlinks
    # TODO: Test if it's a symlink instead of having to redirect stderr to /dev/null
    chmod $PERMS $DESTFILE 2>/dev/null
done

exit 0
