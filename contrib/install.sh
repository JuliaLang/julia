#!/bin/sh

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
    # Copy file, then take output of the form 'src' -> 'dest' and get only 'dest'
    DESTFILE=$(LC_ALL=C cp -va $SRC $DEST | sed -e $'s/ -> /\\\n/g' | tail -n 1)

    # If there are surrounding quotes, remove them.  We do this simply by knowing that the destination is always an absolute path
    if [ "$(echo $DESTFILE | head -c1)" != "/" ]; then
        DESTFILE=$(echo $DESTFILE | awk '{print substr($0, 2, length-2)}')
    fi

    # Do the chmod dance, and ignore errors on platforms that don't like setting permissions of symlinks
    chmod $PERMS $DESTFILE 2>/dev/null
done
