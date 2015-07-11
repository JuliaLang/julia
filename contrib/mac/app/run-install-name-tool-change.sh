#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

if [ $# -lt 3 ]; then
    echo "Usage: $0 library old_prefix new_prefix action"
    exit 1
fi

LIBRARY=$1
WRONG_PREFIX=$2
RIGHT_PREFIX="@executable_path/../$3"
ACTION=$4

if [ "x$ACTION" == "xchange" ]; then
    libs="`otool -L $LIBRARY 2>/dev/null | fgrep compatibility | cut -d\( -f1 | grep $WRONG_PREFIX | sort | uniq`"
    for lib in $libs; do
	if ! echo $lib | grep --silent "@executable_path" ; then
	    fixed=`echo $lib | sed -e s,\$WRONG_PREFIX,\$RIGHT_PREFIX,`
	    install_name_tool -change $lib $fixed $LIBRARY
	fi
    done;
elif [ "x$ACTION" == "xid" ]; then
    lib="`otool -D $LIBRARY 2>/dev/null | grep ^$WRONG_PREFIX`"
    install_name_tool -id "$RIGHT_PREFIX/$lib" $LIBRARY;
fi

