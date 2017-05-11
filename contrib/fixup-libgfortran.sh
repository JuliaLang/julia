#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Run as: fixup-libgfortran.sh <$private_libdir>

if [ -z "$1" ]; then
    echo "Usage: $0 <private_libdir>"
    exit 1
fi

UNAME="$(uname -s)"
if [ "$UNAME" = "Linux" ]; then
    SHLIB_EXT="so"
elif [ "$UNAME" = "Darwin" ]; then
    SHLIB_EXT="dylib"
else
    echo "WARNING: Could not autodetect platform type ('uname -s' == $UNAME); assuming Linux"
    UNAME="Linux"
    SHLIB_EXT="so"
fi

private_libdir=$1

if [ ! -f "$private_libdir/libarpack.$SHLIB_EXT" ]; then
    echo "ERROR: Could not open $private_libdir/libarpack.$SHLIB_EXT" >&2
    exit 2
fi

find_shlib ()
{
    if [ -f "$private_libdir/lib$1.$SHLIB_EXT" ]; then
        if [ "$UNAME" = "Linux" ]; then
            ldd "$private_libdir/lib$1.$SHLIB_EXT" | grep $2 | cut -d' ' -f3 | xargs
        else # $UNAME is "Darwin", we only have two options, see above
            otool -L "$private_libdir/lib$1.$SHLIB_EXT" | grep $2 | cut -d' ' -f1 | xargs
        fi
    fi
}

# First, discover all the places where libgfortran/libgcc is, as well as their true SONAMES
for lib in arpack openspecfun lapack; do
    if [ -f "$private_libdir/lib$lib.$SHLIB_EXT" ]; then
        # Find the paths to the libraries we're interested in.  These are almost
        # always within the same directory, but we like to be general.
        LIBGFORTRAN_PATH=$(find_shlib $lib libgfortran)
        LIBGCC_PATH=$(find_shlib $lib libgcc_s)
        LIBQUADMATH_PATH=$(find_shlib $lib libquadmath)

        # Take the directories, add them onto LIBGFORTRAN_DIRS to search for things later
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $LIBGFORTRAN_PATH)"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $LIBGCC_PATH)"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $LIBQUADMATH_PATH)"

        # Save the SONAMES
        LIBGFORTRAN_SONAMES="$LIBGFORTRAN_SONAMES $(basename "$LIBGFORTRAN_PATH")"
        LIBGCC_SONAMES="$LIBGCC_SONAMES $(basename "$LIBGCC_PATH")"
        LIBQUADMATH_SONAMES="$LIBQUADMATH_SONAMES $(basename "$LIBQUADMATH_PATH")"
    fi
done

# Take in a list of space-separated tokens, return a deduplicated list of the same
uniquify()
{
    echo "$1" | tr " " "\n" | sort | uniq | grep -v '^$' | tr "\n" " "
}

LIBGFORTRAN_DIRS=$(uniquify "$LIBGFORTRAN_DIRS")
SONAMES="$(uniquify "$LIBGFORTRAN_SONAMES $LIBGCC_SONAMES $LIBQUADMATH_SONAMES")"

# Copy the SONAMEs we identified above into our private_libdir
for soname in $SONAMES; do
    for dir in $LIBGFORTRAN_DIRS; do
        if [ ! -f "$private_libdir/$soname" ] && [ -f "$dir/$soname" ]; then
            cp -v "$dir/$soname" "$private_libdir"
            chmod 755 "$private_libdir/$soname"
            if [ "$UNAME" = "Darwin" ]; then
                install_name_tool -id "@rpath/$soname" "$private_libdir/$soname"
            fi
        fi
    done

    # Add possible internal directories to LIBGFORTRAN_DIRS so that we can find all possible ways
    # That any of our libraries or these libraries we just copied in link to themselves later.
    if [ -f "$private_libdir/$soname" ]; then
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(find_shlib $lib libgfortran))"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(find_shlib $lib libgcc_s))"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(find_shlib $lib libquadmath))"
    fi
done


change_linkage()
{
    shlib_path="$1"
    old_link="$2"
    new_link="$3"
    if [ "$UNAME" = "Darwin" ]; then
        install_name_tool -change "$old_link" "$new_link" "$shlib_path"
    else # $UNAME is "Linux", we only have two options, see above
        patchelf --set-rpath \$ORIGIN "$shlib_path"
    fi
}

LIBGFORTRAN_DIRS=$(uniquify "$LIBGFORTRAN_DIRS")
echo "Found traces of libgfortran/libgcc in $LIBGFORTRAN_DIRS"

# For every library that remotely touches libgfortran stuff (so the libraries
# we have copied in ourselves as well as arpack, openspecfun, etc...) we must
# update the linkage to point to @rpath (on OSX) or $ORIGIN (on Linux) so
# that direct links to the old libgfortran directories are instead directed
# to the proper location, which is our $private_libdir.
cd $private_libdir

# Iterate over possible library names
for soname in libopenblas libarpack libcholmod liblapack libopenspecfun $SONAMES; do
    # Grab every incarnation of that library that exists within $private_libdir
    # (e.g. "libopenspecfun.so", and "libopenspecfun.so.0", etc...)
    for lib in $private_libdir/$soname*; do
        # Look for links to any of the our three musketeers within ANY of the
        # potential LIBGFORTRAN_DIRS we've discovered so far
        for dir in $LIBGFORTRAN_DIRS; do
            change_linkage "$lib" "$dir/$soname" "@rpath/$soname"
        done
    done
done

