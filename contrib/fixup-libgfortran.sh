#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Run as: fixup-libgfortran.sh [--verbose] <$private_libdir>

# If we're invoked with "--verbose", create a `debug` function that prints stuff out
if [ "$1" = "--verbose" ] || [ "$1" = "-v" ]; then
shift 1
debug() { echo "$*"; }
else
debug() { :; }
fi

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
    echo "WARNING: Could not autodetect platform type ('uname -s' == $UNAME); assuming Linux" >&2
    UNAME="Linux"
    SHLIB_EXT="so"
fi

private_libdir=$1

find_shlib()
{
    lib_path="$1"
    if [ -f "$lib_path" ]; then
        if [ "$UNAME" = "Linux" ]; then
            ldd "$lib_path" | grep $2 | cut -d' ' -f3 | xargs
        else # $UNAME is "Darwin", we only have two options, see above
            otool -L "$lib_path" | grep $2 | cut -d' ' -f1 | xargs
        fi
    fi
}

# First, discover all the places where libgfortran/libgcc is, as well as their true SONAMES
for lib in lapack blas openblas; do
    for private_libname in ${private_libdir}/lib$lib*.$SHLIB_EXT*; do
        # Find the paths to the libraries we're interested in.  These are almost
        # always within the same directory, but we like to be general.
        LIBGFORTRAN_PATH=$(find_shlib "$private_libname" libgfortran)
        LIBGCC_PATH=$(find_shlib "$private_libname" libgcc_s)
        LIBQUADMATH_PATH=$(find_shlib "$private_libname" libquadmath)

        # Take the directories, add them onto LIBGFORTRAN_DIRS, which we use to
        # search for these libraries in the future.
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $LIBGFORTRAN_PATH 2>/dev/null)"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $LIBGCC_PATH 2>/dev/null)"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $LIBQUADMATH_PATH 2>/dev/null)"

        # Save the SONAMES
        LIBGFORTRAN_SONAMES="$LIBGFORTRAN_SONAMES $(basename "$LIBGFORTRAN_PATH")"
        LIBGCC_SONAMES="$LIBGCC_SONAMES $(basename "$LIBGCC_PATH")"
        LIBQUADMATH_SONAMES="$LIBQUADMATH_SONAMES $(basename "$LIBQUADMATH_PATH")"
    done
done

# Take in a list of space-separated tokens, return a deduplicated list of the same
uniquify()
{
    echo "$1" | tr " " "\n" | sort | uniq | grep -v '^$' | tr "\n" " "
}

LIBGFORTRAN_DIRS=$(uniquify "$LIBGFORTRAN_DIRS")
SONAMES="$(uniquify "$LIBGFORTRAN_SONAMES $LIBGCC_SONAMES $LIBQUADMATH_SONAMES")"
debug "Discovered traces of libgfortran within $LIBGFORTRAN_DIRS"
debug "Got SONAMES of $SONAMES"

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
done

# On OSX, we need to change the old link (which is usually a full path)
# to point to `@rpath/${soname}` explicitly, so we use `find_shlib()`
# to dynamically find the full path we want to change.  On Linux, we
# don't care about full paths, we just set the rpath to `$ORIGIN`.
change_linkage()
{
    # This is the path of the library we want to edit
    lib_path="$1"

    # If it doesn't exist, exit quietly
    if [ ! -f "$lib_path" ]; then
        debug "  $lib_path doesn't exist, skipping"
        return
    fi

    # This is the soname of the dependency we want to swap out
    soname="$2"

    if [ "$UNAME" = "Darwin" ]; then
        old_link=$(find_shlib "$lib_path" "$soname")
        echo " $old_link"
        install_name_tool -change "$old_link" "@rpath/$soname" "$lib_path"
    else # $UNAME is "Linux", we only have two options, see above
        patchelf --set-rpath \$ORIGIN "$lib_path"
    fi
}

# For every library that remotely touches libgfortran stuff (the libraries we
# have copied in ourselves) we must
# update the linkage to point to @rpath (on OSX) or $ORIGIN (on Linux) so
# that direct links to the old libgfortran directories are instead directed
# to the proper location, which is our $private_libdir.
for lib in libopenblas libcholmod liblapack $SONAMES; do
    # Grab every incarnation of that library that exists within $private_libdir
    # (e.g. "libopenblas.so", and "libopenblas.so.0", etc...)
    for lib_path in $private_libdir/$lib*; do
        # Iterate over dependency names that need to be changed
        for soname in $SONAMES; do
            debug "changing linkage of $lib_path to $soname"
            change_linkage "$lib_path" "$soname"
        done
    done
done

