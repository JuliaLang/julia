#!/bin/sh
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
        elif [ "$UNAME" = "Darwin" ]; then
            otool -L "$private_libdir/lib$1.$SHLIB_EXT" | grep $2 | cut -d' ' -f1 | xargs
        fi
    fi
}

# First, discover all the places where libgfortran/libgcc is, as well as their true SONAMES
for lib in arpack openlibm openspecfun lapack; do
    if [ -f "$private_libdir/lib$lib.$SHLIB_EXT" ]; then
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(find_shlib $lib libgfortran) 2>/dev/null)"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(find_shlib $lib libgcc_s) 2>/dev/null)"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(find_shlib $lib libquadmath) 2>/dev/null)"
    fi
done

LIBGFORTRAN_DIRS=$(echo "$LIBGFORTRAN_DIRS" | tr " " "\n" | sort | uniq | grep -v '^$' | tr "\n" " ")

# If only we could agree on something
if [ "$UNAME" = "Linux" ]; then
    NAMEXTS="gcc_s.so.1 gfortran.so.3 quadmath.so.0"
elif [ "$UNAME" = "Darwin" ]; then
    NAMEXTS="gcc_s.1.dylib gfortran.3.dylib quadmath.0.dylib"
fi

for namext in $NAMEXTS; do
    for dir in $LIBGFORTRAN_DIRS; do
        if [ ! -f "$private_libdir/lib$namext" ] && [ -f "$dir/lib$namext" ]; then
            cp -v "$dir/lib$namext" "$private_libdir"
            chmod 755 "$private_libdir/lib$namext"
            if [ "$UNAME" = "Darwin" ]; then
                install_name_tool -id @rpath/lib$namext "$private_libdir/lib$namext"
            fi
        fi
    done
done

# Add possible internal directories to LIBGFORTRAN_DIRS
for lib in gfortran.3 quadmath.0 gcc_s.1 ; do
    if [ -f "$private_libdir/lib$lib.$SHLIB_EXT" ]; then
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(find_shlib $lib libgfortran) 2>/dev/null)"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(find_shlib $lib libgcc_s) 2>/dev/null)"
        LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(find_shlib $lib libquadmath) 2>/dev/null)"
    fi
done

LIBGFORTRAN_DIRS=$(echo "$LIBGFORTRAN_DIRS" | tr " " "\n" | sort | uniq | grep -v '^$' | tr "\n" " ")
echo "Found traces of libgfortran/libgcc in $LIBGFORTRAN_DIRS"


# Do the private_libdir libraries...
if [ "$UNAME" = "Darwin" ]; then
    cd $private_libdir
    for file in openlibm quadmath.0 gfortran.3 openblas arpack lapack openspecfun; do
        for dylib in $(ls lib$file*.dylib* 2>/dev/null); do
            for dir in $LIBGFORTRAN_DIRS; do
                install_name_tool -change "$dir/libgfortran.3.dylib" @rpath/libgfortran.3.dylib $dylib
                install_name_tool -change "$dir/libquadmath.0.dylib" @rpath/libquadmath.0.dylib $dylib
                install_name_tool -change "$dir/libgcc_s.1.dylib" @rpath/libgcc_s.1.dylib $dylib
            done
        done
    done
fi
