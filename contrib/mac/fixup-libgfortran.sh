#!/bin/sh
# Run as: fixup-libgfortran.sh <$private_libdir>

if [[ -z "$1" ]]; then
    echo "Usage: $0 <private_libdir>"
    exit -1
fi

private_libdir=$1

if [[ ! -f "$private_libdir/libarpack.dylib" ]]; then
    echo "ERR: Could not open $private_libdir/libarpack.dylib" >&2
    exit -1
fi

# First, discover all the places where libgfortran/libgcc is
for lib in arpack openlibm openspecfun lapack; do
    LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(otool -L "$private_libdir/lib$lib.dylib" | grep libgfortran | cut -d' ' -f1 | xargs) 2>/dev/null)"
    LIBGFORTRAN_DIRS="$LIBGFORTRAN_DIRS $(dirname $(otool -L "$private_libdir/lib$lib.dylib" | grep libgcc | cut -d' ' -f1 | xargs) 2>/dev/null)"
done

LIBGFORTRAN_DIRS=$(echo "$LIBGFORTRAN_DIRS"|tr " " "\n"| sort | uniq | grep -v '^$' | tr "\n" " ")
echo "Found traces of libgfortran/libgcc in $LIBGFORTRAN_DIRS"

for name in gcc_s.1 gfortran.3 quadmath.0; do
    for dir in $LIBGFORTRAN_DIRS; do
        if [[ ! -f "$private_libdir/lib$name.dylib" ]]; then
            cp "$dir/lib$name.dylib" "$private_libdir"
            chmod 755 "$private_libdir/lib$name.dylib"
            install_name_tool -id @rpath/lib$name.dylib "$private_libdir/lib$name.dylib"
        fi
    done
done


# Do the private_libdir libraries...
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
