#!/bin/sh
# Run as: fixup-libgfortran.sh <$private_libdir>
set +x

if [[ -z "$1" ]]; then
    echo "Usage: $0 <private_libdir>"
    exit -1
fi

private_libdir=$1

if [[ ! -f "$private_libdir/libarpack.dylib" ]]; then
    echo "ERR: Could not open $private_libdir/libarpack.dylib" >&2
    exit -1
fi

# First, discover where libgfortran is
LIBGFORTRAN_DIR=$(dirname $(otool -L $private_libdir/libarpack.dylib | grep libgfortran | cut -d' ' -f1 | xargs))
echo "Found libgfortran in $LIBGFORTRAN_DIR"

for name in gcc_s.1 gfortran.3 quadmath.0; do
    cp $LIBGFORTRAN_DIR/lib$name.dylib $private_libdir
    install_name_tool -id @rpath/lib$name.dylib $private_libdir/lib$name.dylib
done


# Do the private_libdir libraries...
cd $private_libdir
for file in openlibm quadmath.0 gfortran.3 openblas arpack lapack openspecfun; do
	for dylib in $(ls lib$file*.dylib 2>/dev/null); do
    	install_name_tool -change $LIBGFORTRAN_DIR/libgfortran.3.dylib @rpath/libgfortran.3.dylib $dylib
    	install_name_tool -change $LIBGFORTRAN_DIR/libquadmath.0.dylib @rpath/libquadmath.0.dylib $dylib
    	install_name_tool -change $LIBGFORTRAN_DIR/libgcc_s.1.dylib @rpath/libgcc_s.1.dylib $dylib
    done
done
