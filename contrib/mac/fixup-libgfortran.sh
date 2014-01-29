#!/bin/sh
# Run as: fixup-libgfortran.sh <$JL_PRIVATE_LIBDIR>
set +x

if [[ -z "$1" ]]; then
    echo "Usage: $0 <JL_PRIVATE_LIBDIR>"
    exit -1
fi

JL_PRIVATE_LIBDIR=$1

if [[ ! -f "$JL_PRIVATE_LIBDIR/libarpack.dylib" ]]; then
    echo "WARN: Could not open $JL_PRIVATE_LIBDIR/libarpack.dylib" >&2
fi

# First, discover where libgfortran is
LIBGFORTRAN_DIR=$(dirname $(otool -L $JL_PRIVATE_LIBDIR/libarpack.dylib | grep libgfortran | cut -d' ' -f1 | xargs))
echo "Found libgfortran in $LIBGFORTRAN_DIR"

for name in gcc_s.1 gfortran.3 quadmath.0; do
    cp $LIBGFORTRAN_DIR/lib$name.dylib $JL_PRIVATE_LIBDIR
    install_name_tool -id @rpath/lib$name.dylib $JL_PRIVATE_LIBDIR/lib$name.dylib
done


# Do the JL_PRIVATE_LIBDIR libraries...
cd $JL_PRIVATE_LIBDIR
for name in openlibm quadmath.0 gfortran.3 openblas arpack lapack openspecfun; do
    install_name_tool -change $LIBGFORTRAN_DIR/libgfortran.3.dylib @rpath/libgfortran.3.dylib lib$name*.dylib
    install_name_tool -change $LIBGFORTRAN_DIR/libquadmath.0.dylib @rpath/libquadmath.0.dylib lib$name*.dylib
    install_name_tool -change $LIBGFORTRAN_DIR/libgcc_s.1.dylib @rpath/libgcc_s.1.dylib lib$name*.dylib
done
