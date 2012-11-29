#!/bin/sh
# Run as: fixup-libgfortran.sh <$JL_LIBDIR> <$JL_PRIVATE_LIBDIR>

if [[ ! -f "$1/libopenblas.dylib" ]]; then
    echo "ERROR: Could not open $1/libopenblas.dylib" >&2
fi

# First, discover where libgfortran is
LIBGFORTRAN_DIR=$(dirname $(otool -L $1/libopenblas.dylib | grep libgfortran | cut -d' ' -f1 | xargs))
echo "Found libgfortran in $LIBGFORTRAN_DIR"

for name in gcc_s.1 gfortran.3 quadmath.0; do
    cp $LIBGFORTRAN_DIR/lib$name.dylib $1
    install_name_tool -id @rpath/lib$name.dylib $1/lib$name.dylib
done


# Do the JL_LIBDIR libraries...
cd $1
for name in quadmath.0 gfortran.3 openblas arpack; do
    install_name_tool -change $LIBGFORTRAN_DIR/libgfortran.3.dylib @rpath/libgfortran.3.dylib lib$name.dylib
    install_name_tool -change $LIBGFORTRAN_DIR/libquadmath.0.dylib @rpath/libquadmath.0.dylib lib$name.dylib
    install_name_tool -change $LIBGFORTRAN_DIR/libgcc_s.1.dylib @rpath/libgcc_s.1.dylib lib$name.dylib
done

# Go back to the root
cd - >/dev/null

# Next, do the JL_PRIVATE_LIBDIR libraries
cd $2
for name in openlibm; do
    install_name_tool -change $LIBGFORTRAN_DIR/libgfortran.3.dylib @rpath/libgfortran.3.dylib lib$name.dylib
    install_name_tool -change $LIBGFORTRAN_DIR/libquadmath.0.dylib @rpath/libquadmath.0.dylib lib$name.dylib
    install_name_tool -change $LIBGFORTRAN_DIR/libgcc_s.1.dylib @rpath/libgcc_s.1.dylib lib$name.dylib
done
