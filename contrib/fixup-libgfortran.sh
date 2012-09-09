# Run as: fixup-libgfortran.sh <$PREFIX/lib> <libgfortran location>

#! /bin/sh

cd $1

for name in gcc_s.1 gfortran.3 quadmath.0; do
    cp $2/lib$name.dylib .
    install_name_tool -id @rpath/lib$name.dylib lib$name.dylib
done

for name in quadmath.0 gfortran.3 openblas arpack amos; do
    install_name_tool -change $2/libgfortran.3.dylib @rpath/libgfortran.3.dylib lib$name.dylib
    install_name_tool -change $2/libquadmath.0.dylib @rpath/libquadmath.0.dylib lib$name.dylib
    install_name_tool -change $2/libgcc_s.1.dylib @rpath/libgcc_s.1.dylib lib$name.dylib
done
