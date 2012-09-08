# Run this script from $JULIAHOME

#! /bin/sh

cd ./usr/lib

for name in gcc_s.1 gfortran.3 quadmath.0; do
    cp /usr/local/lib/lib$name.dylib .
    install_name_tool -id @executable_path/../lib/lib$name.dylib lib$name.dylib
done

for name in quadmath.0 gfortran.3 openblas arpack amos; do
    install_name_tool -change /usr/local/lib/libgfortran.3.dylib @executable_path/../lib/libgfortran.3.dylib lib$name.dylib
    install_name_tool -change /usr/local/lib/libquadmath.0.dylib @executable_path/../lib/libquadmath.0.dylib lib$name.dylib
    install_name_tool -change /usr/local/lib/libgcc_s.1.dylib @executable_path/../lib/libgcc_s.1.dylib lib$name.dylib
done
