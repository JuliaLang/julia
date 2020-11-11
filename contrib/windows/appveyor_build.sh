#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Script to compile Windows Julia on Appveyor using a Cygwin host
# make sure you cd to the main julia directory beforing running
# uses environment variables defined in .appveyor.yml: MINGW_ARCH

# Stop on error
set -e

# Make sure stdin exists (apparently sometimes needed for mysys2?)
exec < /dev/null

export TERM=ansi  # make sure escape sequences print out properly on appveyor?

# set MARCH for consistency with how binaries get built
if [ "$MINGW_ARCH" = x86_64 ]; then
  echo "override MARCH = x86-64" >> Make.user
elif [ "$MINGW_ARCH" = i686 ]; then
  echo "override MARCH = pentium4" >> Make.user
fi
echo "override XC_HOST = $MINGW_ARCH-w64-mingw32" >> Make.user
echo "override JULIA_CPU_TARGET = generic;native" >> Make.user

echo 'USE_BINARYBUILDER = 1' >> Make.user
# This is pending the binaries actually being available
#echo 'BINARYBUILDER_LLVM_ASSERTS = 1' >> Make.user
echo 'FORCE_ASSERTIONS = 1' >> Make.user
echo 'USECCACHE = 1' >> Make.user
echo 'VERBOSE = 1' >> Make.user

cat Make.user
make check-whitespace
make -j3 release
make -j3 install
make JULIA=../../usr/bin/julia.exe BIN=. "$(make print-CC)" -C test/embedding release
make build-stats
ccache -s
