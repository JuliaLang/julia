#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

# Script to compile Windows Julia, using binary dependencies from nightlies.
# Should work in MSYS assuming 7zip is installed and on the path,
# or Cygwin or Linux assuming make, curl, p7zip, and mingw64-$ARCH-gcc-g++
# are installed

# Run in top-level Julia directory
cd `dirname "$0"`/../..
# Stop on error
set -e
# Make sure stdin exists (not true on appveyor msys2)
exec < /dev/null

curlflags="curl --retry 10 -k -L -y 5"
checksum_download() {
  # checksum_download filename url
  f=$1
  url=$2
  if [ -e "$f" ]; then
    deps/tools/jlchecksum "$f" 2> /dev/null && return
    echo "Checksum for '$f' changed, download again." >&2
  fi
  echo "Downloading '$f'"
  $curlflags -O "$url"
  deps/tools/jlchecksum "$f"
}

# If ARCH environment variable not set, choose based on uname -m
if [ -z "$ARCH" -a -z "$XC_HOST" ]; then
  export ARCH=`uname -m`
elif [ -z "$ARCH" ]; then
  ARCH=`echo $XC_HOST | sed 's/-w64-mingw32//'`
fi

echo "" > Make.user
echo "" > get-deps.log
# set MARCH for consistency with how binaries get built
if [ "$ARCH" = x86_64 ]; then
  bits=64
  archsuffix=64
  exc=seh
  echo "override MARCH = x86-64" >> Make.user
  echo 'USE_BLAS64 = 1' >> Make.user
  echo 'LIBBLAS = -L$(JULIAHOME)/usr/bin -lopenblas64_' >> Make.user
  echo 'LIBBLASNAME = libopenblas64_' >> Make.user
else
  bits=32
  archsuffix=86
  exc=sjlj
  echo "override MARCH = pentium4" >> Make.user
  echo 'LIBBLAS = -L$(JULIAHOME)/usr/bin -lopenblas' >> Make.user
  echo 'LIBBLASNAME = libopenblas' >> Make.user
fi

# Set XC_HOST if in Cygwin or Linux
case $(uname) in
  CYGWIN*)
    if [ -z "$XC_HOST" ]; then
      XC_HOST="$ARCH-w64-mingw32"
      echo "XC_HOST = $XC_HOST" >> Make.user
    fi
    CROSS_COMPILE="$XC_HOST-"
    # Set BUILD_MACHINE and HOSTCC in case we don't have Cygwin gcc installed
    echo "override BUILD_MACHINE = $ARCH-pc-cygwin" >> Make.user
    if [ -z "`which gcc 2>/dev/null`" ]; then
      echo 'override HOSTCC = $(CROSS_COMPILE)gcc' >> Make.user
    fi
    make win-extras >> get-deps.log
    SEVENZIP="dist-extras/7z"
    ;;
  Linux)
    if [ -z "$XC_HOST" ]; then
      XC_HOST="$ARCH-w64-mingw32"
      echo "XC_HOST = $XC_HOST" >> Make.user
    fi
    CROSS_COMPILE="$XC_HOST-"
    make win-extras >> get-deps.log
    SEVENZIP="wine dist-extras/7z.exe"
    ;;
  *)
    CROSS_COMPILE=""
    SEVENZIP="7z"
    ;;
esac

# Download most recent Julia binary for dependencies
if ! [ -e julia-installer.exe ]; then
  f=julia-latest-win$bits.exe
  echo "Downloading $f"
  $curlflags -O https://s3.amazonaws.com/julianightlies/bin/winnt/x$archsuffix/$f
  echo "Extracting $f"
  $SEVENZIP x -y $f >> get-deps.log
fi
for i in bin/*.dll; do
  $SEVENZIP e -y julia-installer.exe "$i" \
    -ousr\\`dirname $i | sed -e 's|/julia||' -e 's|/|\\\\|g'` >> get-deps.log
done
for i in share/julia/base/pcre_h.jl; do
  $SEVENZIP e -y julia-installer.exe "$i" -obase >> get-deps.log
done
echo "override PCRE_INCL_PATH =" >> Make.user
# Remove libjulia.dll if it was copied from downloaded binary
rm -f usr/bin/libjulia.dll
rm -f usr/bin/libjulia-debug.dll
rm -f usr/bin/libgcc_s_s*-1.dll
rm -f usr/bin/libgfortran-3.dll
rm -f usr/bin/libquadmath-0.dll
rm -f usr/bin/libssp-0.dll
rm -f usr/bin/libstdc++-6.dll

if [ -z "$USEMSVC" ]; then
  if [ -z "`which ${CROSS_COMPILE}gcc 2>/dev/null`" -o -n "$APPVEYOR" ]; then
    f=$ARCH-4.9.2-release-win32-$exc-rt_v4-rev3.7z
    checksum_download \
        "$f" "https://bintray.com/artifact/download/tkelman/generic/$f"
    echo "Extracting $f"
    $SEVENZIP x -y $f >> get-deps.log
    export PATH=$PWD/mingw$bits/bin:$PATH
    # If there is a version of make.exe here, it is mingw32-make which won't work
    rm -f mingw$bits/bin/make.exe
  fi
  export AR=${CROSS_COMPILE}ar

  f=llvm-3.9.1-$ARCH-w64-mingw32-juliadeps-r04.7z
else
  echo "override USEMSVC = 1" >> Make.user
  echo "override ARCH = $ARCH" >> Make.user
  echo "override XC_HOST = " >> Make.user
  export CC="$PWD/deps/srccache/libuv/compile cl -nologo -MD -Z7"
  export AR="$PWD/deps/srccache/libuv/ar-lib lib"
  export LD="$PWD/linkld link"
  echo "override CC = $CC" >> Make.user
  echo 'override CXX = $(CC) -EHsc' >> Make.user
  echo "override AR = $AR" >> Make.user
  echo "override LD = $LD -DEBUG" >> Make.user

  f=llvm-3.3-$ARCH-msvc12-juliadeps.7z
fi

checksum_download \
    "$f" "https://bintray.com/artifact/download/tkelman/generic/$f"
echo "Extracting $f"
$SEVENZIP x -y $f >> get-deps.log

if [ -z "`which make 2>/dev/null`" ]; then
  if [ -n "`uname | grep CYGWIN`" ]; then
    echo "Install the Cygwin package for 'make' and try again."
    exit 1
  fi
  f="/make/make-3.81-2/make-3.81-2-msys-1.0.11-bin.tar"
  if ! [ -e `basename $f.lzma` ]; then
    echo "Downloading `basename $f`"
    $curlflags -O http://sourceforge.net/projects/mingw/files/MSYS/Base$f.lzma
  fi
  $SEVENZIP x -y `basename $f.lzma` >> get-deps.log
  tar -xf `basename $f`
  export PATH=$PWD/bin:$PATH
fi

if ! [ -e usr/bin/busybox.exe ]; then
  f=busybox-w32-FRP-875-gc6ec14a.exe
  echo "Downloading $f"
  $curlflags -o usr/bin/busybox.exe http://frippery.org/files/busybox/$f
fi

for lib in SUITESPARSE ARPACK BLAS LAPACK FFTW \
    GMP MPFR PCRE LIBUNWIND OPENSPECFUN; do
  echo "USE_SYSTEM_$lib = 1" >> Make.user
done
echo 'override LIBLAPACK = $(LIBBLAS)' >> Make.user
echo 'override LIBLAPACKNAME = $(LIBBLASNAME)' >> Make.user

# Remaining dependencies:
# libuv since its static lib is no longer included in the binaries
# openlibm since we need it as a static library to work properly
# utf8proc since its headers are not in the binary download
echo 'override DEP_LIBS = libuv utf8proc' >> Make.user

if [ -n "$USEMSVC" ]; then
  # Openlibm doesn't build well with MSVC right now
  echo 'USE_SYSTEM_OPENLIBM = 1' >> Make.user
  # Since we don't have a static library for openlibm
  echo 'override UNTRUSTED_SYSTEM_LIBM = 0' >> Make.user

  # Compile libuv and utf8proc without -TP first, then add -TP
  make -C deps install-libuv install-utf8proc
  cp usr/lib/uv.lib usr/lib/libuv.a
  echo 'override CC += -TP' >> Make.user
  echo 'override DEP_LIBS += dsfmt' >> Make.user

  # Create a modified version of compile for wrapping link
  sed -e 's/-link//' -e 's/cl/link/g' -e 's/ -Fe/ -OUT:/' \
    -e 's|$dir/$lib|$dir/lib$lib|g' deps/srccache/libuv/compile > linkld
  chmod +x linkld
else
  echo 'override DEP_LIBS += openlibm' >> Make.user
  make check-whitespace
  make VERBOSE=1 -C base version_git.jl.phony
  echo 'NO_GIT = 1' >> Make.user
fi
echo 'FORCE_ASSERTIONS = 1' >> Make.user

cat Make.user
make -j3 VERBOSE=1
make build-stats
#make debug
