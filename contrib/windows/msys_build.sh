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
    printf "Checksum for '$f' changed, download again.\n" >&2
  fi
  printf "Downloading '$f'\n"
  $curlflags -O "$url"
  deps/tools/jlchecksum "$f"
}

# If ARCH environment variable not set, choose based on uname -m
if [ -z "$ARCH" -a -z "$XC_HOST" ]; then
  export ARCH=`uname -m`
elif [ -z "$ARCH" ]; then
  ARCH=`printf "$XC_HOST\n" | sed 's/-w64-mingw32//'`
fi

printf "\n" > Make.user
printf "\n" > get-deps.log
# set MARCH for consistency with how binaries get built
if [ "$ARCH" = x86_64 ]; then
  bits=64
  archsuffix=64
  exc=seh
  printf "override MARCH = x86-64\n" >> Make.user
  printf "USE_BLAS64 = 1\n" >> Make.user
  printf "LIBBLAS = -L$(JULIAHOME)/usr/bin -lopenblas64_\n" >> Make.user
  printf "LIBBLASNAME = libopenblas64_\n" >> Make.user
else
  bits=32
  archsuffix=86
  exc=sjlj
  printf "override MARCH = pentium4\n" >> Make.user
  printf "LIBBLAS = -L$(JULIAHOME)/usr/bin -lopenblas\n" >> Make.user
  printf "LIBBLASNAME = libopenblas\n" >> Make.user
fi

# Set XC_HOST if in Cygwin or Linux
case $(uname) in
  CYGWIN*)
    if [ -z "$XC_HOST" ]; then
      XC_HOST="$ARCH-w64-mingw32"
      printf "XC_HOST = $XC_HOST\n" >> Make.user
    fi
    CROSS_COMPILE="$XC_HOST-"
    # Set BUILD_MACHINE and HOSTCC in case we don't have Cygwin gcc installed
    printf "override BUILD_MACHINE = $ARCH-pc-cygwin\n" >> Make.user
    if [ -z "`which gcc 2>/dev/null`" ]; then
      printf "override HOSTCC = $(CROSS_COMPILE)gcc\n" >> Make.user
    fi
    make win-extras >> get-deps.log
    SEVENZIP="dist-extras/7z"
    ;;
  Linux)
    if [ -z "$XC_HOST" ]; then
      XC_HOST="$ARCH-w64-mingw32"
      printf "XC_HOST = $XC_HOST\n" >> Make.user
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
  printf "Downloading $f\n"
  $curlflags -O https://s3.amazonaws.com/julianightlies/bin/winnt/x$archsuffix/$f
  printf "Extracting $f\n"
  $SEVENZIP x -y $f >> get-deps.log
fi
for i in bin/*.dll; do
  $SEVENZIP e -y julia-installer.exe "$i" \
    -ousr\\`dirname $i | sed -e 's|/julia||' -e 's|/|\\\\|g'` >> get-deps.log
done
for i in share/julia/base/pcre_h.jl; do
  $SEVENZIP e -y julia-installer.exe "$i" -obase >> get-deps.log
done
printf "override PCRE_INCL_PATH =\n" >> Make.user
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
    printf "Extracting $f\n"
    $SEVENZIP x -y $f >> get-deps.log
    export PATH=$PWD/mingw$bits/bin:$PATH
    # If there is a version of make.exe here, it is mingw32-make which won't work
    rm -f mingw$bits/bin/make.exe
  fi
  export AR=${CROSS_COMPILE}ar

  f=llvm-3.7.1-$ARCH-w64-mingw32-juliadeps-r09.7z
else
  printf "override USEMSVC = 1\n" >> Make.user
  printf "override ARCH = $ARCH\n" >> Make.user
  printf "override XC_HOST = \n" >> Make.user
  export CC="$PWD/deps/srccache/libuv/compile cl -nologo -MD -Z7"
  export AR="$PWD/deps/srccache/libuv/ar-lib lib"
  export LD="$PWD/linkld link"
  printf "override CC = $CC\n" >> Make.user
  printf "override CXX = $(CC) -EHsc\n" >> Make.user
  printf "override AR = $AR\n" >> Make.user
  printf "override LD = $LD -DEBUG\n" >> Make.user

  f=llvm-3.3-$ARCH-msvc12-juliadeps.7z
fi

checksum_download \
    "$f" "https://bintray.com/artifact/download/tkelman/generic/$f"
printf "Extracting $f\n"
$SEVENZIP x -y $f >> get-deps.log
printf "override LLVM_CONFIG := $(JULIAHOME)/usr/bin/llvm-config.exe\n" >> Make.user
printf "override LLVM_SIZE := $(JULIAHOME)/usr/bin/llvm-size.exe\n" >> Make.user

if [ -z "`which make 2>/dev/null`" ]; then
  if [ -n "`uname | grep CYGWIN`" ]; then
    printf "Install the Cygwin package for 'make' and try again.\n"
    exit 1
  fi
  f="/make/make-3.81-2/make-3.81-2-msys-1.0.11-bin.tar"
  if ! [ -e `basename $f.lzma` ]; then
    printf "Downloading `basename $f`\n"
    $curlflags -O http://sourceforge.net/projects/mingw/files/MSYS/Base$f.lzma
  fi
  $SEVENZIP x -y `basename $f.lzma` >> get-deps.log
  tar -xf `basename $f`
  export PATH=$PWD/bin:$PATH
fi

if ! [ -e usr/bin/busybox.exe ]; then
  f=busybox-w32-FRP-483-g31277ab.exe
  printf "Downloading $f\n"
  $curlflags -o usr/bin/busybox.exe http://frippery.org/files/busybox/$f
fi

for lib in SUITESPARSE ARPACK BLAS LAPACK FFTW \
    GMP MPFR PCRE LIBUNWIND OPENSPECFUN; do
  printf "USE_SYSTEM_$lib = 1\n" >> Make.user
done
printf "override LIBLAPACK = $(LIBBLAS)\n" >> Make.user
printf "override LIBLAPACKNAME = $(LIBBLASNAME)\n" >> Make.user

# Remaining dependencies:
# libuv since its static lib is no longer included in the binaries
# openlibm since we need it as a static library to work properly
# utf8proc since its headers are not in the binary download
printf "override STAGE1_DEPS = libuv\n" >> Make.user
printf "override STAGE2_DEPS = utf8proc\n" >> Make.user
printf "override STAGE3_DEPS = \n" >> Make.user
printf "override STAGE4_DEPS = \n" >> Make.user

if [ -n "$USEMSVC" ]; then
  # Openlibm doesn't build well with MSVC right now
  printf "USE_SYSTEM_OPENLIBM = 1\n" >> Make.user
  # Since we don't have a static library for openlibm
  printf "override UNTRUSTED_SYSTEM_LIBM = 0\n" >> Make.user

  # Compile libuv and utf8proc without -TP first, then add -TP
  make -C deps install-libuv install-utf8proc
  cp usr/lib/uv.lib usr/lib/libuv.a
  printf "override CC += -TP\n" >> Make.user
  printf "override STAGE1_DEPS += dsfmt\n" >> Make.user

  # Create a modified version of compile for wrapping link
  sed -e 's/-link//' -e 's/cl/link/g' -e 's/ -Fe/ -OUT:/' \
    -e 's|$dir/$lib|$dir/lib$lib|g' deps/srccache/libuv/compile > linkld
  chmod +x linkld
else
  printf "override STAGE1_DEPS += openlibm\n" >> Make.user
  make check-whitespace
  make VERBOSE=1 -C base version_git.jl.phony
  printf "NO_GIT = 1\n" >> Make.user
fi
printf "FORCE_ASSERTIONS = 1\n" >> Make.user

cat Make.user
make -j3 VERBOSE=1
make build-stats
#make debug
