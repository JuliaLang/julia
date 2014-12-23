#!/bin/sh
# Script to compile Windows Julia, using binary dependencies from nightlies.
# Should work in MSYS assuming 7zip is installed and on the path,
# or Cygwin or Linux assuming make, curl, p7zip, and mingw64-$ARCH-gcc-g++
# are installed

# Run in top-level Julia directory
cd `dirname "$0"`/../..
# Stop on error
set -e

# Fail fast on AppVeyor if there are newer pending commits in this PR
curlflags="curl --retry 10 -k -L -y 5"
if [ -n "$APPVEYOR_PULL_REQUEST_NUMBER" ]; then
  # download a handy cli json parser
  if ! [ -e jq.exe ]; then
    $curlflags -O http://stedolan.github.io/jq/download/win64/jq.exe
  fi
  av_api_url="https://ci.appveyor.com/api/projects/StefanKarpinski/julia/history?recordsNumber=50"
  query=".builds | map(select(.pullRequestId == \"$APPVEYOR_PULL_REQUEST_NUMBER\"))[0].buildNumber"
  latestbuild="$(curl $av_api_url | ./jq "$query")"
  if [ -n "$latestbuild" -a "$latestbuild" != "null" -a "$latestbuild" != "$APPVEYOR_BUILD_NUMBER" ]; then
    echo "There are newer queued builds for this pull request, failing early."
    exit 1
  fi
fi

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
  echo "override MARCH = x86-64" >> Make.user
else
  bits=32
  archsuffix=86
  echo "override MARCH = i686" >> Make.user
  echo "override JULIA_CPU_TARGET = pentium4" >> Make.user
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
for i in bin/*.dll Git/bin/msys-1.0.dll Git/bin/msys-perl5_8.dll Git/bin/*.exe; do
  $SEVENZIP e -y julia-installer.exe "\$_OUTDIR/$i" \
    -ousr\\`dirname $i | sed -e 's|/julia||' -e 's|/|\\\\|g'` >> get-deps.log
done
for i in share/julia/base/pcre_h.jl; do
  $SEVENZIP e -y julia-installer.exe "\$_OUTDIR/$i" -obase >> get-deps.log
done
# Remove libjulia.dll if it was copied from downloaded binary
rm -f usr/bin/libjulia.dll
rm -f usr/bin/libjulia-debug.dll

mingw=http://sourceforge.net/projects/mingw
if [ -z "$USEMSVC" ]; then
  if [ -z "`which ${CROSS_COMPILE}gcc 2>/dev/null`" ]; then
    f=mingw-w$bits-bin-$ARCH-20140102.7z
    if ! [ -e $f ]; then
      echo "Downloading $f"
      $curlflags -O $mingw-w64-dgn/files/mingw-w64/$f
    fi
    echo "Extracting $f"
    $SEVENZIP x -y $f >> get-deps.log
    export PATH=$PATH:$PWD/mingw$bits/bin
    # If there is a version of make.exe here, it is mingw32-make which won't work
    rm -f mingw$bits/bin/make.exe
  fi
  export AR=${CROSS_COMPILE}ar

  f=llvm-3.3-$ARCH-w64-mingw32-juliadeps.7z
  # The MinGW binary version of LLVM doesn't include libgtest or libgtest_main
  mkdir -p usr/lib
  $AR cr usr/lib/libgtest.a
  $AR cr usr/lib/libgtest_main.a
else
  echo "override USEMSVC = 1" >> Make.user
  echo "override ARCH = $ARCH" >> Make.user
  echo "override XC_HOST = " >> Make.user
  export CC="$PWD/deps/libuv/compile cl -nologo -MD -Z7"
  export AR="$PWD/deps/libuv/ar-lib lib"
  export LD="$PWD/linkld link"
  echo "override CC = $CC" >> Make.user
  echo 'override CXX = $(CC) -EHsc' >> Make.user
  echo "override AR = $AR" >> Make.user
  echo "override LD = $LD -DEBUG" >> Make.user

  f=llvm-3.3-$ARCH-msvc12-juliadeps.7z
fi

if ! [ -e $f ]; then
  echo "Downloading $f"
  $curlflags -O http://sourceforge.net/projects/juliadeps-win/files/$f
fi
echo "Extracting $f"
$SEVENZIP x -y $f >> get-deps.log
echo 'LLVM_CONFIG = $(JULIAHOME)/usr/bin/llvm-config' >> Make.user

if [ -z "`which make 2>/dev/null`" ]; then
  if [ -n "`uname | grep CYGWIN`" ]; then
    echo "Install the Cygwin package for 'make' and try again."
    exit 1
  fi
  f="/make/make-3.81-2/make-3.81-2-msys-1.0.11-bin.tar"
  if ! [ -e `basename $f.lzma` ]; then
    echo "Downloading `basename $f`"
    $curlflags -O $mingw/files/MSYS/Base$f.lzma
  fi
  $SEVENZIP x -y `basename $f.lzma` >> get-deps.log
  tar -xf `basename $f`
  # msysgit has an ancient version of touch that fails with `touch -c nonexistent`
  cp usr/Git/bin/echo.exe bin/touch.exe
  export PATH=$PWD/bin:$PATH
fi

for lib in LLVM ARPACK BLAS LAPACK FFTW \
    GMP MPFR PCRE LIBUNWIND RMATH OPENSPECFUN; do
  echo "USE_SYSTEM_$lib = 1" >> Make.user
done
echo 'LIBBLAS = -L$(JULIAHOME)/usr/bin -lopenblas' >> Make.user
echo 'LIBBLASNAME = libopenblas' >> Make.user
echo 'override LIBLAPACK = $(LIBBLAS)' >> Make.user
echo 'override LIBLAPACKNAME = $(LIBBLASNAME)' >> Make.user

# Remaining dependencies:
# libuv since its static lib is no longer included in the binaries
# openlibm since we need it as a static library to work properly
# mojibake since its headers are not in the binary download
echo 'override STAGE1_DEPS = uv' >> Make.user
echo 'override STAGE2_DEPS = mojibake' >> Make.user
echo 'override STAGE3_DEPS = ' >> Make.user
make -C deps get-uv

if [ -n "$USEMSVC" ]; then
  # Create a modified version of compile for wrapping link
  sed -e 's/-link//' -e 's/cl/link/g' -e 's/ -Fe/ -OUT:/' \
    -e 's|$dir/$lib|$dir/lib$lib|g' deps/libuv/compile > linkld
  chmod +x linkld

  # Openlibm doesn't build well with MSVC right now
  echo 'USE_SYSTEM_OPENLIBM = 1' >> Make.user
  echo 'USE_SYSTEM_SUITESPARSE = 1' >> Make.user
  # Since we don't have a static library for openlibm
  echo 'override UNTRUSTED_SYSTEM_LIBM = 0' >> Make.user

  # Compile libuv and mojibake without -TP first, then add -TP
  make -C deps install-uv install-mojibake
  cp usr/lib/uv.lib usr/lib/libuv.a
  echo 'override CC += -TP' >> Make.user
else
  echo 'override STAGE1_DEPS += openlibm' >> Make.user
  echo 'override STAGE3_DEPS += suitesparse-wrapper' >> Make.user

  # hack so all of suitesparse doesn't rebuild
  echo 'override SUITESPARSE_VER = 4.4.1' >> Make.user
  make -C deps SuiteSparse-4.4.1/Makefile
  touch deps/SuiteSparse-4.4.1/UMFPACK/Lib/libumfpack.a
  touch usr/bin/libspqr.dll
fi

make -j2
#make debug
