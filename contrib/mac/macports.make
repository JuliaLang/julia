#!/usr/bin/make -f

# Any Makefile variable can be set, but the following are the typical defaults
# required to build with a MacPorts installation

PREFIX=/opt/local
LLVM_CONFIG=${PREFIX}/bin/llvm-config-mp-3.2
PCRE_CONFIG=${PREFIX}/bin/pcre-config
CFLAGS='-02 -I${PREFIX}/include -I${PREFIX}/include/ufsparse'
CXXFLAGS='-02'
LDFLAGS='-L${PREFIX}/lib'
CC=/usr/bin/llvm-gcc-4.2
CXX=/usr/bin/llvm-g++-4.2
CPP=/usr/bin/llvm-cpp-4.2
FC=${PREFIX}/bin/gfortran-mp-4.5
USEGCC=1
USECLANG=0
SUITESPARSE_VER_MAJOR=4
JMAKEFLAGS = USE_SYSTEM_LLVM=1 LLVM_CONFIG=${LLVM_CONFIG} \
		USE_SYSTEM_PCRE=1 PCRE_CONFIG=${PCRE_CONFIG} \
		USE_SYSTEM_LIBM=1 \
		USE_SYSTEM_OPENLIBM=0 \
		USE_SYSTEM_BLAS=1 USE_BLAS64=0\
		USE_SYSTEM_LAPACK=1 \
		USE_SYSTEM_FFTW=1 \
		USE_SYSTEM_GMP=1 \
		USE_SYSTEM_MPFR=1 \
		USE_SYSTEM_ARPACK=1 \
		USE_SYSTEM_SUITESPARSE=1 \
		USE_SYSTEM_ZLIB=1 \
		USE_SYSTEM_GRISU=0 \
		USE_SYSTEM_LIBUV=0 \
		PREFIX=${PREFIX} CFLAGS=${CFLAGS} CXXFLAGS=${CXXFLAGS} LDFLAGS=${LDFLAGS} \
		CC=${CC} CXX=${CXX} CPP=${CPP} FC=${FC} USEGCC=${USEGCC} USECLANG=${USECLANG}

all: default
../../usr/lib/libspqr.dylib:
	$(MAKE) -C .. -f repackage_system_suitesparse${SUITESPARSE_VER_MAJOR}.make \
		USE_SYSTEM_BLAS=1 USE_SYSTEM_LAPACK=1 USE_BLAS64=0 \
		CFLAGS=${CFLAGS} CXXFLAGS=${CXXFLAGS} LDFLAGS=${LDFLAGS}

default test release debug: ../../usr/lib/libspqr.dylib
	$(MAKE) -C ../.. $(JMAKEFLAGS) $@

.PHONY: all default test release debug
