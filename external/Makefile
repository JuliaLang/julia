## library versions ##

LLVM_VER = 3.0
READLINE_VER = 6.2
PCRE_VER = 8.21
GRISU_VER = 1.1
DSFMT_VER = 2.1
OPENBLAS_VER = v0.1.0
LAPACK_VER = 3.4.0
ARPACK_VER = 3.0.2
FFTW_VER = 3.3
SUITESPARSE_VER = 3.7.0
CLP_VER = 1.14.5
UNWIND_VER = 1.0.1
LIGHTTPD_VER = 1.4.29
GMP_VER=5.0.4

## high-level setup ##

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

OS = $(shell uname)
ARCH = $(shell uname -m)
LIBS = fdlibm dsfmt double-conversion amos suitesparse-wrapper gmp-wrapper

ifeq ($(OS), Linux)
LIBS += unwind
endif

ifeq ($(OS), FreeBSD)
LIBS += unwind
endif

ifeq ($(USE_SYSTEM_LLVM), 0)
LIBS += llvm
endif

ifeq ($(USE_SYSTEM_READLINE), 0)
LIBS += readline
endif

ifeq ($(USE_SYSTEM_PCRE), 0)
LIBS += pcre
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
LIBS += openblas
endif

ifeq ($(USE_SYSTEM_LAPACK), 0)
LIBS += lapack
endif

ifeq ($(USE_SYSTEM_FFTW), 0)
LIBS += fftw
endif

ifeq ($(USE_SYSTEM_GMP), 0)
LIBS += gmp
endif

ifeq ($(USE_SYSTEM_ARPACK), 0)
LIBS += arpack
endif

ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
LIBS += suitesparse
endif

WGET = wget
WGET_DASH_O = wget -O
ifeq ($(OS), Darwin)
WGET = curl -kLO
WGET_DASH_O = curl -kLo
endif

ifeq ($(OS), FreeBSD)
WGET = curl -LO
WGET_DASH_O = curl -Lo
endif

default: install
compile: $(addprefix compile-, $(LIBS))
install: $(addprefix install-, $(LIBS))
cleanall: $(addprefix clean-, $(LIBS))
distclean: $(addprefix distclean-, $(LIBS))
	rm -rf root

## LLVM ##

LLVM_OBJ_TARGET = $(EXTROOTLIB)/libLLVM-$(LLVM_VER).$(SHLIB_EXT)
LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/Release/lib/libLLVM-$(LLVM_VER).$(SHLIB_EXT)

compile-llvm: $(LLVM_OBJ_SOURCE)
install-llvm: $(LLVM_OBJ_TARGET)

llvm-$(LLVM_VER).tar.gz:
	$(WGET) http://llvm.org/releases/$(LLVM_VER)/$@
llvm-$(LLVM_VER)/configure: llvm-$(LLVM_VER).tar.gz
	mkdir -p llvm-$(LLVM_VER) && \
	tar -C llvm-$(LLVM_VER) --strip-components 1 -xf $<
	touch $@
$(LLVM_OBJ_SOURCE): llvm-$(LLVM_VER)/configure
	cd llvm-$(LLVM_VER) && \
	./configure --prefix=$(abspath $(EXTROOT)) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 --disable-bindings --disable-docs CC=gcc CXX=g++ && \
	$(MAKE) $(jPARALLEL_BUILD_JOBS)
$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE)
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C llvm-$(LLVM_VER) install
	touch $@

clean-llvm:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C llvm-$(LLVM_VER) clean
	rm -f $(LLVM_OBJ_TARGET)
distclean-llvm:
	rm -rf llvm-$(LLVM_VER).tar.gz llvm-$(LLVM_VER)

## GNU readline ##

READLINE_OBJ_TARGET = $(EXTROOTLIB)/libreadline.$(SHLIB_EXT)
READLINE_OBJ_SOURCE = readline-$(READLINE_VER)/shlib/libreadline.$(READLINE_VER).$(SHLIB_EXT)

compile-readline: $(READLINE_OBJ_SOURCE)
install-readline: $(READLINE_OBJ_TARGET)

readline-$(READLINE_VER).tar.gz:
	$(WGET) ftp://ftp.gnu.org/gnu/readline/$@
readline-$(READLINE_VER)/configure: readline-$(READLINE_VER).tar.gz
	tar zxf $<
	touch $@
$(READLINE_OBJ_SOURCE): readline-$(READLINE_VER)/configure
	cd readline-$(READLINE_VER) && \
	./configure --prefix=$(abspath $(EXTROOT)) --disable-shared --enable-static --with-curses && \
	$(MAKE) $(jPARALLEL_BUILD_JOBS)
	touch $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C readline-$(READLINE_VER) install
	touch $@

clean-readline:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C readline-$(READLINE_VER) clean
	rm -f $(READLINE_OBJ_TARGET)
distclean-readline:
	rm -rf readline-$(READLINE_VER).tar.gz readline-$(READLINE_VER)

## PCRE ##

PCRE_OBJ_TARGET = $(EXTROOTLIB)/libpcre.$(SHLIB_EXT)

compile-pcre: install-pcre
install-pcre: $(PCRE_OBJ_TARGET)

pcre-$(PCRE_VER).tar.bz2:
	$(WGET) ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/$@
pcre-$(PCRE_VER)/configure: pcre-$(PCRE_VER).tar.bz2
	tar jxf $<
	touch $@
pcre-$(PCRE_VER)/config.status: pcre-$(PCRE_VER)/configure
	cd pcre-$(PCRE_VER) && \
	./configure --prefix=$(abspath $(EXTROOT)) --enable-utf8 --enable-unicode-properties --enable-jit
$(PCRE_OBJ_TARGET): pcre-$(PCRE_VER)/config.status
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C pcre-$(PCRE_VER) install
	touch $@

clean-pcre:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C pcre-$(PCRE_VER) clean
	rm -f $(PCRE_OBJ_TARGET)
distclean-pcre:
	rm -rf pcre-$(PCRE_VER).tar.bz2 pcre-$(PCRE_VER)

## Grisu floating-point printing library ##

GRISU_OPTS = -fPIC -O3 -fvisibility=hidden

compile-double-conversion: double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
install-double-conversion: $(EXTROOTLIB)/libgrisu.$(SHLIB_EXT)

double-conversion-$(GRISU_VER).tar.gz:
	$(WGET) http://double-conversion.googlecode.com/files/double-conversion-$(GRISU_VER).tar.gz
double-conversion-$(GRISU_VER)/Makefile: double-conversion-$(GRISU_VER).tar.gz
	mkdir -p double-conversion-$(GRISU_VER) && \
	tar -C double-conversion-$(GRISU_VER) --strip-components 1 -xf double-conversion-$(GRISU_VER).tar.gz
	touch $@
double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT): double-conversion-$(GRISU_VER)/Makefile
	cd double-conversion-$(GRISU_VER) && \
	$(CXX) -c $(GRISU_OPTS) -o src/bignum.o -Isrc src/bignum.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/bignum-dtoa.o -Isrc src/bignum-dtoa.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/cached-powers.o -Isrc src/cached-powers.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/diy-fp.o -Isrc src/diy-fp.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/double-conversion.o -Isrc src/double-conversion.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/fast-dtoa.o -Isrc src/fast-dtoa.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/fixed-dtoa.o -Isrc src/fixed-dtoa.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/strtod.o -Isrc src/strtod.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/libdouble-conversion.o -I.. -Isrc ../double_conversion_wrapper.cpp && \
	$(CXX) $(GRISU_OPTS) src/*.o -shared -dead_strip -o src/libgrisu.$(SHLIB_EXT)
$(EXTROOTLIB)/libgrisu.$(SHLIB_EXT): double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
	mkdir -p $(EXTROOTLIB)
	cp -f $< $@

clean-double-conversion:
	rm -f double-conversion-$(GRISU_VER)/src/*.o
	rm -f double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
distclean-double-conversion:
	rm -rf double-conversion-$(GRISU_VER).tar.gz double-conversion-$(GRISU_VER)

## fdlibm ##

FDLIBM_OBJ_TARGET = $(EXTROOTLIB)/libfdm.$(SHLIB_EXT)
FDLIBM_OBJ_SOURCE = fdlibm/libfdm.$(SHLIB_EXT)

compile-fdlibm: $(FDLIBM_OBJ_SOURCE)
install-fdlibm: $(FDLIBM_OBJ_TARGET)

$(FDLIBM_OBJ_SOURCE): fdlibm/*.c
	cd fdlibm && \
	$(MAKE) $(jPARALLEL_BUILD_JOBS) CC=$(CC) CFLAGS="$(CFLAGS) -D_IEEE_LIBM -Dx86 -fPIC -O2 $(CONFIG)" && \
	$(CC) -shared *.o -o libfdm.$(SHLIB_EXT)
$(FDLIBM_OBJ_TARGET): $(FDLIBM_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-fdlibm:
	cd fdlibm && $(MAKE) $(jPARALLEL_BUILD_JOBS) clean
	rm -f $(FDLIBM_OBJ_SOURCE) $(FDLIBM_OBJ_TARGET)
distclean-fdlibm: clean-fdlibm

## rmath ##

RMATH_OBJ_TARGET = $(EXTROOTLIB)/libRmath.$(SHLIB_EXT)
RMATH_OBJ_SOURCE = Rmath/src/libRmath.$(SHLIB_EXT)

compile-rmath: $(RMATH_OBJ_SOURCE)
install-rmath: $(RMATH_OBJ_TARGET)

$(RMATH_OBJ_SOURCE): Rmath/src/Makefile
	cd Rmath/src && \
	$(MAKE) $(jPARALLEL_BUILD_JOBS) CC=$(CC)
$(RMATH_OBJ_TARGET): $(RMATH_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-rmath:
	cd Rmath/src && $(MAKE) $(jPARALLEL_BUILD_JOBS) clean
	rm -f $(RMATH_OBJ_SOURCE) $(RMATH_OBJ_TARGET)
distclean-rmath: clean-rmath

## amos ##

AMOS_OBJ_TARGET = $(EXTROOTLIB)/libamos.$(SHLIB_EXT)
AMOS_OBJ_SOURCE = amos/libamos.$(SHLIB_EXT)

compile-amos: $(AMOS_OBJ_SOURCE)
install-amos: $(AMOS_OBJ_TARGET)

$(AMOS_OBJ_SOURCE): amos/*.f
	cd amos && \
	$(MAKE) $(jPARALLEL_BUILD_JOBS) && \
	$(FC) $(FFLAGS) $(JFFLAGS) -shared *.o -o libamos.$(SHLIB_EXT)
$(AMOS_OBJ_TARGET): $(AMOS_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-amos:
	cd amos && $(MAKE) $(jPARALLEL_BUILD_JOBS) clean
	rm -f $(AMOS_OBJ_SOURCE) $(AMOS_OBJ_TARGET)
distclean-amos: clean-amos

## DSFMT ##

DSFMT_OBJ_TARGET = $(EXTROOTLIB)/librandom.$(SHLIB_EXT)
DSFMT_OBJ_SOURCE = random/librandom.$(SHLIB_EXT)

compile-dsfmt: $(DSFMT_OBJ_SOURCE)
install-dsfmt: $(DSFMT_OBJ_TARGET)

random/dsfmt-$(DSFMT_VER).tar.gz:
	$(WGET_DASH_O) $@ http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz 
	touch $@
random/jl_random.c: random/dsfmt-$(DSFMT_VER).tar.gz
	cd random && \
	mkdir -p dsfmt-$(DSFMT_VER) && \
	tar -C dsfmt-$(DSFMT_VER) --strip-components 1 -xf dsfmt-$(DSFMT_VER).tar.gz && \
	cd dsfmt-$(DSFMT_VER) && patch < ../dSFMT.h.patch
	touch $@
$(DSFMT_OBJ_SOURCE): random/jl_random.c
	cd random && \
	$(CC) $(CFLAGS) -O3 -finline-functions -fomit-frame-pointer -DNDEBUG -fno-strict-aliasing --param max-inline-insns-single=1800  -Wmissing-prototypes -Wall  -std=c99 -DDSFMT_MEXP=19937 -fPIC -shared -DDSFMT_DO_NOT_USE_OLD_NAMES jl_random.c -o librandom.$(SHLIB_EXT)

$(DSFMT_OBJ_TARGET): $(DSFMT_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-dsfmt:
	rm -f random/librandom.$(SHLIB_EXT)
distclean-dsfmt: clean-dsfmt
	cd random && rm -rf *.tar.gz dsfmt-$(DSFMT_VER)

## OpenBLAS ##

ifeq ($(USE_SYSTEM_BLAS), 0)
OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.a
else
OPENBLAS_OBJ_SOURCE =
endif

compile-openblas: $(OPENBLAS_OBJ_SOURCE)
install-openblas: compile-openblas

openblas-$(OPENBLAS_VER).tar.gz:
	$(WGET_DASH_O) $@ https://github.com/xianyi/OpenBLAS/tarball/$(OPENBLAS_VER) 
openblas-$(OPENBLAS_VER)/Makefile: openblas-$(OPENBLAS_VER).tar.gz
	mkdir -p openblas-$(OPENBLAS_VER) && \
	tar -C openblas-$(OPENBLAS_VER) --strip-components 1 -xf $<
	perl -i -ple 's/^\s*(EXTRALIB\s*\+=\s*-lSystemStubs)\s*$$/# $$1/g' $@.system
	touch $@
$(OPENBLAS_OBJ_SOURCE): openblas-$(OPENBLAS_VER)/Makefile
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C openblas-$(OPENBLAS_VER) DYNAMIC_ARCH=1 USE_THREAD=0 NO_LAPACK=1 CC=$(CC) FC=$(FC)  FFLAGS="$(FFLAGS) $(JFFLAGS)" TARGET=$(TARGET_OPENBLAS_ARCH)
	cp -f openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT) $(EXTROOTLIB)

clean-openblas:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C openblas-$(OPENBLAS_VER) clean
distclean-openblas:
	rm -rf openblas-$(OPENBLAS_VER).tar.gz openblas-$(OPENBLAS_VER)

## LAPACK ##

ifeq ($(USE_SYSTEM_LAPACK), 0)
LAPACK_OBJ_TARGET = $(EXTROOTLIB)/liblapack.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE = lapack-$(LAPACK_VER)/liblapack.$(SHLIB_EXT)
else
LAPACK_OBJ_TARGET =
LAPACK_OBJ_SOURCE =
endif

compile-lapack: $(LAPACK_OBJ_SOURCE)
install-lapack: $(LAPACK_OBJ_TARGET)

lapack-$(LAPACK_VER).tgz:
	$(WGET) http://www.netlib.org/lapack/$@
lapack-$(LAPACK_VER)/Makefile: lapack-$(LAPACK_VER).tgz
	tar zxf $<
	touch $@
lapack-$(LAPACK_VER)/INSTALL/dlamch.o: lapack-$(LAPACK_VER)/Makefile
	cd lapack-$(LAPACK_VER) && \
	cp INSTALL/make.inc.gfortran ./make.inc && \
	$(MAKE) $(jPARALLEL_BUILD_JOBS) lapacklib NOOPT="-O0 -fPIC" OPTS="$(FFLAGS) $(JFFLAGS)" FORTRAN=$(FC) LOADER=$(FC)
$(LAPACK_OBJ_SOURCE): lapack-$(LAPACK_VER)/INSTALL/dlamch.o $(OPENBLAS_OBJ_SOURCE)
	cd lapack-$(LAPACK_VER) && \
	$(FC) -shared $(FFLAGS) $(JFFLAGS) SRC/*.o INSTALL/dlamch.o INSTALL/dsecnd_INT_ETIME.o INSTALL/ilaver.o INSTALL/slamch.o $(LIBBLAS) -o liblapack.$(SHLIB_EXT)
$(LAPACK_OBJ_TARGET): $(LAPACK_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-lapack:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C lapack-$(LAPACK_VER) clean
	rm -f $(LAPACK_OBJ_SOURCE) $(LAPACK_OBJ_TARGET)
distclean-lapack:
	rm -rf lapack-$(LAPACK_VER).tgz lapack-$(LAPACK_VER)

## ARPACK ##

ARPACK_OBJ_TARGET = $(EXTROOTLIB)/libarpack.$(SHLIB_EXT)
ARPACK_OBJ_SOURCE = arpack-$(ARPACK_VER)/libarpack.$(SHLIB_EXT)

compile-arpack: $(ARPACK_OBJ_SOURCE)
install-arpack: $(ARPACK_OBJ_TARGET)

arpack-ng-$(ARPACK_VER).tar.gz:
	$(WGET_DASH_O) $@ http://forge.scilab.org/index.php/p/arpack-ng/downloads/353/get/
	touch $@
arpack-ng-$(ARPACK_VER)/configure: arpack-ng-$(ARPACK_VER).tar.gz
	tar zxf $<
	touch $@
$(ARPACK_OBJ_TARGET): arpack-ng-$(ARPACK_VER)/configure $(LAPACK_OBJ_TARGET) $(OPENBLAS_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cd arpack-ng-$(ARPACK_VER) && \
	rm -f UTIL/second_NONE.f && \
	$(FC) -shared $(FFLAGS) $(JFFLAGS) SRC/*.f UTIL/*.f $(LIBLAPACK) $(LIBBLAS) -o libarpack.$(SHLIB_EXT) && \
	cp libarpack.$(SHLIB_EXT) $(EXTROOTLIB)
	touch $@

clean-arpack:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C arpack-ng-$(ARPACK_VER) clean
	rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	rm -rf arpack-ng-$(ARPACK_VER).tar.gz arpack-ng-$(ARPACK_VER)

## FFTW ##

FFTW_SINGLE_OBJ_TARGET = $(EXTROOTLIB)/libfftw3f.3.$(SHLIB_EXT)
FFTW_DOUBLE_OBJ_TARGET = $(EXTROOTLIB)/libfftw3.3.$(SHLIB_EXT)

compile-fftw: compile-fftw-single compile-fftw-double
compile-fftw-single: install-fftw-single
compile-fftw-double: install-fftw-double

install-fftw: install-fftw-single install-fftw-double
install-fftw-single: $(FFTW_SINGLE_OBJ_TARGET)
install-fftw-double: $(FFTW_DOUBLE_OBJ_TARGET)

FFTW_CONFIG = --enable-shared --disable-fortran --disable-mpi --disable-openmp --disable-threads --enable-sse2 --enable-fma

fftw-$(FFTW_VER).tar.gz:
	$(WGET) http://www.fftw.org/$@

fftw-$(FFTW_VER)-single/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-single && \
	tar -C fftw-$(FFTW_VER)-single --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-single/config.status: fftw-$(FFTW_VER)-single/configure
	cd fftw-$(FFTW_VER)-single && \
	./configure --prefix=$(abspath $(EXTROOT)) $(FFTW_CONFIG) --enable-sse --enable-float && \
	$(MAKE) $(jPARALLEL_BUILD_JOBS) clean
	touch $@
$(FFTW_SINGLE_OBJ_TARGET): fftw-$(FFTW_VER)-single/config.status
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C fftw-$(FFTW_VER)-single install
	touch $@

fftw-$(FFTW_VER)-double/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-double && \
	tar -C fftw-$(FFTW_VER)-double --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-double/config.status: fftw-$(FFTW_VER)-double/configure
	cd fftw-$(FFTW_VER)-double && \
	./configure --prefix=$(abspath $(EXTROOT)) $(FFTW_CONFIG) && \
	$(MAKE) $(jPARALLEL_BUILD_JOBS) clean
	touch $@
$(FFTW_DOUBLE_OBJ_TARGET): fftw-$(FFTW_VER)-double/config.status
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C fftw-$(FFTW_VER)-double install
	touch $@

clean-fftw: clean-fftw-single clean-fftw-double
clean-fftw-single:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C fftw-$(FFTW_VER)-single clean
	rm -f $(FFTW_SINGLE_OBJ_TARGET)
clean-fftw-double:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C fftw-$(FFTW_VER)-double clean
	rm -f $(FFTW_DOUBLE_OBJ_TARGET)
distclean-fftw:
	rm -rf fftw-$(FFTW_VER).tar.gz fftw-$(FFTW_VER)-{single,double}


## SUITESPARSE ##

SUITESPARSE_LOCAL_OBJ_SOURCE = SuiteSparse-$(SUITESPARSE_VER)/lib/libsuitesparse.$(SHLIB_EXT)
SUITESPARSE_SYSTEM_OBJ_SOURCE = SuiteSparse-SYSTEM/lib/libsuitesparse.$(SHLIB_EXT)
SUITESPARSE_OBJ_TARGET = $(EXTROOTLIB)/libsuitesparse.$(SHLIB_EXT)
ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
SUITESPARSE_OBJ_SOURCE = $(SUITESPARSE_LOCAL_OBJ_SOURCE)
else
SUITESPARSE_OBJ_SOURCE = $(SUITESPARSE_SYSTEM_OBJ_SOURCE)
endif

compile-suitesparse: $(SUITESPARSE_OBJ_SOURCE)
install-suitesparse: $(SUITESPARSE_OBJ_TARGET) $(EXTROOTLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)

SuiteSparse-$(SUITESPARSE_VER).tar.gz:
	$(WGET) http://www.cise.ufl.edu/research/sparse/SuiteSparse/$@ 
SuiteSparse-$(SUITESPARSE_VER)/Makefile: SuiteSparse-$(SUITESPARSE_VER).tar.gz
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)
	tar -C SuiteSparse-$(SUITESPARSE_VER) --strip-components 1 -zxf $<
	touch $@
$(SUITESPARSE_LOCAL_OBJ_SOURCE): SuiteSparse-$(SUITESPARSE_VER)/Makefile $(LAPACK_OBJ_TARGET) $(OPENBLAS_OBJ_SOURCE)
	cd SuiteSparse-$(SUITESPARSE_VER) && \
	$(MAKE) $(jPARALLEL_BUILD_JOBS) && \
	mkdir -p lib && \
	cp `find UMFPACK CHOLMOD SPQR *AMD BTF UFconfig -name *.a` lib && \
	cd lib && \
	for i in *.a; do ar x $$i; done && \
	$(FC) -shared $(FFLAGS) $(JFFLAGS) *.o $(LIBLAPACK) $(LIBBLAS) -lstdc++ -o libsuitesparse.$(SHLIB_EXT)
$(SUITESPARSE_SYSTEM_OBJ_SOURCE):
	mkdir -p SuiteSparse-SYSTEM/lib && \
	cd SuiteSparse-SYSTEM/lib && \
	cp `find /lib /usr/lib /usr/local/lib $(subst :, ,$(LD_LIBRARY_PATH)) $(subst :, ,$(DYLD_LIBRARY_PATH)) \
	$(subst :, ,$(LIBRARY_PATH)) -name libamd.a -o -name libbtf.a -o -name libcamd.a -o -name libccolamd.a \
	-o -name libcholmod.a -o -name libcolamd.a -o -name libspqr.a -o -name libufconfig.a \
	-o -name libcerbla.a -o -name libumfpack.a` . && \
	for i in *.a; do ar x $$i; done && \
	$(FC) -shared $(FFLAGS) $(JFFLAGS) *.o $(LIBLAPACK) $(LIBBLAS) -lstdc++ -o libsuitesparse.$(SHLIB_EXT)
$(SUITESPARSE_OBJ_TARGET): $(SUITESPARSE_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp -f $< $@

clean-suitesparse:
	-$(MAKE) $(jPARALLEL_BUILD_JOBS) -C SuiteSparse-$(SUITESPARSE_VER) clean
	rm -fr SuiteSparse-$(SUITESPARSE_VER)/lib
	rm -fr SuiteSparse-SYSTEM/lib
distclean-suitesparse: clean-suitesparse
	rm -rf SuiteSparse-$(SUITESPARSE_VER).tar.gz SuiteSparse-$(SUITESPARSE_VER) SuiteSparse-SYSTEM

# SUITESPARSE WRAPPER

ifeq ($(USE_SYSTEM_SUITESPARSE), 1)
SUITESPARSE_INC = -I /usr/include/suitesparse
SUITESPARSE_LIB = -lumfpack -lcholmod -lamd -lcamd -lcolamd
else
SUITESPARSE_INC = -I SuiteSparse-$(SUITESPARSE_VER)/CHOLMOD/Include -I SuiteSparse-$(SUITESPARSE_VER)/UFconfig
SUITESPARSE_LIB = 
endif

$(EXTROOTLIB)/libsuitesparse_wrapper.$(SHLIB_EXT): SuiteSparse_wrapper.c $(SUITESPARSE_OBJ_TARGET)
	mkdir -p $(EXTROOTLIB)
	$(CC) $(CFLAGS) -O2 -shared -fPIC $(SUITESPARSE_INC) SuiteSparse_wrapper.c -o $(EXTROOTLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)
	touch $@
install-suitesparse-wrapper: $(EXTROOTLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)

clean-suitesparse-wrapper:
	rm -f $(SUITESPARSE_OBJ_TARGET) $(EXTROOTLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)
distclean-suitesparse-wrapper: clean-suitesparse-wrapper

## CLP ##

CLP_OBJ_TARGET = $(EXTROOTLIB)/libClp.$(SHLIB_EXT)

compile-clp: install-clp
install-clp: $(CLP_OBJ_TARGET)

clp-$(CLP_VER).tar.gz:
	$(WGET_DASH_O) $@ http://www.coin-or.org/download/source/Clp/Clp-1.14.5.tgz
clp-$(CLP_VER)/configure: clp-$(CLP_VER).tar.gz
	mkdir -p clp-$(CLP_VER)
	tar -C clp-$(CLP_VER) --strip-components 1 -zxf $<
	touch $@
clp-$(CLP_VER)/config.status: clp-$(CLP_VER)/configure
	cd clp-$(CLP_VER) && \
	./configure --prefix=$(abspath $(EXTROOT))
$(CLP_OBJ_TARGET): clp-$(CLP_VER)/config.status
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C clp-$(CLP_VER) install
	touch $@

clean-clp:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C clp-$(CLP_VER) clean
	rm -f $(CLP_OBJ_TARGET)
distclean-clp:
	rm -rf clp-$(CLP_VER).tar.gz clp-$(CLP_VER)

## UNWIND ##

ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
LIBUNWIND_TARGET_OBJ = $(EXTROOTLIB)/libunwind.a
LIBUNWIND_TARGET_SOURCE = libunwind-$(UNWIND_VER)/src/.libs/libunwind.a
else
LIBUNWIND_TARGET_OBJ = 
LIBUNWIND_TARGET_SOURCE = 
endif

compile-unwind: $(LIBUNWIND_TARGET_SOURCE)
install-unwind: $(LIBUNWIND_TARGET_OBJ)

libunwind-$(UNWIND_VER).tar.gz:
	$(WGET) http://savannah.spinellicreations.com/libunwind/libunwind-$(UNWIND_VER).tar.gz
libunwind-$(UNWIND_VER)/Makefile: libunwind-$(UNWIND_VER).tar.gz
	tar xfz $<
	cd libunwind-$(UNWIND_VER) && ./configure  CFLAGS="$(CFLAGS) -U_FORTIFY_SOURCE -fPIC" --prefix=$(abspath $(EXTROOT))

$(LIBUNWIND_TARGET_SOURCE): libunwind-$(UNWIND_VER)/Makefile
	cd libunwind-$(UNWIND_VER) && $(MAKE) $(jPARALLEL_BUILD_JOBS)

$(LIBUNWIND_TARGET_OBJ): $(LIBUNWIND_TARGET_SOURCE)
	cd libunwind-$(UNWIND_VER) && $(MAKE) $(jPARALLEL_BUILD_JOBS) install

clean-unwind:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C libunwind-$(UNWIND_VER) clean
	rm -f $(LIBUNWIND_TARGET_OBJ) $(LIBUNWIND_TARGET_SOURCE)

distclean-unwind:
	rm -rf libunwind-$(UNWIND_VER).tar.gz libunwind-$(UNWIND_VER)

## GNU LIGHTTPD ##

LIGHTTPD_OBJ_TARGET = $(EXTROOT)/sbin/lighttpd

compile-lighttpd: install-lighttpd
install-lighttpd: $(LIGHTTPD_OBJ_TARGET)

lighttpd-$(LIGHTTPD_VER).tar.gz:
	$(WGET) http://download.lighttpd.net/lighttpd/releases-1.4.x/lighttpd-$(LIGHTTPD_VER).tar.gz
lighttpd-$(LIGHTTPD_VER)/configure: lighttpd-$(LIGHTTPD_VER).tar.gz
	tar zxf $<
	touch $@
lighttpd-$(LIGHTTPD_VER)/config.status: lighttpd-$(LIGHTTPD_VER)/configure
	cd lighttpd-$(LIGHTTPD_VER) && \
	./configure --prefix=$(abspath $(EXTROOT)) --without-pcre --without-zlib --without-bzip2
$(LIGHTTPD_OBJ_TARGET): lighttpd-$(LIGHTTPD_VER)/config.status
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C lighttpd-$(LIGHTTPD_VER) install
	touch $@

clean-lighttpd:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C lighttpd-$(LIGHTTPD_VER) clean
	rm -f $(LIGHTTPD_OBJ_TARGET)
distclean-lighttpd:
	rm -rf lighttpd-$(LIGHTTPD_VER).tar.gz lighttpd-$(LIGHTTPD_VER)

## GMP ##

ifeq ($(USE_SYSTEM_GMP), 0)
GMP_OBJ_TARGET = $(EXTROOTLIB)/libgmp.$(SHLIB_EXT)
else
GMP_OBJ_TARGET = 
endif

compile-gmp: install-gmp
install-gmp: $(GMP_OBJ_TARGET)

gmp-$(GMP_VER).tar.bz2:
	$(WGET) ftp://ftp.gmplib.org/pub/gmp-5.0.4/$@
gmp-$(GMP_VER)/configure: gmp-$(GMP_VER).tar.bz2
	tar jxf $<
	touch $@
gmp-$(GMP_VER)/config.status: gmp-$(GMP_VER)/configure
	cd gmp-$(GMP_VER) && \
	./configure --prefix=$(abspath $(EXTROOT)) 
$(GMP_OBJ_TARGET): gmp-$(GMP_VER)/config.status
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C gmp-$(GMP_VER) 
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C gmp-$(GMP_VER) check
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C gmp-$(GMP_VER) install
	touch $@

clean-gmp:
	$(MAKE) $(jPARALLEL_BUILD_JOBS) -C gmp-$(GMP_VER) clean
	rm -f $(GMP_OBJ_TARGET)
distclean-gmp:
	rm -rf gmp-$(GMP_VER).tar.bz2 gmp-$(GMP_VER)

##GMP Wrapper

GMP_INC = -I gmp-$(GMP_VER)/
GMP_LIB = -L$(EXTROOTLIB)/ -lgmp

$(EXTROOTLIB)/libgmp_wrapper.$(SHLIB_EXT): gmp_wrapper.c $(GMP_OBJ_TARGET)
	mkdir -p $(EXTROOTLIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared -fPIC $(GMP_INC) gmp_wrapper.c -o $(EXTROOTLIB)/libgmp_wrapper.$(SHLIB_EXT) -Wl,-rpath,$(EXTROOTLIB) $(GMP_LIB)
	touch $@
install-gmp-wrapper: $(EXTROOTLIB)/libgmp_wrapper.$(SHLIB_EXT)

clean-gmp-wrapper:
	rm -f $(GMP_OBJ_TARGET) $(EXTROOTLIB)/libgmp_wrapper.$(SHLIB_EXT)
distclean-gmp-wrapper: clean-gmp-wrapper

## phony targets ##

.PHONY: \
	default compile install cleanall distclean \
	compile-* install-* clean-* distclean-*
