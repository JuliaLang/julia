## library versions ##

LLVM_VER = 3.0
READLINE_VER = 6.2
PCRE_VER = 8.21
GRISU_VER = 1.1
DSFMT_VER = 2.1
OPENBLAS_VER = v0.1alpha2.5
LAPACK_VER = 3.4.0
ARPACK_VER = 3.0.2
FFTW_VER = 3.3
SUITESPARSE_VER = 3.7.0
CLP_VER = 1.14.5
UNWIND_VER = 1.0.1
LIGHTTPD_VER = 1.4.29

## high-level setup ##

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

OS = $(shell uname)
ARCH = $(shell uname -m)
LIBS = pcre fdlibm dsfmt openblas lapack arpack fftw suitesparse suitesparse-wrapper double-conversion amos

ifeq ($(OS), Linux)
LIBS += unwind
endif

ifeq ($(USE_SYSTEM_READLINE), 0)
LIBS += readline
endif

ifeq ($(USE_SYSTEM_LLVM), 0)
LIBS += llvm
endif

ifeq ($(USE_DEBIAN), 1)
LIBS = fdlibm dsfmt double-conversion amos suitesparse-wrapper
endif

WGET = wget
WGET_DASH_O = wget -O
ifeq ($(OS), Darwin)
WGET = curl -kLO
WGET_DASH_O = curl -kLo
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
	./configure --prefix=$(abspath $(EXTROOT)) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 --disable-bindings --disable-docs && \
	$(MAKE)
$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE)
	$(MAKE) -C llvm-$(LLVM_VER) install
	touch $@

clean-llvm:
	$(MAKE) -C llvm-$(LLVM_VER) clean
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
	$(MAKE)
	touch $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	$(MAKE) -C readline-$(READLINE_VER) install
	touch $@

clean-readline:
	$(MAKE) -C readline-$(READLINE_VER) clean
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
	$(MAKE) -C pcre-$(PCRE_VER) install
	touch $@

clean-pcre:
	$(MAKE) -C pcre-$(PCRE_VER) clean
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
	cp -f $< $@

clean-double-conversion:
	rm -f double-conversion-$(GRISU_VER)/src/*.o
	rm -f double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
distclean-double-conversion:
	rm -rf double-conversion-$(GRISU_VER)

## fdlibm ##

FDLIBM_OBJ_TARGET = $(EXTROOTLIB)/libfdm.$(SHLIB_EXT)
FDLIBM_OBJ_SOURCE = fdlibm/libfdm.$(SHLIB_EXT)

compile-fdlibm: $(FDLIBM_OBJ_SOURCE)
install-fdlibm: $(FDLIBM_OBJ_TARGET)

$(FDLIBM_OBJ_SOURCE): fdlibm/*.c
	cd fdlibm && \
	$(MAKE) CC=$(CC) CFLAGS="-D_IEEE_LIBM -Dx86 -fPIC -O2 $(CONFIG)" && \
	$(CC) -shared *.o -o libfdm.$(SHLIB_EXT)
$(FDLIBM_OBJ_TARGET): $(FDLIBM_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-fdlibm:
	cd fdlibm && $(MAKE) clean
	rm -f $(FDLIBM_OBJ_SOURCE) $(FDLIBM_OBJ_TARGET)
distclean-fdlibm: clean-fdlibm

## amos ##

AMOS_OBJ_TARGET = $(EXTROOTLIB)/libamos.$(SHLIB_EXT)
AMOS_OBJ_SOURCE = amos/libamos.$(SHLIB_EXT)

compile-amos: $(AMOS_OBJ_SOURCE)
install-amos: $(AMOS_OBJ_TARGET)

$(AMOS_OBJ_SOURCE): amos/*.f
	cd amos && \
	$(MAKE) && \
	$(FC) -shared *.o -o libamos.$(SHLIB_EXT)
$(AMOS_OBJ_TARGET): $(AMOS_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-amos:
	cd amos && $(MAKE) clean
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
	$(CC) -O3 -finline-functions -fomit-frame-pointer -DNDEBUG -fno-strict-aliasing --param max-inline-insns-single=1800  -Wmissing-prototypes -Wall  -std=c99 -DDSFMT_MEXP=19937 -fPIC -shared -DDSFMT_DO_NOT_USE_OLD_NAMES jl_random.c -o librandom.$(SHLIB_EXT)

$(DSFMT_OBJ_TARGET): $(DSFMT_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-dsfmt:
	rm -f random/librandom.$(SHLIB_EXT)
distclean-dsfmt: clean-dsfmt
	cd random && rm -rf *.tar.gz dsfmt-$(DSFMT_VER)

## OpenBLAS ##

OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.a

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
	$(MAKE) -C openblas-$(OPENBLAS_VER) DYNAMIC_ARCH=1 USE_THREAD=0 NO_LAPACK=1 CC=$(CC) FC=$(FC)  FFLAGS="$(FFLAGS)" TARGET=$(TARGET_OPENBLAS_ARCH)

clean-openblas:
	$(MAKE) -C openblas-$(OPENBLAS_VER) clean
distclean-openblas:
	rm -rf openblas-$(OPENBLAS_VER).tar.gz openblas-$(OPENBLAS_VER)

## LAPACK ##

LAPACK_OBJ_TARGET = $(EXTROOTLIB)/liblapack.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE = lapack-$(LAPACK_VER)/liblapack.$(SHLIB_EXT)

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
	$(MAKE) lapacklib NOOPT="-O0 -fPIC" OPTS="$(FFLAGS)" FORTRAN=$(FC)
$(LAPACK_OBJ_SOURCE): lapack-$(LAPACK_VER)/INSTALL/dlamch.o $(OPENBLAS_OBJ_SOURCE)
	cd lapack-$(LAPACK_VER) && \
	$(FC) -shared $(FFLAGS) SRC/*.o INSTALL/dlamch.o INSTALL/dsecnd_INT_ETIME.o INSTALL/ilaver.o INSTALL/slamch.o ../$(OPENBLAS_OBJ_SOURCE) -o liblapack.$(SHLIB_EXT)
$(LAPACK_OBJ_TARGET): $(LAPACK_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-lapack:
	$(MAKE) -C lapack-$(LAPACK_VER) clean
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
$(ARPACK_OBJ_TARGET): arpack-ng-$(ARPACK_VER)/configure
	cd arpack-ng-$(ARPACK_VER) && \
	rm -f UTIL/second_NONE.f && \
	$(FC) -shared $(FFLAGS) SRC/*.f UTIL/*.f ../lapack-$(LAPACK_VER)/liblapack.a ../$(OPENBLAS_OBJ_SOURCE) -o libarpack.$(SHLIB_EXT) && \
	cp libarpack.$(SHLIB_EXT) $(JULIALIB)
	touch $@

clean-arpack:
	$(MAKE) -C arpack-ng-$(ARPACK_VER) clean
	rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	rm -rf arpack-$(ARPACK_VER).tar.gz arpack-ng-$(ARPACK_VER)

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
	$(MAKE) clean
	touch $@
$(FFTW_SINGLE_OBJ_TARGET): fftw-$(FFTW_VER)-single/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-single install
	touch $@

fftw-$(FFTW_VER)-double/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-double && \
	tar -C fftw-$(FFTW_VER)-double --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-double/config.status: fftw-$(FFTW_VER)-double/configure
	cd fftw-$(FFTW_VER)-double && \
	./configure --prefix=$(abspath $(EXTROOT)) $(FFTW_CONFIG) && \
	$(MAKE) clean
	touch $@
$(FFTW_DOUBLE_OBJ_TARGET): fftw-$(FFTW_VER)-double/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-double install
	touch $@

clean-fftw: clean-fftw-single clean-fftw-double
clean-fftw-single:
	$(MAKE) -C fftw-$(FFTW_VER)-single clean
	rm -f $(FFTW_SINGLE_OBJ_TARGET)
clean-fftw-double:
	$(MAKE) -C fftw-$(FFTW_VER)-double clean
	rm -f $(FFTW_DOUBLE_OBJ_TARGET)
distclean-fftw:
	rm -rf fftw-$(FFTW_VER).tar.gz fftw-$(FFTW_VER)-{single,double}


## SUITESPARSE ##

SUITESPARSE_OBJ_TARGET = $(EXTROOTLIB)/libsuitesparse.$(SHLIB_EXT)
SUITESPARSE_OBJ_SOURCE = SuiteSparse-$(SUITESPARSE_VER)/lib/libsuitesparse.$(SHLIB_EXT)

compile-suitesparse: $(SUITESPARSE_OBJ_SOURCE)
install-suitesparse: $(SUITESPARSE_OBJ_TARGET) $(EXTROOTLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)

SuiteSparse-$(SUITESPARSE_VER).tar.gz:
	$(WGET) http://www.cise.ufl.edu/research/sparse/SuiteSparse/$@ 
SuiteSparse-$(SUITESPARSE_VER)/Makefile: SuiteSparse-$(SUITESPARSE_VER).tar.gz
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)
	tar -C SuiteSparse-$(SUITESPARSE_VER) --strip-components 1 -zxf $<
	touch $@
$(SUITESPARSE_OBJ_SOURCE): SuiteSparse-$(SUITESPARSE_VER)/Makefile
	cd SuiteSparse-$(SUITESPARSE_VER) && \
	$(MAKE) && \
	mkdir -p lib && \
	cp `find UMFPACK CHOLMOD SPQR *AMD BTF UFconfig -name *.a` lib && \
	cd lib && \
	for i in *.a; do ar x $$i; done && \
	gfortran -shared $(FFLAGS) *.o ../../lapack-$(LAPACK_VER)/liblapack.a ../../$(OPENBLAS_OBJ_SOURCE) -lstdc++ -o libsuitesparse.$(SHLIB_EXT)
$(SUITESPARSE_OBJ_TARGET): $(SUITESPARSE_OBJ_SOURCE)
	cp -f $< $@

clean-suitesparse:
	$(MAKE) -C SuiteSparse-$(SUITESPARSE_VER) clean
	rm -fr SuiteSparse-$(SUITESPARSE_VER)/lib
distclean-suitesparse: clean-suitesparse
	rm -rf SuiteSparse-$(SUITESPARSE_VER).tar.gz SuiteSparse-$(SUITESPARSE_VER)

# SUITESPARSE WRAPPER

ifeq ($(USE_DEBIAN), 1)
SUITESPARSE_INC = -I /usr/include/suitesparse
SUITESPARSE_LIB = -lumfpack -lcholmod -lamd -lcamd -lcolamd
else
SUITESPARSE_INC = -I SuiteSparse-$(SUITESPARSE_VER)/CHOLMOD/Include -I SuiteSparse-$(SUITESPARSE_VER)/UFconfig
SUITESPARSE_LIB = 
endif

$(EXTROOTLIB)/libsuitesparse_wrapper.$(SHLIB_EXT): SuiteSparse_wrapper.c
	$(CC) -O2 -shared -fPIC $(SUITESPARSE_INC) SuiteSparse_wrapper.c -o $(EXTROOTLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)
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
	$(MAKE) -C clp-$(CLP_VER) install
	touch $@

clean-clp:
	$(MAKE) -C clp-$(CLP_VER) clean
	rm -f $(CLP_OBJ_TARGET)
distclean-clp:
	rm -rf clp-$(CLP_VER).tar.bz2 clp-$(CLP_VER)

## UNWIND ##

LIBUNWIND_TARGET_OBJ = $(EXTROOTLIB)/libunwind.a
LIBUNWIND_TARGET_SOURCE = libunwind-$(UNWIND_VER)/src/.libs/libunwind.a

compile-unwind: $(LIBUNWIND_TARGET_SOURCE)
install-unwind: $(LIBUNWIND_TARGET_OBJ)

libunwind-$(UNWIND_VER).tar.gz:
	$(WGET) http://savannah.spinellicreations.com/libunwind/libunwind-$(UNWIND_VER).tar.gz
libunwind-$(UNWIND_VER)/Makefile: libunwind-$(UNWIND_VER).tar.gz
	tar xfz $<
	cd libunwind-$(UNWIND_VER) && ./configure  CFLAGS="-U_FORTIFY_SOURCE -fPIC" --prefix=$(abspath $(EXTROOT))

$(LIBUNWIND_TARGET_SOURCE): libunwind-$(UNWIND_VER)/Makefile
	cd libunwind-$(UNWIND_VER) && $(MAKE)

$(LIBUNWIND_TARGET_OBJ): $(LIBUNWIND_TARGET_SOURCE)
	cd libunwind-$(UNWIND_VER) && $(MAKE) install

clean-unwind:
	$(MAKE) -C libunwind-$(UNWIND_VER) clean
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
	$(MAKE) -C lighttpd-$(LIGHTTPD_VER) install
	touch $@

clean-lighttpd:
	$(MAKE) -C lighttpd-$(LIGHTTPD_VER) clean
	rm -f $(LIGHTTPD_OBJ_TARGET)
distclean-lighttpd:
	rm -rf lighttpd-$(LIGHTTPD_VER).tar.gz lighttpd-$(LIGHTTPD_VER)

## phony targets ##

.PHONY: \
	default compile install cleanall distclean \
	compile-* install-* clean-* distclean-*
