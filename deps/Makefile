## library versions ##

LLVM_VER = 3.0
READLINE_VER = 6.2
PCRE_VER = 8.30
GRISU_VER = 1.1
DSFMT_VER = 2.1
OPENBLAS_VER = v0.1.1
LAPACK_VER = 3.4.1
ARPACK_VER = 3.1.0
FFTW_VER = 3.3.1
SUITESPARSE_VER = 3.7.0
CLP_VER = 1.14.5
UNWIND_VER = 1.0.1
LIGHTTPD_VER = 1.4.29
GMP_VER=5.0.4
GLPK_VER = 4.43

## high-level setup ##

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

LIBS = uv fdlibm dsfmt rmath double-conversion amos suitesparse-wrapper gmp-wrapper

## WGET / CURL
ifeq ($(OS), Linux)
WGET = wget --no-check-certificate
WGET_DASH_O = wget --no-check-certificate -O
LIBS += unwind
endif

ifeq ($(OS), Darwin)
WGET = curl -kLO
WGET_DASH_O = curl -kLo
endif

ifeq ($(OS), FreeBSD)
WGET = curl -LO
WGET_DASH_O = curl -Lo
LIBS += unwind
endif

## USE_SYSTEM_LIBS options

ifeq ($(USE_SYSTEM_LLVM), 0)
LIBS += llvm
endif

ifeq ($(OS),WINNT)
READLINE_VER = 5.0
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

ifeq ($(USE_SYSTEM_FFTW), 0)
LIBS += fftw
endif

ifeq ($(USE_SYSTEM_GMP), 0)
LIBS += gmp
endif

ifeq ($(USE_SYSTEM_ARPACK), 0)
LIBS += arpack
endif

#ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
LIBS += suitesparse
#endif

ifeq ($(USE_SYSTEM_GLPK), 0)
LIBS += glpk
endif

#ifeq ($(USE_SYSTEM_LAPACK), 0)
#LIBS += lapack
#endif

## Common build target prefixes

default: $(USR) install
compile: $(addprefix compile-, $(LIBS))
install: $(addprefix install-, $(LIBS))
cleanall: $(addprefix clean-, $(LIBS))
distclean: $(addprefix distclean-, $(LIBS))
	rm -rf root

## PATHS ##
$(USR): 
	mkdir -p $(USR)/lib
	mkdir -p $(USR)/include
	mkdir -p $(USR)/bin

## LLVM ##

LLVM_OBJ_TARGET = $(USRLIB)/libLLVM-$(LLVM_VER).$(SHLIB_EXT)
ifeq ($(OS),WINNT)
	LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/Release/bin/LLVM-$(LLVM_VER).$(SHLIB_EXT)
else
	LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/Release/lib/libLLVM-$(LLVM_VER).$(SHLIB_EXT)
endif

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
	./configure --prefix=$(abspath $(USR)) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 --disable-bindings --disable-docs CC=gcc CXX=g++ && \
	$(MAKE)
$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE)
	$(MAKE) -C llvm-$(LLVM_VER) install
	$(JULIAHOME)/deps/install-name-$(OS).sh libLLVM-3.0.$(SHLIB_EXT) $(USRLIB)/libLLVM-3.0.$(SHLIB_EXT) 
	touch $@

clean-llvm:
	$(MAKE) -C llvm-$(LLVM_VER) clean
	rm -f $(LLVM_OBJ_TARGET)
distclean-llvm:
	rm -rf llvm-$(LLVM_VER).tar.gz llvm-$(LLVM_VER)

## GNU readline ##

READLINE_OBJ_TARGET = $(USRLIB)/libreadline.$(SHLIB_EXT)
READLINE_OBJ_SOURCE = readline-$(READLINE_VER)/shlib/libreadline.$(READLINE_VER).$(SHLIB_EXT)

READLINE_OPTS = --disable-shared --enable-static
ifeq ($(OS),WINNT)
	READLINE_URL = https://github.com/JuliaLang/readline/tarball/master
	READLINE_CFLAGS = LOCAL_DEFS="-include $(abspath .)/readline-win.h"
else
	READLINE_URL = ftp://ftp.gnu.org/gnu/readline/readline-$(READLINE_VER).tar.gz
	READLINE_OPTS += --with-curses
	READLINE_CFLAGS = 
endif

compile-readline: $(READLINE_OBJ_SOURCE)
install-readline: $(READLINE_OBJ_TARGET)

ifeq ($(OS),WINNT)

readline-$(READLINE_VER).tar.gz:
	$(WGET_DASH_O) $@ $(READLINE_URL)
readline-$(READLINE_VER)/configure: readline-$(READLINE_VER).tar.gz
	mkdir readline-$(READLINE_VER)
	tar -C readline-$(READLINE_VER) --strip-components 1 -xf $<
	touch $@
$(READLINE_OBJ_SOURCE): readline-$(READLINE_VER)/configure
	cd readline-$(READLINE_VER) && \
	./configure --prefix=$(abspath $(USR)) $(READLINE_OPTS)  && \
	$(MAKE) $(READLINE_CFLAGS)
	touch $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	$(MAKE) -C readline-$(READLINE_VER) $(READLINE_CFLAGS) install
	touch $@

clean-readline:
	$(MAKE) -C readline-$(READLINE_VER) clean
	rm -f $(READLINE_OBJ_TARGET)
distclean-readline:
	rm -rf readline-$(READLINE_VER).tar.gz readline-$(READLINE_VER)

else

readline-$(READLINE_VER).tar.gz:
	$(WGET) http://ftp.gnu.org/gnu/readline/$@
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-001
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-002
readline-$(READLINE_VER)/configure: readline-$(READLINE_VER).tar.gz
	tar zxf $<
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-001
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-002
	touch $@
$(READLINE_OBJ_SOURCE): readline-$(READLINE_VER)/configure
	cd readline-$(READLINE_VER) && \
	./configure --prefix=$(abspath $(USR)) --enable-shared --enable-static --with-curses && \
	$(MAKE)
	touch $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	$(MAKE) -C readline-$(READLINE_VER) install
	chmod +w $(USRLIB)/libreadline.*
	$(JULIAHOME)/deps/install-name-$(OS).sh libreadline.$(SHLIB_EXT) $(USRLIB)/libreadline.$(SHLIB_EXT) 
	touch $@

clean-readline:
	$(MAKE) -C readline-$(READLINE_VER) clean
	rm -f $(READLINE_OBJ_TARGET)
distclean-readline:
	rm -rf readline-$(READLINE_VER).tar.gz readline-$(READLINE_VER)

endif

## LIBUV

ifneq ($(OS), WINNT) 
	UV_CFLAGS=
else
	UV_CFLAGS=CFLAGS="-DEV_SIGNAL_ENABLE=0"
endif

UV_OBJ_TARGET = $(USRLIB)/uv.a
UV_OBJ_SOURCE = libuv/uv.a

libuv/Makefile:
	(cd .. && git submodule update --init)
$(UV_OBJ_SOURCE): libuv/Makefile
	$(MAKE) -C libuv $(UV_CFLAGS)
$(UV_OBJ_TARGET): $(UV_OBJ_SOURCE)
	mkdir -p $(USR)/include
	cp $(UV_OBJ_SOURCE) $(USRLIB)/
	cp libuv/include/uv.h $(USR)/include/uv.h 
	cp -r libuv/include/uv-private $(USR)/include
install-uv: $(UV_OBJ_TARGET)

clean-uv:
	$(MAKE) -C libuv clean
	rm -f $(USRLIB)/uv.a $(USR)/include/uv.h
distclean-uv: clean-uv

## PCRE ##

PCRE_OBJ_TARGET = $(USRLIB)/libpcre.$(SHLIB_EXT)

compile-pcre: install-pcre
install-pcre: $(PCRE_OBJ_TARGET)

pcre-$(PCRE_VER).tar.bz2:
	$(WGET) ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/$@
pcre-$(PCRE_VER)/configure: pcre-$(PCRE_VER).tar.bz2
	tar jxf $<
	touch $@
pcre-$(PCRE_VER)/config.status: pcre-$(PCRE_VER)/configure
	cd pcre-$(PCRE_VER) && \
	./configure --prefix=$(abspath $(USR)) --enable-utf8 --enable-unicode-properties --enable-jit
$(PCRE_OBJ_TARGET): pcre-$(PCRE_VER)/config.status
	$(MAKE) -C pcre-$(PCRE_VER) install
ifeq ($(OS),WINNT)
	-rm root/lib/libpcre.dll
	mv root/bin/libpcre-0.dll root/lib/libpcre.dll
endif
	touch $@

clean-pcre:
	$(MAKE) -C pcre-$(PCRE_VER) clean
	rm -f $(PCRE_OBJ_TARGET)
distclean-pcre:
	rm -rf pcre-$(PCRE_VER).tar.bz2 pcre-$(PCRE_VER)

## Grisu floating-point printing library ##

GRISU_OPTS = -O3 -fvisibility=hidden $(fPIC)


compile-double-conversion: double-conversion-$(GRISU_VER)/src/libgrisu_.$(SHLIB_EXT)
install-double-conversion: $(USRLIB)/libgrisu.$(SHLIB_EXT)

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
$(USRLIB)/libgrisu.$(SHLIB_EXT): double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
	cp -f $< $@

clean-double-conversion:
	rm -f double-conversion-$(GRISU_VER)/src/*.o
	rm -f double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
distclean-double-conversion:
	rm -rf double-conversion-$(GRISU_VER).tar.gz double-conversion-$(GRISU_VER)

## fdlibm ##

FDLIBM_OBJ_TARGET = $(USRLIB)/libfdm.$(SHLIB_EXT)
FDLIBM_OBJ_SOURCE = fdlibm/libfdm.$(SHLIB_EXT)

compile-fdlibm: $(FDLIBM_OBJ_SOURCE)
install-fdlibm: $(FDLIBM_OBJ_TARGET)

$(FDLIBM_OBJ_SOURCE): fdlibm/*.c
	cd fdlibm && \
	$(MAKE) CC=$(CC) CFLAGS="$(CFLAGS) -D_IEEE_LIBM -Dx86 $(fPIC) -O2 $(CONFIG)" && \
	$(CC) -shared *.o -o libfdm.$(SHLIB_EXT)
$(FDLIBM_OBJ_TARGET): $(FDLIBM_OBJ_SOURCE)
	cp $< $@

clean-fdlibm:
	cd fdlibm && $(MAKE) clean
	rm -f $(FDLIBM_OBJ_SOURCE) $(FDLIBM_OBJ_TARGET)
distclean-fdlibm: clean-fdlibm

## rmath ##

RMATH_OBJ_TARGET = $(USRLIB)/libRmath.$(SHLIB_EXT)
RMATH_OBJ_SOURCE = Rmath/src/libRmath.$(SHLIB_EXT)

compile-rmath: $(RMATH_OBJ_SOURCE)
install-rmath: $(RMATH_OBJ_TARGET)

$(RMATH_OBJ_SOURCE): Rmath/src/Makefile install-dsfmt
	cd Rmath/src && \
	$(MAKE) CC=$(CC) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh libRmath.$(SHLIB_EXT) libRmath.$(SHLIB_EXT)
$(RMATH_OBJ_TARGET): $(RMATH_OBJ_SOURCE)
	mkdir -p $(USRLIB)
	cp $< $@

clean-rmath:
	cd Rmath/src && $(MAKE) clean
	rm -f $(RMATH_OBJ_SOURCE) $(RMATH_OBJ_TARGET)
distclean-rmath: clean-rmath

## amos ##

AMOS_OBJ_TARGET = $(USRLIB)/libamos.$(SHLIB_EXT)
AMOS_OBJ_SOURCE = amos/libamos.$(SHLIB_EXT)

compile-amos: $(AMOS_OBJ_SOURCE)
install-amos: $(AMOS_OBJ_TARGET)


$(AMOS_OBJ_SOURCE): amos/*.f
	cd amos && \
	$(MAKE) && \
	$(FC) $(FFLAGS) $(JFFLAGS) -shared *.o -o libamos.$(SHLIB_EXT)
$(AMOS_OBJ_TARGET): $(AMOS_OBJ_SOURCE)
	cp $< $@

clean-amos:
	cd amos && $(MAKE) clean
	rm -f $(AMOS_OBJ_SOURCE) $(AMOS_OBJ_TARGET)
distclean-amos: clean-amos

## DSFMT ##

DSFMT_OBJ_TARGET = $(USRLIB)/librandom.$(SHLIB_EXT)
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
	$(CC) $(CFLAGS) -O3 -finline-functions -fomit-frame-pointer -DNDEBUG -fno-strict-aliasing --param max-inline-insns-single=1800  -Wmissing-prototypes -Wall  -std=c99 -DDSFMT_MEXP=19937 $(fPIC) -shared -DDSFMT_DO_NOT_USE_OLD_NAMES jl_random.c -o librandom.$(SHLIB_EXT) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh librandom.$(SHLIB_EXT) librandom.$(SHLIB_EXT)

$(DSFMT_OBJ_TARGET): $(DSFMT_OBJ_SOURCE)
	cp $< $@

clean-dsfmt:
	rm -f random/librandom.$(SHLIB_EXT)
distclean-dsfmt: clean-dsfmt
	cd random && rm -rf *.tar.gz dsfmt-$(DSFMT_VER)

## OpenBLAS ##

ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(OS),WINNT)
OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.lib
else
OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT)
endif
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
	$(MAKE) -C openblas-$(OPENBLAS_VER) DYNAMIC_ARCH=1 USE_THREAD=1 CC=$(CC) FC=$(FC)  FFLAGS="$(FFLAGS) $(JFFLAGS)" TARGET=$(TARGET_OPENBLAS_ARCH)
	cp -f openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT) $(USRLIB)
	$(JULIAHOME)/deps/install-name-$(OS).sh libopenblas.$(SHLIB_EXT) $(USRLIB)/libopenblas.$(SHLIB_EXT)

clean-openblas:
	$(MAKE) -C openblas-$(OPENBLAS_VER) clean
distclean-openblas:
	rm -rf openblas-$(OPENBLAS_VER).tar.gz openblas-$(OPENBLAS_VER)

## LAPACK ##

ifeq ($(USE_SYSTEM_LAPACK), 0)
LAPACK_OBJ_TARGET = $(USRLIB)/liblapack.$(SHLIB_EXT)
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
lapack-$(LAPACK_VER)/INSTALL/dlamch.o: lapack-$(LAPACK_VER)/Makefile $(OPENBLAS_OBJ_SOURCE)
	cd lapack-$(LAPACK_VER) && \
	cp INSTALL/make.inc.gfortran ./make.inc && \
	$(MAKE) lapacklib NOOPT="-O0 $(fPIC)" OPTS="$(FFLAGS) $(JFFLAGS)" FORTRAN=$(FC) LOADER=$(FC)
	touch $@
$(LAPACK_OBJ_SOURCE): lapack-$(LAPACK_VER)/INSTALL/dlamch.o $(OPENBLAS_OBJ_SOURCE)
	cd lapack-$(LAPACK_VER) && \
	$(FC) -shared $(FFLAGS) $(JFFLAGS) SRC/*.o INSTALL/dlamch.o INSTALL/dsecnd_INT_ETIME.o INSTALL/ilaver.o INSTALL/slamch.o $(LIBBLAS) -o liblapack.$(SHLIB_EXT)
$(LAPACK_OBJ_TARGET): $(LAPACK_OBJ_SOURCE)
	cp $< $@

clean-lapack:
	$(MAKE) -C lapack-$(LAPACK_VER) clean
	rm -f $(LAPACK_OBJ_SOURCE) $(LAPACK_OBJ_TARGET)
distclean-lapack:
	rm -rf lapack-$(LAPACK_VER).tgz lapack-$(LAPACK_VER)

## ARPACK ##

ARPACK_OBJ_TARGET = $(USRLIB)/libarpack.$(SHLIB_EXT)
ARPACK_OBJ_SOURCE = arpack-ng_$(ARPACK_VER)/libarpack.$(SHLIB_EXT)

compile-arpack: $(ARPACK_OBJ_SOURCE)
install-arpack: $(ARPACK_OBJ_TARGET)

arpack-ng_$(ARPACK_VER).tar.gz:
	$(WGET_DASH_O) $@ http://forge.scilab.org/index.php/p/arpack-ng/downloads/get/arpack-ng_$(ARPACK_VER).tar.gz
	touch $@
arpack-ng_$(ARPACK_VER)/configure: arpack-ng_$(ARPACK_VER).tar.gz
	tar zxf $< 
	touch $@
$(ARPACK_OBJ_SOURCE): arpack-ng_$(ARPACK_VER)/configure install-openblas
	cd arpack-ng_$(ARPACK_VER) && \
	./configure  --prefix=$(abspath $(USR)) --with-blas="$(LIBBLAS)" --with-lapack="$(LIBLAPACK)" --disable-mpi --enable-shared MPIF77=$(FC)
	touch $@
$(ARPACK_OBJ_TARGET): $(ARPACK_OBJ_SOURCE)
	mkdir -p $(USRLIB)
	cd arpack-ng_$(ARPACK_VER) && \
	make install MPIF77=$(FC)
	touch $@

clean-arpack:
	$(MAKE) -C arpack-ng_$(ARPACK_VER) clean
	rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	rm -rf arpack-ng_$(ARPACK_VER).tar.gz arpack-ng_$(ARPACK_VER)

## FFTW ##

FFTW_SINGLE_OBJ_TARGET = $(USRLIB)/libfftw3f.3.$(SHLIB_EXT)
FFTW_DOUBLE_OBJ_TARGET = $(USRLIB)/libfftw3.3.$(SHLIB_EXT)

compile-fftw: compile-fftw-single compile-fftw-double
compile-fftw-single: install-fftw-single
compile-fftw-double: install-fftw-double

install-fftw: install-fftw-single install-fftw-double
install-fftw-single: $(FFTW_SINGLE_OBJ_TARGET)
install-fftw-double: $(FFTW_DOUBLE_OBJ_TARGET)

FFTW_CONFIG = --enable-shared --disable-fortran --disable-mpi --enable-fma --enable-sse2 --enable-threads
ifeq ($(OS),WINNT)
	FFTW_CONFIG += --with-our-malloc
endif

fftw-$(FFTW_VER).tar.gz:
	$(WGET) http://www.fftw.org/$@

fftw-$(FFTW_VER)-single/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-single && \
	tar -C fftw-$(FFTW_VER)-single --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-single/config.status: fftw-$(FFTW_VER)-single/configure
	cd fftw-$(FFTW_VER)-single && \
	./configure --prefix=$(abspath $(USR)) $(FFTW_CONFIG) --enable-shared --disable-fortran --enable-sse --enable-single --enable-threads && \
	$(MAKE) clean
	touch $@
$(FFTW_SINGLE_OBJ_TARGET): fftw-$(FFTW_VER)-single/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-single install
ifeq ($(OS),WINNT)
	-rm root/lib/libfftw3f.dll
	mv root/bin/libfftw3f-3.dll root/lib/libfftw3f.dll
endif
	touch $@

fftw-$(FFTW_VER)-double/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-double && \
	tar -C fftw-$(FFTW_VER)-double --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-double/config.status: fftw-$(FFTW_VER)-double/configure
	cd fftw-$(FFTW_VER)-double && \
	./configure --prefix=$(abspath $(USR)) $(FFTW_CONFIG) && \
	$(MAKE) clean
	touch $@
$(FFTW_DOUBLE_OBJ_TARGET): fftw-$(FFTW_VER)-double/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-double install
ifeq ($(OS),WINNT)
	-rm root/lib/libfftw3.dll
	mv root/bin/libfftw3-3.dll root/lib/libfftw3.dll
endif
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

ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
SUITESPARSE_OBJ_SOURCE = SuiteSparse-$(SUITESPARSE_VER)/UMFPACK/Lib/libumfpack.a
endif

SUITESPARSE_OBJ_TARGET = $(USRLIB)/libumfpack.$(SHLIB_EXT)

ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
compile-suitesparse: $(SUITESPARSE_OBJ_SOURCE)
else:
compile-suitesparse: install-openblas
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)/lib
	cd SuiteSparse-$(SUITESPARSE_VER)/lib && \
	rm -f *.a && \
	cp -f `find /lib /usr/lib /usr/local/lib $(subst -L,,$(LDFLAGS)) -name libamd.a -o -name libcolamd.a -o -name libcholmod.a -o -name libumfpack.a 2>/dev/null` . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libamd.$(SHLIB_EXT) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh libamd.$(SHLIB_EXT) $(USRLIB)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libcolamd.$(SHLIB_EXT) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh libcolamd.$(SHLIB_EXT) $(USRLIB)/libcolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libcholmod.$(SHLIB_EXT) -L$(USRLIB) -lcolamd -lamd $(LIBBLAS) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh libcholmod.$(SHLIB_EXT) $(USRLIB)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libumfpack.$(SHLIB_EXT) -L$(USRLIB) -lcholmod -lcolamd -lamd $(LIBBLAS) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh libumfpack.$(SHLIB_EXT) $(USRLIB)/libumfpack.$(SHLIB_EXT)
endif
install-suitesparse: $(SUITESPARSE_OBJ_TARGET) $(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)

SuiteSparse-$(SUITESPARSE_VER).tar.gz:
	$(WGET) http://www.cise.ufl.edu/research/sparse/SuiteSparse/$@ 
SuiteSparse-$(SUITESPARSE_VER)/Makefile: SuiteSparse-$(SUITESPARSE_VER).tar.gz
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)
	tar -C SuiteSparse-$(SUITESPARSE_VER) --strip-components 1 -zxf $<
	touch $@
$(SUITESPARSE_OBJ_SOURCE): $(OPENBLAS_OBJ_SOURCE) SuiteSparse-$(SUITESPARSE_VER)/Makefile
	cd SuiteSparse-$(SUITESPARSE_VER) && \
	$(MAKE) CC=$(CC) CXX=$(CXX)
	touch $@
$(SUITESPARSE_OBJ_TARGET): $(SUITESPARSE_OBJ_SOURCE)
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)/lib && \
	cd SuiteSparse-$(SUITESPARSE_VER)/lib && \
	rm -f *.a && \
	cp -f `find .. -name libamd.a -o -name libcolamd.a -o -name libcholmod.a -o -name libumfpack.a 2>/dev/null` . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libamd.$(SHLIB_EXT) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh libamd.$(SHLIB_EXT) $(USRLIB)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libcolamd.$(SHLIB_EXT) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh libcolamd.$(SHLIB_EXT) $(USRLIB)/libcolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libcholmod.$(SHLIB_EXT) -L$(USRLIB) -lcolamd -lamd $(LIBBLAS) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh libcholmod.$(SHLIB_EXT) $(USRLIB)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libumfpack.$(SHLIB_EXT) -L$(USRLIB) -lcholmod -lcolamd -lamd $(LIBBLAS) && \
	$(JULIAHOME)/deps/install-name-$(OS).sh libumfpack.$(SHLIB_EXT) $(USRLIB)/libumfpack.$(SHLIB_EXT)

clean-suitesparse:
	-$(MAKE) -C SuiteSparse-$(SUITESPARSE_VER) clean
	rm -fr SuiteSparse-$(SUITESPARSE_VER)/lib
distclean-suitesparse: clean-suitesparse
	rm -rf SuiteSparse-$(SUITESPARSE_VER).tar.gz SuiteSparse-$(SUITESPARSE_VER)

# SUITESPARSE WRAPPER

ifeq ($(USE_SYSTEM_SUITESPARSE), 1)
SUITESPARSE_INC = -I /usr/include/suitesparse
SUITESPARSE_LIB = -lumfpack -lcholmod -lamd -lcamd -lcolamd
else
SUITESPARSE_INC = -I SuiteSparse-$(SUITESPARSE_VER)/CHOLMOD/Include -I SuiteSparse-$(SUITESPARSE_VER)/UFconfig
SUITESPARSE_LIB = 
endif

$(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT): SuiteSparse_wrapper.c $(SUITESPARSE_OBJ_TARGET)
	$(CC) -O2 -shared $(fPIC) $(SUITESPARSE_INC) SuiteSparse_wrapper.c -o $(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)
	touch $@
install-suitesparse-wrapper: $(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)

clean-suitesparse-wrapper:
	rm -f $(SUITESPARSE_OBJ_TARGET) $(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)
distclean-suitesparse-wrapper: clean-suitesparse-wrapper

## CLP ##

CLP_OBJ_TARGET = $(USRLIB)/libClp.$(SHLIB_EXT)

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
	./configure --prefix=$(abspath $(USR))
$(CLP_OBJ_TARGET): clp-$(CLP_VER)/config.status
	$(MAKE) -C clp-$(CLP_VER) install
	touch $@

clean-clp:
	$(MAKE) -C clp-$(CLP_VER) clean
	rm -f $(CLP_OBJ_TARGET)
distclean-clp:
	rm -rf clp-$(CLP_VER).tar.gz clp-$(CLP_VER)

## UNWIND ##

ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
LIBUNWIND_TARGET_OBJ = $(USRLIB)/libunwind.a
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
	cd libunwind-$(UNWIND_VER) && ./configure  CFLAGS="$(CFLAGS) -U_FORTIFY_SOURCE $(fPIC)" --prefix=$(abspath $(USR))

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

LIGHTTPD_OBJ_TARGET = $(USR)/sbin/lighttpd

compile-lighttpd: install-lighttpd
install-lighttpd: $(LIGHTTPD_OBJ_TARGET)

lighttpd-$(LIGHTTPD_VER).tar.gz:
	$(WGET) http://download.lighttpd.net/lighttpd/releases-1.4.x/lighttpd-$(LIGHTTPD_VER).tar.gz
lighttpd-$(LIGHTTPD_VER)/configure: lighttpd-$(LIGHTTPD_VER).tar.gz
	tar zxf $<
	touch $@
lighttpd-$(LIGHTTPD_VER)/config.status: lighttpd-$(LIGHTTPD_VER)/configure
	cd lighttpd-$(LIGHTTPD_VER) && \
	./configure --prefix=$(abspath $(USR)) --without-pcre --without-zlib --without-bzip2
$(LIGHTTPD_OBJ_TARGET): lighttpd-$(LIGHTTPD_VER)/config.status
	$(MAKE) -C lighttpd-$(LIGHTTPD_VER) install
	touch $@

clean-lighttpd:
	$(MAKE) -C lighttpd-$(LIGHTTPD_VER) clean
	rm -f $(LIGHTTPD_OBJ_TARGET)
distclean-lighttpd:
	rm -rf lighttpd-$(LIGHTTPD_VER).tar.gz lighttpd-$(LIGHTTPD_VER)

## GMP ##

ifeq ($(USE_SYSTEM_GMP), 0)
GMP_OBJ_TARGET = $(USRLIB)/libgmp.$(SHLIB_EXT)
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
	./configure --prefix=$(abspath $(USR)) 
$(GMP_OBJ_TARGET): gmp-$(GMP_VER)/config.status
	$(MAKE) -C gmp-$(GMP_VER)
	$(MAKE) -C gmp-$(GMP_VER) check
	$(MAKE) -C gmp-$(GMP_VER) install
	touch $@

clean-gmp:
	$(MAKE) -C gmp-$(GMP_VER) clean
	rm -f $(GMP_OBJ_TARGET)
distclean-gmp:
	rm -rf gmp-$(GMP_VER).tar.bz2 gmp-$(GMP_VER)

## GMP Wrapper

GMP_INC = -I gmp-$(GMP_VER)/
GMP_LIB = -L$(USRLIB)/ -lgmp

$(USRLIB)/libgmp_wrapper.$(SHLIB_EXT): gmp_wrapper.c $(GMP_OBJ_TARGET)
	mkdir -p $(USRLIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(GMP_INC) gmp_wrapper.c -o $(USRLIB)/libgmp_wrapper.$(SHLIB_EXT) -Wl,-rpath,$(USRLIB) $(GMP_LIB)
	touch $@
install-gmp-wrapper: $(USRLIB)/libgmp_wrapper.$(SHLIB_EXT)

clean-gmp-wrapper:
	rm -f $(GMP_OBJ_TARGET) $(USRLIB)/libgmp_wrapper.$(SHLIB_EXT)
distclean-gmp-wrapper: clean-gmp-wrapper

## GLPK ##

GLPK_CONST = 0x[0-9a-fA-F]+|[-+]?\s*[0-9]+

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPK_PREFIX = /usr/
GLPK_OBJ_TARGET =
else
GLPK_PREFIX = glpk-$(GLPK_VER)/
GLPK_OBJ_TARGET = $(USRLIB)/libglpk.$(SHLIB_EXT)
endif


compile-glpk: install-glpk
install-glpk: $(GLPK_OBJ_TARGET) $(USRLIB)/libglpk_wrapper.$(SHLIB_EXT)

glpk-$(GLPK_VER).tar.gz:
	$(WGET) http://ftp.gnu.org/gnu/glpk/$@

glpk-$(GLPK_VER)/configure: glpk-$(GLPK_VER).tar.gz
	tar zxf $<
	touch $@
glpk-$(GLPK_VER)/config.status: glpk-$(GLPK_VER)/configure
	cd glpk-$(GLPK_VER) && \
	./configure --prefix=$(abspath $(USR))
$(GLPK_OBJ_TARGET): glpk-$(GLPK_VER)/config.status
	$(MAKE) -C glpk-$(GLPK_VER) install
	touch $@

clean-glpk:
	-$(MAKE) -C glpk-$(GLPK_VER) uninstall || true
	-$(MAKE) -C glpk-$(GLPK_VER) clean
distclean-glpk: clean-glpk
	rm -rf glpk-$(GLPK_VER).tar.bz2 glpk-$(GLPK_VER)


## GLPK Wrapper

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPKW_INC = -I /usr/include/
GLPKW_LIB = -lglpk
else
GLPKW_INC = -I glpk-$(GLPK_VER)/include/
GLPKW_LIB = -L$(USRLIB)/ -lglpk
endif


$(USRLIB)/libglpk_wrapper.$(SHLIB_EXT): glpk_wrapper.c $(GLPK_OBJ_TARGET)
	mkdir -p $(USRLIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared -fPIC $(GLPKW_INC) glpk_wrapper.c $(GLPKW_LIB) -o $(USRLIB)/libglpk_wrapper.$(SHLIB_EXT) -Wl,-rpath,$(USRLIB)
	touch $@
install-glpk-wrapper: $(USRLIB)/libglpk_wrapper.$(SHLIB_EXT) glpk_wrapper.c

clean-glpk-wrapper:
	rm -f $(GLPK_OBJ_TARGET) $(USRLIB)/libglpk_wrapper.$(SHLIB_EXT)
distclean-glpk-wrapper: clean-glpk-wrapper

## phony targets ##

.PHONY: \
	default compile install cleanall distclean \
	compile-* install-* clean-* distclean-*
