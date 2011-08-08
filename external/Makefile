## library versions ##

LLVM_VER = 2.9
READLINE_VER = 6.2
PCRE_VER = 8.12
DSFMT_VER = 2.1
OPENBLAS_VER = v0.1alpha2.2
LAPACK_VER = 3.3.1
ARPACK_VER = 96
FFTW_VER = 3.3
UNWIND_VER = 0.99

## high-level setup ##

JULIAHOME = $(shell pwd)/..
include ../Make.inc

OS = $(shell uname)
ARCH = $(shell uname -m)
LIBS = llvm readline pcre fdlibm dsfmt openblas lapack arpack fftw
ifeq ($(OS), Linux)
LIBS += unwind
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

llvm-$(LLVM_VER).tgz:
	curl -O http://llvm.org/releases/$(LLVM_VER)/$@
llvm-$(LLVM_VER)/configure: llvm-$(LLVM_VER).tgz
	tar zxf $<
	touch $@
$(LLVM_OBJ_SOURCE): llvm-$(LLVM_VER)/configure
	cd llvm-$(LLVM_VER) && \
	./configure --prefix=$(EXTROOT) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 --disable-bindings --disable-docs && \
	make
$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE)
	make -C llvm-$(LLVM_VER) install
	touch $@

clean-llvm:
	make -C llvm-$(LLVM_VER) clean
	rm -f $(LLVM_OBJ_TARGET)
distclean-llvm:
	rm -rf llvm-$(LLVM_VER).tgz llvm-$(LLVM_VER)

## GNU readline ##

READLINE_OBJ_TARGET = $(EXTROOTLIB)/libreadline.$(SHLIB_EXT)
READLINE_OBJ_SOURCE = readline-$(READLINE_VER)/shlib/libreadline.$(READLINE_VER).$(SHLIB_EXT)

compile-readline: $(READLINE_OBJ_SOURCE)
install-readline: $(READLINE_OBJ_TARGET)

readline-$(READLINE_VER).tar.gz:
	curl -O ftp://ftp.gnu.org/gnu/readline/$@
readline-$(READLINE_VER)/configure: readline-$(READLINE_VER).tar.gz
	tar zxf $<
	touch $@
$(READLINE_OBJ_SOURCE): readline-$(READLINE_VER)/configure
	cd readline-$(READLINE_VER) && \
	./configure --prefix=$(EXTROOT) --disable-shared --enable-static --with-curses && \
	make
	touch $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	make -C readline-$(READLINE_VER) install
	touch $@

clean-readline:
	make -C readline-$(READLINE_VER) clean
	rm -f $(READLINE_OBJ_TARGET)
distclean-readline:
	rm -rf readline-$(READLINE_VER).tar.gz readline-$(READLINE_VER)

## PCRE ##

PCRE_OBJ_TARGET = $(EXTROOTLIB)/libpcre.$(SHLIB_EXT)

compile-pcre: install-pcre
install-pcre: $(PCRE_OBJ_TARGET)

pcre-$(PCRE_VER).tar.bz2:
	curl -O ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/$@
pcre-$(PCRE_VER)/configure: pcre-$(PCRE_VER).tar.bz2
	tar jxf $<
	touch $@
pcre-$(PCRE_VER)/config.status: pcre-$(PCRE_VER)/configure
	cd pcre-$(PCRE_VER) && \
	./configure --prefix=$(EXTROOT) --enable-utf8 --enable-unicode-properties
$(PCRE_OBJ_TARGET): pcre-$(PCRE_VER)/config.status
	make -C pcre-$(PCRE_VER) install
	touch $@

clean-pcre:
	make -C pcre-$(PCRE_VER) clean
	rm -f $(PCRE_OBJ_TARGET)
distclean-pcre:
	rm -rf pcre-$(PCRE_VER).tar.bz2 pcre-$(PCRE_VER)

## fdlibm ##

FDLIBM_OBJ_TARGET = $(EXTROOTLIB)/libfdm.$(SHLIB_EXT)
FDLIBM_OBJ_SOURCE = fdlibm/libfdm.$(SHLIB_EXT)

compile-fdlibm: $(FDLIBM_OBJ_SOURCE)
install-fdlibm: $(FDLIBM_OBJ_TARGET)

$(FDLIBM_OBJ_SOURCE): fdlibm/*.c
	cd fdlibm && \
	make CC=$(CC) CFLAGS="-D_IEEE_LIBM -Dx86 -fPIC -O2 $(CONFIG)" && \
	$(CC) -shared *.o -o libfdm.$(SHLIB_EXT)
$(FDLIBM_OBJ_TARGET): $(FDLIBM_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-fdlibm:
	cd fdlibm && make clean
	rm -f $(FDLIBM_OBJ_SOURCE) $(FDLIBM_OBJ_TARGET)
distclean-fdlibm: clean-fdlibm

## DSFMT ##

DSFMT_OBJ_TARGET = $(EXTROOTLIB)/libMT.$(SHLIB_EXT)
DSFMT_OBJ_SOURCE = random/libMT.$(SHLIB_EXT)

compile-dsfmt: $(DSFMT_OBJ_SOURCE)
install-dsfmt: $(DSFMT_OBJ_TARGET)

random/dsfmt-$(DSFMT_VER).tar.gz:
	cd random && \
	curl -o dsfmt-$(DSFMT_VER).tar.gz http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz 
random/jl_random.c: random/dsfmt-$(DSFMT_VER).tar.gz
	cd random && \
	mkdir -p dsfmt-$(DSFMT_VER) && \
	tar -C dsfmt-$(DSFMT_VER) --strip-components 1 -xf dsfmt-$(DSFMT_VER).tar.gz && \
	cp dSFMT.h dsfmt-$(DSFMT_VER)
	touch $@
$(DSFMT_OBJ_SOURCE): random/jl_random.c
	cd random && \
	$(CC) -O3 -finline-functions -fomit-frame-pointer -DNDEBUG -fno-strict-aliasing --param max-inline-insns-single=1800  -Wmissing-prototypes -Wall  -std=c99 -DDSFMT_MEXP=19937 -fPIC -shared -DDSFMT_DO_NOT_USE_OLD_NAMES jl_random.c -o libMT.$(SHLIB_EXT)

$(DSFMT_OBJ_TARGET): $(DSFMT_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-dsfmt:
	rm -f random/libMT.$(SHLIB_EXT)
distclean-dsfmt: clean-dsfmt
	cd random && rm -rf *.tar.gz dsfmt-$(DSFMT_VER)

## OpenBLAS ##

OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.a

compile-openblas: $(OPENBLAS_OBJ_SOURCE)
install-openblas: compile-openblas

openblas-$(OPENBLAS_VER).tar.gz:
	curl -Lk https://github.com/xianyi/OpenBLAS/tarball/$(OPENBLAS_VER) > $@
openblas-$(OPENBLAS_VER)/Makefile: openblas-$(OPENBLAS_VER).tar.gz
	mkdir -p openblas-$(OPENBLAS_VER) && \
	tar -C openblas-$(OPENBLAS_VER) --strip-components 1 -xf $<
	touch $@
$(OPENBLAS_OBJ_SOURCE): openblas-$(OPENBLAS_VER)/Makefile
	make -C openblas-$(OPENBLAS_VER) USE_THREAD=0 NO_CBLAS=1 NO_LAPACK=1 CC=$(CC) FC=$(FC)

clean-openblas:
	make -C openblas-$(OPENBLAS_VER) clean
distclean-openblas:
	rm -rf openblas-$(OPENBLAS_VER).tar.gz openblas-$(OPENBLAS_VER)

## LAPACK ##

LAPACK_OBJ_TARGET = $(EXTROOTLIB)/libLAPACK.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE = lapack-$(LAPACK_VER)/libLAPACK.$(SHLIB_EXT)

compile-lapack: $(LAPACK_OBJ_SOURCE)
install-lapack: $(LAPACK_OBJ_TARGET)

lapack-$(LAPACK_VER).tgz:
	curl -O http://www.netlib.org/lapack/$@
lapack-$(LAPACK_VER)/Makefile: lapack-$(LAPACK_VER).tgz
	tar zxf $<
	touch $@
lapack-$(LAPACK_VER)/INSTALL/dlamch.o: lapack-$(LAPACK_VER)/Makefile
	cd lapack-$(LAPACK_VER) && \
	cp INSTALL/make.inc.gfortran ./make.inc && \
	make lapacklib NOOPT="-O0 -fPIC" OPTS="-O2 -fPIC" FORTRAN=$(FC)
$(LAPACK_OBJ_SOURCE): lapack-$(LAPACK_VER)/INSTALL/dlamch.o $(OPENBLAS_OBJ_SOURCE)
	cd lapack-$(LAPACK_VER) && \
	$(FC) -shared SRC/*.o INSTALL/dlamch.o INSTALL/dsecnd_INT_ETIME.o INSTALL/ilaver.o INSTALL/slamch.o ../$(OPENBLAS_OBJ_SOURCE) -o libLAPACK.$(SHLIB_EXT)
$(LAPACK_OBJ_TARGET): $(LAPACK_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-lapack:
	make -C lapack-$(LAPACK_VER) clean
	rm -f $(LAPACK_OBJ_SOURCE) $(LAPACK_OBJ_TARGET)
distclean-lapack:
	rm -rf lapack-$(LAPACK_VER).tgz lapack-$(LAPACK_VER)

## ARPACK ##

ARPACK_OBJ_TARGET = $(EXTROOTLIB)/libarpack.$(SHLIB_EXT)
ARPACK_OBJ_SOURCE = arpack-$(ARPACK_VER)/libarpack.$(SHLIB_EXT)

compile-arpack: $(ARPACK_OBJ_SOURCE)
install-arpack: $(ARPACK_OBJ_TARGET)

arpack-$(ARPACK_VER).tar.gz:
	curl http://www.caam.rice.edu/software/ARPACK/SRC/arpack$(ARPACK_VER).tar.gz > $@
arpack-patch.tar.gz:
	curl http://www.caam.rice.edu/software/ARPACK/SRC/patch.tar.gz > $@
arpack-$(ARPACK_VER)/Makefile: arpack-$(ARPACK_VER).tar.gz arpack-patch.tar.gz
	mkdir -p arpack-$(ARPACK_VER)
	for x in $^; do tar -C arpack-$(ARPACK_VER) --strip-components 1 -zxf $$x; done
	perl -i -nle 'print unless /EXTERNAL\s+ETIME/' arpack-$(ARPACK_VER)/UTIL/second.f
	touch $@
$(ARPACK_OBJ_SOURCE): arpack-$(ARPACK_VER)/Makefile
	cd arpack-$(ARPACK_VER) && \
	make lib MAKE=make PLAT= home=`pwd` FC=$(FC) FFLAGS="-O3 -fPIC" && \
	ar x libarpack_.a && \
	$(FC) -shared *.o -o libarpack.$(SHLIB_EXT)
$(ARPACK_OBJ_TARGET): $(ARPACK_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-arpack:
	make -C arpack-$(ARPACK_VER) home=`pwd`/arpack-$(ARPACK_VER) clean MAKE=make
	rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	rm -rf arpack-$(ARPACK_VER).tar.gz arpack-patch.tar.gz arpack-$(ARPACK_VER)

## FFTW ##

FFTW_SINGLE_OBJ_TARGET = $(EXTROOTLIB)/libfftw3f.3.$(SHLIB_EXT)
FFTW_DOUBLE_OBJ_TARGET = $(EXTROOTLIB)/libfftw3.3.$(SHLIB_EXT)

compile-fftw: compile-fftw-single compile-fftw-double
compile-fftw-single: install-fftw-single
compile-fftw-double: install-fftw-double

install-fftw: install-fftw-single install-fftw-double
install-fftw-single: $(FFTW_SINGLE_OBJ_TARGET)
install-fftw-double: $(FFTW_DOUBLE_OBJ_TARGET)

fftw-$(FFTW_VER).tar.gz:
	curl -O http://www.fftw.org/$@

fftw-$(FFTW_VER)-single/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-single && \
	tar -C fftw-$(FFTW_VER)-single --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-single/config.status: fftw-$(FFTW_VER)-single/configure
	cd fftw-$(FFTW_VER)-single && \
	./configure --prefix=$(EXTROOT) --enable-shared --disable-threads --enable-float && \
	make clean
	touch $@
$(FFTW_SINGLE_OBJ_TARGET): fftw-$(FFTW_VER)-single/config.status
	make -C fftw-$(FFTW_VER)-single install
	touch $@

fftw-$(FFTW_VER)-double/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-double && \
	tar -C fftw-$(FFTW_VER)-double --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-double/config.status: fftw-$(FFTW_VER)-double/configure
	cd fftw-$(FFTW_VER)-double && \
	./configure --prefix=$(EXTROOT) --enable-shared --disable-threads && \
	make clean
	touch $@
$(FFTW_DOUBLE_OBJ_TARGET): fftw-$(FFTW_VER)-double/config.status
	make -C fftw-$(FFTW_VER)-double install
	touch $@

clean-fftw: clean-fftw-single clean-fftw-double
clean-fftw-single:
	make -C fftw-$(FFTW_VER)-single clean
	rm -f $(FFTW_SINGLE_OBJ_TARGET)
clean-fftw-double:
	make -C fftw-$(FFTW_VER)-double clean
	rm -f $(FFTW_DOUBLE_OBJ_TARGET)
distclean-fftw:
	rm -rf fftw-$(FFTW_VER).tar.gz fftw-$(FFTW_VER)-{single,double}

## UNWIND ##

LIBUNWIND_TARGET_OBJ = $(EXTROOTLIB)/libunwind.a
LIBUNWIND_TARGET_SOURCE = $(JULIAHOME)/external/libunwind-$(UNWIND_VER)/Makefile

compile-unwind: $(LIBUNWIND_TARGET_SOURCE)
install-unwind: $(LIBUNWIND_TARGET_OBJ)

$(LIBUNWIND_TARGET_OBJ): $(LIBUNWIND_TARGET_SOURCE)
	cd libunwind-$(UNWIND_VER) && make && make install


libunwind-$(UNWIND_VER).tar.gz:
	curl -O http://savannah.spinellicreations.com/libunwind/libunwind-$(UNWIND_VER).tar.gz 

open-unwind: libunwind-$(UNWIND_VER).tar.gz
	tar xvfz $<
	touch $@

$(LIBUNWIND_TARGET_SOURCE): open-unwind
	cd libunwind-$(UNWIND_VER) && ./configure  CFLAGS="-U_FORTIFY_SOURCE -fPIC" --prefix=$(EXTROOT) 

clean-unwind:
	make -C libunwind-$(UNWIND_VER) clean
	rm -rf $(LIBUNWIND_TARGET_OBJ) $(LIBUNWIND_TARGET_SOURCE)

distclean-unwind:
	rm -rf libunwind-$(UNWIND_VER).tar.gz libunwind-$(UNWIND_VER)

## phony targets ##

.PHONY: \
	default compile install cleanall distclean \
	compile-* install-* clean-* distclean-*
