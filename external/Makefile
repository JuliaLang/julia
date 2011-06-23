## Library Versions ##

LLVM_VER = 2.9
READLINE_VER = 6.2
PCRE_VER = 8.12
OPENBLAS_VER = develop
LAPACK_VER = 3.3.1
ARPACK_VER = 96
FFTW_VER = 3.2.2
MONGOOSE_VER = 3.0

## High-Level Setup ##

JULIAHOME = $(shell pwd)/..
include ../Make.inc

OS = $(shell uname)
ARCH = $(shell uname -m)
LIBS = llvm pcre fdlibm openblas lapack arpack fftw mongoose readline

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
	./configure --prefix=$(EXTROOT) && \
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
	make CC=$(CC) CFLAGS="-D_IEEE_LIBM -Dx86 -fPIC -O2 -mtune=native $(CONFIG)" && \
	$(CC) -shared *.o -o libfdm.$(SHLIB_EXT)
$(FDLIBM_OBJ_TARGET): $(FDLIBM_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-fdlibm:
	cd fdlibm && make clean
	rm -f $(FDLIBM_OBJ_SOURCE) $(FDLIBM_OBJ_TARGET)
distclean-fdlibm: clean-fdlibm

## OpenBLAS ##

OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.a

compile-openblas: $(OPENBLAS_OBJ_SOURCE)
install-openblas: compile-openblas

openblas-$(OPENBLAS_VER).tar.gz:
	curl -Lk https://github.com/xianyi/OpenBLAS/tarball/$(OPENBLAS_VER) > $@
openblas-$(OPENBLAS_VER)/Makefile: openblas-$(OPENBLAS_VER).tar.gz
	mkdir -p openblas-$(OPENBLAS_VER) && tar -C openblas-$(OPENBLAS_VER) --strip-components 1 -xf $<
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
ARPACK_OBJ_SOURCE = ARPACK/libarpack.$(SHLIB_EXT)

compile-arpack: $(ARPACK_OBJ_SOURCE)
install-arpack: $(ARPACK_OBJ_TARGET)

arpack$(ARPACK_VER).tar.gz:
	curl -O http://www.caam.rice.edu/software/ARPACK/SRC/$@
arpack-patch.tar.gz:
	curl -O http://www.caam.rice.edu/software/ARPACK/SRC/patch.tar.gz
	mv patch.tar.gz $@
ARPACK/Makefile: arpack$(ARPACK_VER).tar.gz arpack-patch.tar.gz
	for x in $^; do tar zxf $$x; done
	perl -i -nle 'print unless /EXTERNAL\s+ETIME/' ARPACK/UTIL/second.f
	touch $@
$(ARPACK_OBJ_SOURCE): ARPACK/Makefile
	cd ARPACK && \
	make lib MAKE=make PLAT= home=`pwd` FC=$(FC) FFLAGS="-O3 -fPIC" && \
	ar x libarpack_.a && \
	$(FC) -shared *.o -o libarpack.$(SHLIB_EXT)
$(ARPACK_OBJ_TARGET): $(ARPACK_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-arpack:
	make -C ARPACK clean
	rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	rm -rf arpack$(ARPACK_VER).tar.gz arpack-patch.tar.gz ARPACK

## FFTW ##

FFTW_OBJ_TARGET = $(EXTROOTLIB)/libfftw3.3.$(SHLIB_EXT)

compile-fftw: install-fftw
install-fftw: $(FFTW_OBJ_TARGET)

fftw-$(FFTW_VER).tar.gz:
	curl -O http://www.fftw.org/$@
fftw-$(FFTW_VER)/configure: fftw-$(FFTW_VER).tar.gz
	tar zxf $<
	touch $@
fftw-$(FFTW_VER)/config.status: fftw-$(FFTW_VER)/configure
	cd fftw-$(FFTW_VER) && \
	./configure --prefix=$(EXTROOT) --enable-shared && \
	make clean
	touch $@
$(FFTW_OBJ_TARGET): fftw-$(FFTW_VER)/config.status
	make -C fftw-$(FFTW_VER) install
	touch $@

clean-fftw:
	make -C fftw-$(FFTW_VER) clean
	rm -f $(FFTW_OBJ_TARGET)
distclean-fftw:
	rm -rf fftw-$(FFTW_VER).tar.gz fftw-$(FFTW_VER)

## mongoose ##

compile-mongoose: mongoose/Makefile
install-mongoose: compile-mongoose

mongoose-$(MONGOOSE_VER).tgz:
	curl -O http://mongoose.googlecode.com/files/mongoose-$(MONGOOSE_VER).tgz
mongoose/Makefile: mongoose-$(MONGOOSE_VER).tgz
	tar zxf $<
	touch $@

clean-mongoose:
distclean-mongoose:
	rm -rf mongoose-$(MONGOOSE_VER).tgz mongoose

## phony targets ##

.PHONY: \
	default compile install cleanall distclean \
	compile-* install-* clean-* distclean-*
