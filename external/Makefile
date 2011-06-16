JULIAHOME = $(shell pwd)/..
include ../Make.inc

OS = $(shell uname)
ARCH = $(shell uname -m)

LINUX_LIBS  = llvm pcre fdlibm lapack arpack mongoose
DARWIN_LIBS = llvm pcre fdlibm lapack arpack mongoose readline

ifeq ($(OS), Linux)
LIBS = $(LINUX_LIBS)
endif
ifeq ($(OS), Darwin)
LIBS = $(DARWIN_LIBS)
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
	$(DOWNLOAD) http://llvm.org/releases/$(LLVM_VER)/$@
llvm-$(LLVM_VER)/Makefile: llvm-$(LLVM_VER).tgz
	tar zxf $<
	touch $@
$(LLVM_OBJ_SOURCE): llvm-$(LLVM_VER)/Makefile
	cd llvm-$(LLVM_VER) && \
	./configure --prefix=$(EXTROOT) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 --disable-bindings --disable-docs && \
	make
$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cd llvm-$(LLVM_VER) && make install
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
	$(DOWNLOAD) ftp://ftp.gnu.org/gnu/readline/$@
readline-$(READLINE_VER)/Makefile: readline-$(READLINE_VER).tar.gz
	tar zxf $<
	touch $@
$(READLINE_OBJ_SOURCE): readline-$(READLINE_VER)/Makefile
	cd readline-$(READLINE_VER) && \
	./configure --prefix=$(EXTROOT) && make
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cd readline-$(READLINE_VER) && make install
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
	$(DOWNLOAD) ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/$@
pcre-$(PCRE_VER)/Makefile: pcre-$(PCRE_VER).tar.bz2
	tar jxvf $<
	touch $@
$(PCRE_OBJ_TARGET): pcre-$(PCRE_VER)/Makefile
	mkdir -p $(EXTROOTLIB)
	cd pcre-$(PCRE_VER) && \
	./configure --prefix=$(EXTROOT) --enable-utf8 --enable-unicode-properties && \
	make && make install
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
	make CC=$(CC) CFLAGS="-D_IEEE_LIBM -Dx86 -fPIC -O1 $(CONFIG)" && \
	$(CC) -shared *.o -o libfdm.$(SHLIB_EXT)
$(FDLIBM_OBJ_TARGET): $(FDLIBM_OBJ_SOURCE)
	mkdir -p $(EXTROOTLIB)
	cp $< $@

clean-fdlibm:
	cd fdlibm && make clean
	rm -f $(FDLIBM_OBJ_SOURCE) $(FDLIBM_OBJ_TARGET)
distclean-fdlibm: clean-fdlibm

## OpenBLAS ##

OPENBLAS_OBJ_SOURCE = openblas/libopenblas.a

compile-openblas: $(OPENBLAS_OBJ_SOURCE)

openblas.tar.gz:
	$(DOWNLOAD) -Lk https://github.com/xianyi/OpenBLAS/tarball/develop
	mv develop $@
openblas/Makefile: openblas.tar.gz
	mkdir -p openblas && tar --strip-components 1 -C openblas -xf $<
	touch $@
$(OPENBLAS_OBJ_SOURCE): openblas/Makefile
	cd openblas && \
	make USE_THREAD=0 NO_CBLAS=1 NO_LAPACK=1 CC=$(CC) FC=$(FC)

clean-openblas:
	make -C openblas clean
distclean-openblas:
	rm -rf openblas.tar.gz openblas

## LAPACK ##

LAPACK_OBJ_TARGET = $(EXTROOTLIB)/libLAPACK.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE = lapack-$(LAPACK_VER)/libLAPACK.$(SHLIB_EXT)

compile-lapack: $(LAPACK_OBJ_SOURCE)
install-lapack: $(LAPACK_OBJ_TARGET)

lapack-$(LAPACK_VER).tgz:
	$(DOWNLOAD) http://www.netlib.org/lapack/lapack-$(LAPACK_VER).tgz
lapack-$(LAPACK_VER)/Makefile: lapack-$(LAPACK_VER).tgz
	tar zxf $<
	touch $@
$(LAPACK_OBJ_SOURCE): lapack-$(LAPACK_VER)/Makefile $(OPENBLAS_OBJ_SOURCE)
	cd lapack-$(LAPACK_VER) && \
	cp INSTALL/make.inc.gfortran ./make.inc && \
	make lapacklib NOOPT="-O0 -fPIC" OPTS="-O2 -fPIC" FORTRAN=$(FC) && \
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
	$(DOWNLOAD) http://www.caam.rice.edu/software/ARPACK/SRC/arpack$(ARPACK_VER).tar.gz
arpack-patch.tar.gz:
	$(DOWNLOAD) http://www.caam.rice.edu/software/ARPACK/SRC/patch.tar.gz
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

## mongoose ##

compile-mongoose: mongoose/Makefile
install-mongoose: compile-mongoose

mongoose-$(MONGOOSE_VER).tgz:
	$(DOWNLOAD) http://mongoose.googlecode.com/files/mongoose-$(MONGOOSE_VER).tgz
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
