JULIAHOME = $(shell pwd)/..
include ../Make.inc

OS = $(shell uname)
ARCH = $(shell uname -m)

ifeq ($(ARCH), $(filter $(ARCH), i386 i486 i586 i686))
LIBC = /lib/libc.so.6
endif

ifeq ($(ARCH), x86_64)
LIBC = /lib64/libc.so.6
endif

default: defaultlibs_$(OS)

defaultlibs_Linux: \
	llvm pcre_install fdlibm_install lapack_install libc_install \
	arpack_install mongoose_install

defaultlibs_Darwin: \
	llvm readline_install pcre_install fdlibm_install lapack_install \
	arpack_install mongoose_install

$(JULIALIB):
	mkdir -p $(JULIALIB)

libc_install: $(JULIALIB)
	ln -sf $(LIBC) $(JULIALIB)/libc.so

LLVM_OBJ = $(EXTROOTLIB)/libLLVM-$(LLVM_VER).$(SHLIB_EXT)
llvm-$(LLVM_VER).tgz:
	curl -O http://llvm.org/releases/$(LLVM_VER)/llvm-$(LLVM_VER).tgz
llvm-$(LLVM_VER): llvm-$(LLVM_VER).tgz
	tar zxf llvm-$(LLVM_VER).tgz
$(LLVM_OBJ): llvm-$(LLVM_VER)
	cd llvm-$(LLVM_VER) && ./configure --prefix=$(EXTROOT) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 --disable-bindings --disable-docs && make install
llvm: $(LLVM_OBJ)

READLINE_OBJ = $(EXTROOTLIB)/libreadline.$(SHLIB_EXT)
readline-$(READLINE_VER).tar.gz:
	$(DOWNLOAD) ftp://ftp.gnu.org/gnu/readline/readline-$(READLINE_VER).tar.gz
readline-$(READLINE_VER): readline-$(READLINE_VER).tar.gz
	tar zxf readline-$(READLINE_VER).tar.gz
$(READLINE_OBJ): readline-$(READLINE_VER)
	cd readline-$(READLINE_VER) && ./configure --prefix=$(EXTROOT) && make install
readline_install: $(READLINE_OBJ) $(JULIALIB)
	ln -sf $(READLINE_OBJ) $(JULIALIB)/libreadline.$(SHLIB_EXT)
	ln -sf $(EXTROOTLIB)/libhistory.$(SHLIB_EXT) $(JULIALIB)/libhistory.$(SHLIB_EXT)

PCRE_OBJ = $(EXTROOTLIB)/libpcre.$(SHLIB_EXT)
pcre-$(PCRE_VER).tar.bz2:
	$(DOWNLOAD) ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-$(PCRE_VER).tar.bz2
pcre-$(PCRE_VER): pcre-$(PCRE_VER).tar.bz2
	tar jxvf pcre-$(PCRE_VER).tar.bz2
$(PCRE_OBJ): pcre-$(PCRE_VER)
	cd pcre-$(PCRE_VER) && ./configure --prefix=$(EXTROOT) --enable-utf8 --enable-unicode-properties && make install
pcre_install: $(PCRE_OBJ) $(JULIALIB)
	ln -sf $(PCRE_OBJ) $(JULIALIB)/libpcre.$(SHLIB_EXT)
	ln -sf $(EXTROOTLIB)/libpcreposix.$(SHLIB_EXT) $(JULIALIB)/libpcreposix.$(SHLIB_EXT)

fdlibmf: fdlibmf/libmf.a
fdlibmf/libmf.a: fdlibmf/*.c
	cd fdlibmf && make CC=$(CC) CFLAGS="-D_IEEE_LIBM -Dx86 -fPIC -O1 $(CONFIG)"

FDLIBM_OBJ = $(EXTROOTLIB)/libfdm.$(SHLIB_EXT)
fdlibm.tar.gz:
	$(DOWNLOAD) -u anonymous:julia@juliamath.com ftp://netlib.org/fdlibm.tar.gz
fdlibm: fdlibm.tar.gz
	tar zxf fdlibm.tar.gz
$(FDLIBM_OBJ): fdlibm fdlibmf
	cd fdlibm && make CC=$(CC) CFLAGS="-D_IEEE_LIBM -Dx86 -fPIC -O1 $(CONFIG)" && $(CC) -shared *.o ../fdlibmf/*.o -o $@
fdlibm_install: $(FDLIBM_OBJ) $(JULIALIB)
	ln -sf $< $(JULIALIB)/libfdm.$(SHLIB_EXT)

LAPACK_OBJ = $(EXTROOTLIB)/libLAPACK.$(SHLIB_EXT)
lapack-$(LAPACK_VER).tgz:
	$(DOWNLOAD) http://www.netlib.org/lapack/lapack-$(LAPACK_VER).tgz
lapack-$(LAPACK_VER): lapack-$(LAPACK_VER).tgz
	tar zxf lapack-$(LAPACK_VER).tgz
$(LAPACK_OBJ): lapack-$(LAPACK_VER)
	cd lapack-$(LAPACK_VER) && \
	cp INSTALL/make.inc.gfortran ./make.inc && \
	make blaslib lapacklib NOOPT="-O0 -fPIC" OPTS="-O2 -fPIC" FORTRAN=$(FC) && \
	rm -f BLAS/SRC/xerbla_array.o && rm -f BLAS/SRC/xerbla.o && \
	$(FC) -shared SRC/*.o BLAS/SRC/*.o INSTALL/dlamch.o INSTALL/dsecnd_INT_ETIME.o INSTALL/ilaver.o INSTALL/slamch.o -o $(LAPACK_OBJ) -L$(EXTROOTLIB)
lapack_install: $(LAPACK_OBJ) $(JULIALIB)
	ln -sf $(LAPACK_OBJ) $(JULIALIB)/libLAPACK.$(SHLIB_EXT)

ARPACK_OBJ = $(EXTROOTLIB)/libarpack.$(SHLIB_EXT)
arpack$(ARPACK_VER).tar.gz:
	$(DOWNLOAD) http://www.caam.rice.edu/software/ARPACK/SRC/arpack$(ARPACK_VER).tar.gz
	$(DOWNLOAD) http://www.caam.rice.edu/software/ARPACK/SRC/patch.tar.gz
arpack$(ARPACK_VER): arpack$(ARPACK_VER).tar.gz
	tar zxf arpack$(ARPACK_VER).tar.gz
	tar zxf patch.tar.gz
	sed -i.bak -e s/"EXTERNAL           ETIME"// ./ARPACK/UTIL/second.f
$(ARPACK_OBJ): arpack$(ARPACK_VER)
	cd ARPACK && make lib MAKE=make PLAT= home=`pwd` FC=$(FC) FFLAGS="-O3 -fPIC" && \
	ar x libarpack_.a && \
	$(FC) -shared *.o -o $(EXTROOTLIB)/libarpack.$(SHLIB_EXT)
arpack_install: $(ARPACK_OBJ) $(JULIALIB)
	ln -sf $(ARPACK_OBJ) $(JULIALIB)/libarpack.$(SHLIB_EXT)

mongoose-$(MONGOOSE_VER).tgz:
	$(DOWNLOAD) http://mongoose.googlecode.com/files/mongoose-$(MONGOOSE_VER).tgz
mongoose-$(MONGOOSE_VER): mongoose-$(MONGOOSE_VER).tgz
	tar zxf mongoose-$(MONGOOSE_VER).tgz
mongoose_install: mongoose-$(MONGOOSE_VER)

cleanall:
	rm -rf llvm-$(LLVM_VER){,.tgz}
	rm -rf readline-$(READLINE_VER){,.tar.gz}
	rm -rf pcre-$(PCRE_VER){,.tar.bz2}
	rm -rf fdlibm{,.tar.gz}
	rm -rf lapack-$(LAPACK_VER){,.tgz}
	rm -rf mongoose-$(MONGOOSE_VER){,.tgz}
