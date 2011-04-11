JULIAHOME = $(shell pwd)/..
include ../Make.inc

OS = $(shell uname)

default: defaultlibs_$(OS)

defaultlibs_Linux: llvm pcre_install fdlibm_install lapack_install libc_install

defaultlibs_Darwin: llvm readline_install pcre_install fdlibm_install lapack_install libc_install

$(JULIALIB):
	mkdir -p $(JULIALIB)

libc_install: $(JULIALIB)
	ln -sf /lib/libc.so.6 $(JULIALIB)/libc.so

LLVM_VER = 2.9
LLVM_OBJ = $(EXTROOTLIB)/libLLVM-$(LLVM_VER).$(SHLIB_EXT)
llvm-$(LLVM_VER).tgz:
	curl -O http://llvm.org/releases/$(LLVM_VER)/llvm-$(LLVM_VER).tgz
llvm-$(LLVM_VER): llvm-$(LLVM_VER).tgz
	tar zxf llvm-$(LLVM_VER).tgz
$(LLVM_OBJ): llvm-$(LLVM_VER)
	cd llvm-$(LLVM_VER) && ./configure --prefix=$(EXTROOT) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 --disable-bindings --disable-docs && make install
llvm: $(LLVM_OBJ)

READLINE_OBJ = $(EXTROOTLIB)/libreadline.$(SHLIB_EXT)
readline-6.2.tar.gz:
	curl -O ftp://ftp.gnu.org/gnu/readline/readline-6.2.tar.gz
readline-6.2: readline-6.2.tar.gz
	tar zxf readline-6.2.tar.gz
$(READLINE_OBJ): readline-6.2
	cd readline-6.2 && ./configure --prefix=$(EXTROOT) && make install
readline_install: $(READLINE_OBJ) $(JULIALIB)
	ln -sf $(READLINE_OBJ) $(JULIALIB)/libreadline.$(SHLIB_EXT)
	ln -sf $(EXTROOTLIB)/libhistory.$(SHLIB_EXT) $(JULIALIB)/libhistory.$(SHLIB_EXT)

PCRE_OBJ = $(EXTROOTLIB)/libpcre.$(SHLIB_EXT)
pcre-8.12.tar.bz2:
	curl -O ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.12.tar.bz2
pcre-8.12: pcre-8.12.tar.bz2
	tar jxvf pcre-8.12.tar.bz2
$(PCRE_OBJ): pcre-8.12
	cd pcre-8.12 && ./configure --prefix=$(EXTROOT) --enable-utf8 --enable-unicode-properties && make install
pcre_install: $(PCRE_OBJ) $(JULIALIB)
	ln -sf $(PCRE_OBJ) $(JULIALIB)/libpcre.$(SHLIB_EXT)
	ln -sf $(EXTROOTLIB)/libpcreposix.$(SHLIB_EXT) $(JULIALIB)/libpcreposix.$(SHLIB_EXT)

fdlibmf: fdlibmf/libmf.a
fdlibmf/libmf.a: fdlibmf/*.c
	cd fdlibmf && make CC=$(CC) CFLAGS="-D_IEEE_LIBM -Dx86 -fPIC"

FDLIBM_OBJ = $(EXTROOTLIB)/libfdm.$(SHLIB_EXT)
fdlibm.tar.gz:
	curl -O -u anonymous:julia@juliamath.com ftp://netlib.org/fdlibm.tar.gz
fdlibm: fdlibm.tar.gz
	tar zxf fdlibm.tar.gz
$(FDLIBM_OBJ): fdlibm fdlibmf
	cd fdlibm && make CC=$(CC) CFLAGS="-D_IEEE_LIBM -Dx86 -fPIC" && $(CC) -shared *.o ../fdlibmf/*.o -o $@
fdlibm_install: $(FDLIBM_OBJ) $(JULIALIB)
	ln -sf $< $(JULIALIB)/libfdm.$(SHLIB_EXT)

LAPACK_OBJ = $(EXTROOTLIB)/libLAPACK.$(SHLIB_EXT)
lapack-3.3.0.tgz:
	curl -O http://www.netlib.org/lapack/lapack-3.3.0.tgz
lapack-3.3.0: lapack-3.3.0.tgz
	tar zxf lapack-3.3.0.tgz
$(LAPACK_OBJ): lapack-3.3.0
	cd lapack-3.3.0 && \
	cp INSTALL/make.inc.gfortran ./make.inc && \
	make blaslib lapacklib NOOPT="-O0 -fPIC" OPTS="-O2 -fPIC" FORTRAN=$(FC) && \
	rm -f BLAS/SRC/xerbla_array.o && rm -f BLAS/SRC/xerbla.o && \
	$(FC) -shared SRC/*.o BLAS/SRC/*.o INSTALL/dlamch.o INSTALL/dsecnd_INT_ETIME.o INSTALL/ilaver.o INSTALL/slamch.o -o $(LAPACK_OBJ) -L$(EXTROOTLIB)
lapack_install: $(LAPACK_OBJ) $(JULIALIB)
	ln -sf $(LAPACK_OBJ) $(JULIALIB)/libLAPACK.$(SHLIB_EXT)

clean:
	rm -fr readline-6.2 llvm-$(LLVM_VER) pcre-8.12 fdlibm lapack-3.3.0 *gz
