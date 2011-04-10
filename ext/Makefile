JULIAHOME = $(shell pwd)/..
include ../Make.inc

all: llvm readline pcre fdlibm lapack

llvm:
	curl -O http://llvm.org/releases/2.9/llvm-2.9.tgz && tar zxf llvm-2.9.tgz && cd llvm-2.9 && ./configure --prefix=$(EXTROOT) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 --disable-bindings --disable-docs && make install

readline:
	curl -O ftp://ftp.gnu.org/gnu/readline/readline-6.2.tar.gz && tar zxf readline-6.2.tar.gz && cd readline-6.2 && ./configure --prefix=$(EXTROOT) && make install

readline_install:
	ln -sf $(EXTROOTLIB)/libreadline.$(SHLIB_EXT) $(JULIALIB)/libreadline.$(SHLIB_EXT)
	ln -sf $(EXTROOTLIB)/libreadline.6.$(SHLIB_EXT) $(JULIALIB)/libreadline.6.$(SHLIB_EXT)
	ln -sf $(EXTROOTLIB)/libreadline.6.2.$(SHLIB_EXT) $(JULIALIB)/libreadline.6.2.$(SHLIB_EXT)

pcre:
	curl -O ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.12.tar.bz2 && tar jxvf pcre-8.12.tar.bz2 && cd pcre-8.12 && ./configure --prefix=$(EXTROOT) --enable-utf8 --enable-unicode-properties && make install

pcre_install:
	ln -sf $(EXTROOTLIB)/libpcre.$(SHLIB_EXT) $(JULIALIB)/libpcre.$(SHLIB_EXT)
	ln -sf $(EXTROOTLIB)/libpcreposix.$(SHLIB_EXT) $(JULIALIB)/libpcreposix.$(SHLIB_EXT)

fdlibm:
	curl -O -u anonymous:julia@juliamath.com ftp://netlib.org/fdlibm.tar.gz && tar zxf fdlibm.tar.gz && cd fdlibm && make CC=$(CC) CFLAGS="-D_IEEE_LIBM -fPIC" && $(CC) -shared *.o -o $(EXTROOT)/libfdm.$(SHLIB_EXT)

fdlibm_install:
	ln -sf $(EXTROOTLIB)/libfdm.$(SHLIB_EXT) $(JULIALIB)/libfdm.$(SHLIB_EXT)

lapack:
	curl -O http://www.netlib.org/lapack/lapack-3.3.0.tgz && cd lapack-3.3.0 && cp INSTALL/make.inc.gfortran ./make.inc && make blaslib lapacklib OPTS="-O2 -fPIC" FORTRAN=$(FC) && $(FC) -shared BLAS/SRC/*.o -o $(EXTROOT)/libBLAS.$(SHLIB_EXT) && $(FC) -shared lapack_LINUX.a -o $(EXTROOT)/libLAPACK.$(SHLIB_EXT)

blas_install:
	ln -sf $(EXTROOTLIB)/libBLAS.$(SHLIB_EXT) $(JULIALIB)/libBLAS.$(SHLIB_EXT)

lapack_install:
	ln -sf $(EXTROOTLIB)/libLAPACK.$(SHLIB_EXT) $(JULIALIB)/libLAPACK.$(SHLIB_EXT)

clean:
	rm -fr readline-6.2 llvm-2.9 pcre-8.12 fdlibm lapack-3.3.0 *gz
