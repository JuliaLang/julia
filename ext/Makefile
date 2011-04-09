include ../Make.inc.$(shell uname)
ROOT=$(shell pwd)/root

all: llvm readline pcre fdlibm lapack

llvm:
	curl -O http://llvm.org/releases/2.7/llvm-2.7.tgz && tar zxf llvm-2.7.tgz && cd llvm-2.7 && ./configure --prefix=$(ROOT) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 && make install

readline:
	curl -O ftp://ftp.gnu.org/gnu/readline/readline-6.2.tar.gz && tar zxf readline-6.2.tar.gz && cd readline-6.2 && ./configure --prefix=$(ROOT) && make install

pcre:
	curl -O ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.12.tar.bz2 && tar jxvf pcre-8.12.tar.bz2 && cd pcre-8.12 && ./configure --prefix=$(ROOT) --enable-utf8 --enable-unicode-properties && make install

fdlibm:
	curl -O -u anonymous:julia@juliamath.com ftp://netlib.org/fdlibm.tar.gz && tar zxf fdlibm.tar.gz && cd fdlibm && make CC=$(CC) CFLAGS="-D_IEEE_LIBM -fPIC" && $(CC) -shared libm.a -o $(ROOT)/libfdm.$(SHLIB_EXT)

lapack:
	curl -O http://www.netlib.org/lapack/lapack-3.3.0.tgz && cd lapack-3.3.0 && cp INSTALL/make.inc.gfortran ./make.inc && make blaslib lapacklib OPTS="-O2 -fPIC" FORTRAN=$(FC) && $(CC) -shared blas_LINUX.a -o $(ROOT)/libBLAS.$(SHLIB_EXT) && $(CC) -shared lapack_LINUX.a -o $(ROOT)/libLAPACK.$(SHLIB_EXT)

clean:
	rm -fr readline-6.2 llvm-2.7 pcre-8.12 fdlibm lapack-3.3.0 *gz
