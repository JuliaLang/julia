include ../Make.inc.$(shell uname)

all: llvm readline pcre fdlibm lapack

llvm:
	curl -O http://llvm.org/releases/2.7/llvm-2.7.tgz && tar zxvf llvm-2.7.tgz && cd llvm-2.7 && ./configure --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 && make install

readline:
	curl -O ftp://ftp.gnu.org/gnu/readline/readline-6.1.tar.gz && tar zxvf readline-6.1.tar.gz && cd readline-6.1 && ./configure && make install

pcre:
	curl -O ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.10.tar.bz2 && tar jxvf pcre-8.10.tar.bz2 && cd pcre-8.10 && ./configure --enable-utf8 --enable-unicode-properties && make install

fdlibm:
	curl -O -u anonymous:julia@juliamath.com ftp://netlib.org/fdlibm.tar.gz && tar zxf fdlibm.tar.gz && cd fdlibm && make CC=$(CC) CFLAGS="-D_IEEE_LIBM -fPIC"

lapack:
	curl -O http://www.netlib.org/lapack/lapack-3.3.0.tgz && cd lapack-3.3.0 && cp INSTALL/make.inc.gfortran ./make.inc && make blaslib lapacklib OPTS="-O2 -fPIC" FORTRAN=$(FC)

clean:
	rm -fr readline-6.1 llvm-2.7 pcre-8.10 fdlibm lapack-3.3.0 *gz
