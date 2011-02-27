all: llvm readline pcre

llvm:
	curl -O http://llvm.org/releases/2.7/llvm-2.7.tgz && tar zxvf llvm-2.7.tgz && cd llvm-2.7 && ./configure --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 && sudo make install

readline:
	curl -O ftp://ftp.gnu.org/gnu/readline/readline-6.1.tar.gz && tar zxvf readline-6.1.tar.gz && cd readline-6.1 && ./configure && sudo make install

pcre:
	curl -O ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.10.tar.bz2 && tar jxvf pcre-8.10.tar.bz2 && cd pcre-8.10 && ./configure --enable-utf8 --enable-unicode-properties && sudo make install

clean:
	rm -fr readline-6.1 llvm-2.7 pcre-8.10

