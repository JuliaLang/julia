all: llvm readline

llvm:
	curl -O http://llvm.org/releases/2.7/llvm-2.7.tgz && tar zxvf llvm-2.7.tgz && cd llvm-2.7 && ./configure --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 && sudo make install

readline:
	curl -O ftp://ftp.gnu.org/gnu/readline/readline-6.1.tar.gz && tar zxvf readline-6.1.tar.gz && cd readline-6.1 && ./configure && sudo make install

clean:
	rm -fr readline-6.1 llvm-2.7
