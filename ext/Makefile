all: llvm gc readline

llvm:
	curl -O http://llvm.org/releases/2.7/llvm-2.7.tgz && cd llvm-2.7 && ./configure --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=x86,x86_64 && sudo make install

gc:
	curl -O http://www.hpl.hp.com/personal/Hans_Boehm/gc/gc_source/gc-7.1.tar.gz && cd gc-7.1 && ./configure CFLAGS="-D_XOPEN_SOURCE" --disable-threads && sudo make install

readline:
	curl -O ftp://ftp.gnu.org/gnu/readline/readline-6.1.tar.gz && cd readline-6.1 && ./configure && sudo make install


