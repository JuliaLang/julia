include Versions.make

## optional packages ##
# set to 1 to get clang and compiler-rt
BUILD_LLVM_CLANG = 0

## high-level setup ##

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc

# prevent installing libs into usr/lib64 on opensuse
unexport CONFIG_SITE

STAGE1_DEPS = uv fdlibm dsfmt rmath double-conversion amos glpk
STAGE2_DEPS = gmp-wrapper
STAGE3_DEPS = suitesparse-wrapper

ifeq ($(OS), Linux)
STAGE1_DEPS += unwind
endif

ifeq ($(OS), FreeBSD)
STAGE1_DEPS += unwind
endif

WGET = $(abspath .)/jldownload
WGET_DASH_O = $(WGET)

## USE_SYSTEM_LIBS options

ifeq ($(USE_SYSTEM_LLVM), 0)
STAGE1_DEPS += llvm
endif

ifneq ($(OS),WINNT)
STAGE1_DEPS += lighttpd
endif

ifeq ($(OS),WINNT)
READLINE_VER = 5.0
endif

ifeq ($(USE_SYSTEM_READLINE), 0)
STAGE1_DEPS += readline
endif

ifeq ($(USE_SYSTEM_PCRE), 0)
STAGE1_DEPS += pcre
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
STAGE1_DEPS += openblas
endif

ifeq ($(USE_SYSTEM_FFTW), 0)
STAGE1_DEPS += fftw
endif

ifeq ($(USE_SYSTEM_GMP), 0)
STAGE1_DEPS += gmp
endif

ifeq ($(USE_SYSTEM_ZLIB), 0)
STAGE1_DEPS += zlib
endif

ifeq ($(USE_SYSTEM_ARPACK), 0)
STAGE2_DEPS += arpack
endif

#ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
STAGE2_DEPS += suitesparse
#endif

#ifeq ($(USE_SYSTEM_LAPACK), 0)
#STAGE2_DEPS += lapack
#endif

## Common build target prefixes

LIBS = $(STAGE1_DEPS) $(STAGE2_DEPS) $(STAGE3_DEPS)

default: $(USR) install
compile: $(addprefix compile-, $(LIBS))
install: $(addprefix install-, $(LIBS))
cleanall: $(addprefix clean-, $(LIBS))
distclean: $(addprefix distclean-, $(LIBS))
	rm -rf $(USR)

## PATHS ##
DIRS = $(addprefix $(USR)/,lib include bin share etc)

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))

$(USR): $(DIRS)

## LLVM ##

LLVM_OBJ_TARGET = $(USRLIB)/libLLVM-$(LLVM_VER).$(SHLIB_EXT)
ifeq ($(OS),WINNT)
	LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/Release/bin/LLVM-$(LLVM_VER).$(SHLIB_EXT)
else
	LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/Release/lib/libLLVM-$(LLVM_VER).$(SHLIB_EXT)
endif

compile-llvm: $(LLVM_OBJ_SOURCE)
install-llvm: $(LLVM_OBJ_TARGET)

ifeq ($(LLVM_VER), 3.0)
LLVM_TAR=llvm-$(LLVM_VER).tar.gz
else
LLVM_TAR=llvm-$(LLVM_VER).src.tar.gz
endif

ifneq ($(BUILD_LLVM_CLANG), 1)
LLVM_CLANG_TAR=
LLVM_COMPILER_RT_TAR=
else

ifeq ($(LLVM_VER), 3.0)
LLVM_CLANG_TAR=clang-$(LLVM_VER).tar.gz
LLVM_COMPILER_RT_TAR=
else
LLVM_CLANG_TAR=clang-$(LLVM_VER).src.tar.gz
LLVM_COMPILER_RT_TAR=compiler-rt-$(LLVM_VER).src.tar.gz
endif

endif

ifneq ($(LLVM_CLANG_TAR),)
$(LLVM_CLANG_TAR):
	$(WGET) http://llvm.org/releases/$(LLVM_VER)/$@
endif

ifneq ($(LLVM_COMPILER_RT_TAR),)
$(LLVM_COMPILER_RT_TAR):
	$(WGET) http://llvm.org/releases/$(LLVM_VER)/$@
endif


$(LLVM_TAR):
	$(WGET) http://llvm.org/releases/$(LLVM_VER)/$@

llvm-$(LLVM_VER)/configure: $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR)
	mkdir -p llvm-$(LLVM_VER) && \
	tar -C llvm-$(LLVM_VER) --strip-components 1 -xf $(LLVM_TAR)
ifneq ($(LLVM_CLANG_TAR),)
	mkdir -p llvm-$(LLVM_VER)/tools/clang && \
	tar -C llvm-$(LLVM_VER)/tools/clang --strip-components 1 -xf $(LLVM_CLANG_TAR)
endif
ifneq ($(LLVM_COMPILER_RT_TAR),)
	mkdir -p llvm-$(LLVM_VER)/projects/compiler-rt && \
	tar -C llvm-$(LLVM_VER)/projects/compiler-rt --strip-components 1 -xf $(LLVM_COMPILER_RT_TAR)
endif
	touch $@

## LLVM needs python 2.x, but doesn't check for it, so we have to use an ugly workaround to make it compile
llvm_python_workaround:
	-python -c 'import sys; sys.exit(not sys.version_info > (3, 0))' && \
	/usr/bin/python2 -c 'import sys; sys.exit(not sys.version_info < (3, 0))' && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	test ! -e llvm-$(LLVM_VER)/python2_path/python && \
	mkdir -p llvm-$(LLVM_VER)/python2_path && \
	ln -s /usr/bin/python2 "llvm-$(LLVM_VER)/python2_path/python"

LLVM_CXX = $(CXX)
ifeq ($(ARCH), ppc64)
  LLVM_CXX += -mminimal-toc
endif

$(LLVM_OBJ_SOURCE): llvm-$(LLVM_VER)/configure | llvm_python_workaround
	cd llvm-$(LLVM_VER) && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	./configure --prefix=$(abspath $(USR)) --disable-threads --enable-optimized --disable-profiling --disable-assertions --enable-shared --enable-targets=host --disable-bindings --disable-docs CC="$(CC)" CXX="$(LLVM_CXX)" && \
	$(MAKE)
$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE) | llvm_python_workaround
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) -C llvm-$(LLVM_VER) install
	$(INSTALL_NAME_CMD)libLLVM-$(LLVM_VER).$(SHLIB_EXT) $(USRLIB)/libLLVM-$(LLVM_VER).$(SHLIB_EXT)
	touch $@

clean-llvm:
	$(MAKE) -C llvm-$(LLVM_VER) clean
	rm -f $(USRBIN)/llvm-config
distclean-llvm:
	rm -rf llvm-$(LLVM_VER).tar.gz llvm-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).tar.gz compiler-rt-$(LLVM_VER).src.tar.gz llvm-$(LLVM_VER)

## GNU readline ##

READLINE_OBJ_TARGET = $(USRLIB)/libreadline.$(SHLIB_EXT)
READLINE_OBJ_SOURCE = readline-$(READLINE_VER)/shlib/libreadline.$(READLINE_VER).$(SHLIB_EXT)

READLINE_OPTS = --disable-shared --enable-static
ifeq ($(OS),WINNT)
	READLINE_URL = https://github.com/JuliaLang/readline/tarball/master
	READLINE_CFLAGS = LOCAL_DEFS="-include $(abspath .)/readline-win.h"
else
	READLINE_URL = ftp://ftp.gnu.org/gnu/readline/readline-$(READLINE_VER).tar.gz
	READLINE_OPTS += --with-curses
	READLINE_CFLAGS = 
endif

compile-readline: $(READLINE_OBJ_SOURCE)
install-readline: $(READLINE_OBJ_TARGET)

ifeq ($(OS),WINNT)

readline-$(READLINE_VER).tar.gz:
	$(WGET_DASH_O) $@ $(READLINE_URL)
readline-$(READLINE_VER)/configure: readline-$(READLINE_VER).tar.gz
	mkdir readline-$(READLINE_VER)
	tar -C readline-$(READLINE_VER) --strip-components 1 -xf $<
	touch $@
$(READLINE_OBJ_SOURCE): readline-$(READLINE_VER)/configure
	cd readline-$(READLINE_VER) && \
	./configure --prefix=$(abspath $(USR)) $(READLINE_OPTS) CC="$(CC)" CXX="$(CXX)" && \
	$(MAKE) $(READLINE_CFLAGS)
	touch $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	$(MAKE) -C readline-$(READLINE_VER) $(READLINE_CFLAGS) install
	touch $@

clean-readline:
	$(MAKE) -C readline-$(READLINE_VER) clean
	rm -f $(READLINE_OBJ_TARGET)
distclean-readline:
	rm -rf readline-$(READLINE_VER).tar.gz readline-$(READLINE_VER)

else

readline-$(READLINE_VER).tar.gz:
	$(WGET) http://ftp.gnu.org/gnu/readline/$@
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-001
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-002
readline-$(READLINE_VER)/configure: readline-$(READLINE_VER).tar.gz
	tar zxf $<
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-001
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-002
	touch $@
$(READLINE_OBJ_SOURCE): readline-$(READLINE_VER)/configure
	cd readline-$(READLINE_VER) && \
	./configure --prefix=$(abspath $(USR)) --enable-shared --enable-static --with-curses CC="$(CC)" CXX="$(CXX)" && \
	$(MAKE)
	touch $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	$(MAKE) -C readline-$(READLINE_VER) install
	chmod +w $(USRLIB)/libreadline.* $(USRLIB)/libhistory.*
	$(INSTALL_NAME_CMD)libreadline.$(SHLIB_EXT) $(USRLIB)/libreadline.$(SHLIB_EXT) 
	$(INSTALL_NAME_CMD)libhistory.dylib $(USRLIB)/libhistory.dylib
	touch $@

clean-readline:
	$(MAKE) -C readline-$(READLINE_VER) clean
	rm -f $(READLINE_OBJ_TARGET)
distclean-readline:
	rm -rf readline-$(READLINE_VER).tar.gz readline-$(READLINE_VER)

endif

## LIBUV

ifeq ($(OS), WINNT) 
UV_CFLAGS=
else
UV_CFLAGS = CFLAGS="-g -fPIC"
endif

UV_OBJ_TARGET = $(USRLIB)/uv.a
UV_OBJ_SOURCE = libuv/uv.a

libuv/Makefile:
	(cd .. && git submodule update --init)
$(UV_OBJ_SOURCE): libuv/Makefile
	$(MAKE) -C libuv $(UV_CFLAGS)
$(UV_OBJ_TARGET): $(UV_OBJ_SOURCE)
	mkdir -p $(USR)/include
	cp $(UV_OBJ_SOURCE) $(USRLIB)/
	cp -r libuv/include/* $(USR)/include
install-uv: $(UV_OBJ_TARGET)

clean-uv:
	$(MAKE) -C libuv clean
	rm -f $(USRLIB)/uv.a $(USR)/include/uv.h
distclean-uv: clean-uv

## PCRE ##

PCRE_OBJ_TARGET = $(USRLIB)/libpcre.$(SHLIB_EXT)

compile-pcre: install-pcre
install-pcre: $(PCRE_OBJ_TARGET)

pcre-$(PCRE_VER).tar.bz2:
	$(WGET) ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/$@
pcre-$(PCRE_VER)/configure: pcre-$(PCRE_VER).tar.bz2
	tar jxf $<
	touch $@
pcre-$(PCRE_VER)/config.status: pcre-$(PCRE_VER)/configure
	cd pcre-$(PCRE_VER) && \
	./configure --prefix=$(abspath $(USR)) --enable-utf8 --enable-unicode-properties --enable-jit CC="$(CC)" CXX="$(CXX)"
$(PCRE_OBJ_TARGET): pcre-$(PCRE_VER)/config.status
	$(MAKE) -C pcre-$(PCRE_VER) install
	$(INSTALL_NAME_CMD)libpcre.dylib $@
ifeq ($(OS),WINNT)
	-rm $(USRLIB)/libpcre.dll
	mv $(USRBIN)/libpcre-1.dll $(USRLIB)/libpcre.dll
endif
	touch $@

clean-pcre:
	$(MAKE) -C pcre-$(PCRE_VER) clean
	rm -f $(PCRE_OBJ_TARGET)
distclean-pcre:
	rm -rf pcre-$(PCRE_VER).tar.bz2 pcre-$(PCRE_VER)

## Grisu floating-point printing library ##

GRISU_OPTS = -O3 -fvisibility=hidden $(fPIC)

compile-double-conversion: double-conversion-$(GRISU_VER)/src/libgrisu_.$(SHLIB_EXT)
install-double-conversion: $(USRLIB)/libgrisu.$(SHLIB_EXT)

double-conversion-$(GRISU_VER).tar.gz:
	$(WGET) http://double-conversion.googlecode.com/files/double-conversion-$(GRISU_VER).tar.gz
double-conversion-$(GRISU_VER)/Makefile: double-conversion-$(GRISU_VER).tar.gz
	mkdir -p double-conversion-$(GRISU_VER) && \
	tar -C double-conversion-$(GRISU_VER) --strip-components 1 -xf double-conversion-$(GRISU_VER).tar.gz
	touch $@
double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT): double-conversion-$(GRISU_VER)/Makefile
	cd double-conversion-$(GRISU_VER) && \
	$(CXX) -c $(GRISU_OPTS) -o src/bignum.o -Isrc src/bignum.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/bignum-dtoa.o -Isrc src/bignum-dtoa.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/cached-powers.o -Isrc src/cached-powers.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/diy-fp.o -Isrc src/diy-fp.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/double-conversion.o -Isrc src/double-conversion.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/fast-dtoa.o -Isrc src/fast-dtoa.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/fixed-dtoa.o -Isrc src/fixed-dtoa.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/strtod.o -Isrc src/strtod.cc && \
	$(CXX) -c $(GRISU_OPTS) -o src/libdouble-conversion.o -I.. -Isrc ../double_conversion_wrapper.cpp && \
	$(CXX) $(GRISU_OPTS) src/*.o -shared -dead_strip -o src/libgrisu.$(SHLIB_EXT)
$(USRLIB)/libgrisu.$(SHLIB_EXT): double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
	cp -f $< $@
	$(INSTALL_NAME_CMD)libgrisu.dylib $@

clean-double-conversion:
	rm -f double-conversion-$(GRISU_VER)/src/*.o
	rm -f double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
distclean-double-conversion:
	rm -rf double-conversion-$(GRISU_VER).tar.gz double-conversion-$(GRISU_VER)

## fdlibm ##

FDLIBM_OBJ_TARGET = $(USRLIB)/libfdm.$(SHLIB_EXT)
FDLIBM_OBJ_SOURCE = fdlibm/libfdm.$(SHLIB_EXT)

compile-fdlibm: $(FDLIBM_OBJ_SOURCE)
install-fdlibm: $(FDLIBM_OBJ_TARGET)

$(FDLIBM_OBJ_SOURCE): fdlibm/*.c
	cd fdlibm && \
	$(MAKE) CC="$(CC)" CFLAGS="$(CFLAGS) -D_IEEE_LIBM -Dx86 $(fPIC) -O2 $(CONFIG)" && \
	$(CC) -shared *.o -o libfdm.$(SHLIB_EXT)
$(FDLIBM_OBJ_TARGET): $(FDLIBM_OBJ_SOURCE)
	cp $< $@
	$(INSTALL_NAME_CMD)libfdm.dylib $@

clean-fdlibm:
	cd fdlibm && $(MAKE) clean
	rm -f $(FDLIBM_OBJ_SOURCE) $(FDLIBM_OBJ_TARGET)
distclean-fdlibm: clean-fdlibm

## rmath ##

RMATH_OBJ_TARGET = $(USRLIB)/libRmath.$(SHLIB_EXT)
RMATH_OBJ_SOURCE = Rmath/src/libRmath.$(SHLIB_EXT)

compile-rmath: $(RMATH_OBJ_SOURCE)
install-rmath: $(RMATH_OBJ_TARGET)

$(RMATH_OBJ_SOURCE): Rmath/src/Makefile install-dsfmt
	cd Rmath/src && \
	$(MAKE) CC="$(CC)" && \
	$(INSTALL_NAME_CMD)libRmath.$(SHLIB_EXT) libRmath.$(SHLIB_EXT)
$(RMATH_OBJ_TARGET): $(RMATH_OBJ_SOURCE)
	cp $< $@

clean-rmath:
	cd Rmath/src && $(MAKE) clean
	rm -f $(RMATH_OBJ_SOURCE) $(RMATH_OBJ_TARGET)
distclean-rmath: clean-rmath

## amos ##

AMOS_OBJ_TARGET = $(USRLIB)/libamos.$(SHLIB_EXT)
AMOS_OBJ_SOURCE = amos/libamos.$(SHLIB_EXT)

compile-amos: $(AMOS_OBJ_SOURCE)
install-amos: $(AMOS_OBJ_TARGET)

$(AMOS_OBJ_SOURCE): amos/*.f
	cd amos && \
	$(MAKE) && \
	$(FC) $(FFLAGS) $(JFFLAGS) -shared *.o -o libamos.$(SHLIB_EXT)
$(AMOS_OBJ_TARGET): $(AMOS_OBJ_SOURCE)
	cp $< $@
	$(INSTALL_NAME_CMD)libamos.dylib $@

clean-amos:
	cd amos && $(MAKE) clean
	rm -f $(AMOS_OBJ_SOURCE) $(AMOS_OBJ_TARGET)
distclean-amos: clean-amos

## DSFMT ##

DSFMT_OBJ_TARGET = $(USRLIB)/librandom.$(SHLIB_EXT)
DSFMT_OBJ_SOURCE = random/librandom.$(SHLIB_EXT)

LIBRANDOM_CFLAGS = $(CFLAGS) -O3 -finline-functions -fomit-frame-pointer -DNDEBUG -fno-strict-aliasing --param max-inline-insns-single=1800  -Wmissing-prototypes -Wall  -std=c99 -DDSFMT_MEXP=19937 $(fPIC) -shared -DDSFMT_DO_NOT_USE_OLD_NAMES
ifeq ($(ARCH), x86_64)
LIBRANDOM_CFLAGS += -msse2 -DHAVE_SSE2
endif

compile-dsfmt: $(DSFMT_OBJ_SOURCE)
install-dsfmt: $(DSFMT_OBJ_TARGET)

random/dsfmt-$(DSFMT_VER).tar.gz:
	$(WGET_DASH_O) $@ http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz 
	touch $@
random/jl_random.c: random/dsfmt-$(DSFMT_VER).tar.gz
	cd random && \
	mkdir -p dsfmt-$(DSFMT_VER) && \
	tar -C dsfmt-$(DSFMT_VER) --strip-components 1 -xf dsfmt-$(DSFMT_VER).tar.gz && \
	cd dsfmt-$(DSFMT_VER) && patch < ../dSFMT.h.patch
	touch $@
$(DSFMT_OBJ_SOURCE): random/jl_random.c
	cd random && \
	$(CC) $(LIBRANDOM_CFLAGS) jl_random.c -o librandom.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)librandom.$(SHLIB_EXT) librandom.$(SHLIB_EXT)

$(DSFMT_OBJ_TARGET): $(DSFMT_OBJ_SOURCE)
	cp $< $@

clean-dsfmt:
	rm -f random/librandom.$(SHLIB_EXT)
distclean-dsfmt: clean-dsfmt
	cd random && rm -rf *.tar.gz dsfmt-$(DSFMT_VER)

## OpenBLAS ##

ifeq ($(USE_SYSTEM_BLAS), 0)
OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT)
else
OPENBLAS_OBJ_SOURCE = 
endif

OPENBLAS_BUILD_OPTS =
ifeq ($(OPENBLAS_DYNAMIC_ARCH), 1)
OPENBLAS_BUILD_OPTS += DYNAMIC_ARCH=1
endif

compile-openblas: $(OPENBLAS_OBJ_SOURCE)
install-openblas: $(USRLIB)/libopenblas.$(SHLIB_EXT)

openblas-$(OPENBLAS_VER).tar.gz:
	$(WGET_DASH_O) $@ https://github.com/xianyi/OpenBLAS/tarball/$(OPENBLAS_VER) 
openblas-$(OPENBLAS_VER)/Makefile: openblas-$(OPENBLAS_VER).tar.gz
	mkdir -p openblas-$(OPENBLAS_VER) && \
	tar -C openblas-$(OPENBLAS_VER) --strip-components 1 -xf $<
	perl -i -ple 's/^\s*(EXTRALIB\s*\+=\s*-lSystemStubs)\s*$$/# $$1/g' $@.system
	touch $@
$(OPENBLAS_OBJ_SOURCE): openblas-$(OPENBLAS_VER)/Makefile
	$(MAKE) -C openblas-$(OPENBLAS_VER) CC="$(CC)" FC="$(FC)" FFLAGS="$(FFLAGS) $(JFFLAGS)" USE_THREAD=$(OPENBLAS_USE_THREAD) TARGET=$(OPENBLAS_TARGET_ARCH) $(OPENBLAS_BUILD_OPTS)
$(USRLIB)/libopenblas.$(SHLIB_EXT): $(OPENBLAS_OBJ_SOURCE) | $(USRLIB)
	cp -f openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT) $(USRLIB)
	$(INSTALL_NAME_CMD)libopenblas.$(SHLIB_EXT) $(USRLIB)/libopenblas.$(SHLIB_EXT)

clean-openblas:
	$(MAKE) -C openblas-$(OPENBLAS_VER) clean
distclean-openblas:
	rm -rf openblas-$(OPENBLAS_VER).tar.gz openblas-$(OPENBLAS_VER)

## LAPACK ##

ifeq ($(USE_SYSTEM_LAPACK), 0)
LAPACK_OBJ_TARGET = $(USRLIB)/liblapack.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE = lapack-$(LAPACK_VER)/liblapack.$(SHLIB_EXT)
else
LAPACK_OBJ_TARGET =
LAPACK_OBJ_SOURCE =
endif

compile-lapack: $(LAPACK_OBJ_SOURCE)
install-lapack: $(LAPACK_OBJ_TARGET)

lapack-$(LAPACK_VER).tgz:
	$(WGET) http://www.netlib.org/lapack/$@
lapack-$(LAPACK_VER)/Makefile: lapack-$(LAPACK_VER).tgz
	tar zxf $<
	touch $@
lapack-$(LAPACK_VER)/INSTALL/dlamch.o: lapack-$(LAPACK_VER)/Makefile $(OPENBLAS_OBJ_SOURCE)
	cd lapack-$(LAPACK_VER) && \
	cp INSTALL/make.inc.gfortran ./make.inc && \
	$(MAKE) lapacklib NOOPT="-O0 $(fPIC)" OPTS="$(FFLAGS) $(JFFLAGS)" FORTRAN="$(FC)" LOADER="$(FC)"
	touch $@
$(LAPACK_OBJ_SOURCE): lapack-$(LAPACK_VER)/INSTALL/dlamch.o $(OPENBLAS_OBJ_SOURCE)
	cd lapack-$(LAPACK_VER) && \
	$(FC) -shared $(FFLAGS) $(JFFLAGS) SRC/*.o INSTALL/dlamch.o INSTALL/dsecnd_INT_ETIME.o INSTALL/ilaver.o INSTALL/slamch.o $(LIBBLAS) -o liblapack.$(SHLIB_EXT)
$(LAPACK_OBJ_TARGET): $(LAPACK_OBJ_SOURCE)
	cp $< $@

clean-lapack:
	$(MAKE) -C lapack-$(LAPACK_VER) clean
	rm -f $(LAPACK_OBJ_SOURCE) $(LAPACK_OBJ_TARGET)
distclean-lapack:
	rm -rf lapack-$(LAPACK_VER).tgz lapack-$(LAPACK_VER)

## ARPACK ##

ARPACK_OBJ_TARGET = $(USRLIB)/libarpack.$(SHLIB_EXT)
ARPACK_OBJ_SOURCE = arpack-ng_$(ARPACK_VER)/libarpack.$(SHLIB_EXT)

compile-arpack: $(ARPACK_OBJ_SOURCE)
install-arpack: $(ARPACK_OBJ_TARGET)

arpack-ng_$(ARPACK_VER).tar.gz:
	$(WGET_DASH_O) $@ http://forge.scilab.org/index.php/p/arpack-ng/downloads/get/arpack-ng_$(ARPACK_VER).tar.gz
	touch $@
arpack-ng_$(ARPACK_VER)/configure: arpack-ng_$(ARPACK_VER).tar.gz
	tar zxf $< 
	touch $@
$(ARPACK_OBJ_SOURCE): arpack-ng_$(ARPACK_VER)/configure $(OPENBLAS_OBJ_SOURCE)
	cd arpack-ng_$(ARPACK_VER) && \
	./configure  --prefix=$(abspath $(USR)) --with-blas="$(LIBBLAS)" --with-lapack="$(LIBLAPACK)" --disable-mpi --enable-shared F77="$(FC)" MPIF77="$(FC)" CC="$(CC)" CXX="$(CXX)"
	touch $@
$(ARPACK_OBJ_TARGET): $(ARPACK_OBJ_SOURCE) | $(USRLIB)
	cd arpack-ng_$(ARPACK_VER) && \
	$(MAKE) install F77="$(FC)" MPIF77="$(FC)"
	$(INSTALL_NAME_CMD)libarpack.$(SHLIB_EXT) $(USRLIB)/libarpack.$(SHLIB_EXT)
	touch $@

clean-arpack:
	$(MAKE) -C arpack-ng_$(ARPACK_VER) clean
	rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	rm -rf arpack-ng_$(ARPACK_VER).tar.gz arpack-ng_$(ARPACK_VER)

## FFTW ##

FFTW_SINGLE_OBJ_TARGET = $(USRLIB)/libfftw3f.3.$(SHLIB_EXT)
FFTW_DOUBLE_OBJ_TARGET = $(USRLIB)/libfftw3.3.$(SHLIB_EXT)

compile-fftw: compile-fftw-single compile-fftw-double
compile-fftw-single: install-fftw-single
compile-fftw-double: install-fftw-double

install-fftw: install-fftw-single install-fftw-double
install-fftw-single: $(FFTW_SINGLE_OBJ_TARGET)
install-fftw-double: $(FFTW_DOUBLE_OBJ_TARGET)

FFTW_CONFIG = --enable-shared --disable-fortran --disable-mpi --enable-fma --enable-threads
ifneq ($(ARCH), ppc64)
  FFTW_CONFIG += --enable-sse2
endif
ifeq ($(OS),WINNT)
	FFTW_CONFIG += --with-our-malloc --with-combined-threads
endif

fftw-$(FFTW_VER).tar.gz:
	$(WGET) http://www.fftw.org/$@

fftw-$(FFTW_VER)-single/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-single && \
	tar -C fftw-$(FFTW_VER)-single --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-single/config.status: fftw-$(FFTW_VER)-single/configure
	cd fftw-$(FFTW_VER)-single && \
	./configure --prefix=$(abspath $(USR)) $(FFTW_CONFIG) --enable-single CC="$(CC)" CXX="$(CXX)" && \
	$(MAKE) clean
	touch $@
$(FFTW_SINGLE_OBJ_TARGET): fftw-$(FFTW_VER)-single/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-single install
	$(INSTALL_NAME_CMD)libfftw3f.dylib $(USRLIB)/libfftw3f.dylib
	$(INSTALL_NAME_CMD)libfftw3f_threads.$(SHLIB_EXT) $(USRLIB)/libfftw3f_threads.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) $(USRLIB)/libfftw3f.3.dylib $(INSTALL_NAME_ID_DIR)libfftw3f.dylib $(USRLIB)/libfftw3f_threads.dylib
ifeq ($(OS),WINNT)
	-rm $(USRLIB)/libfftw3f.dll
	mv $(USRBIN)/libfftw3f-3.dll $(USRLIB)/libfftw3f.dll
endif
	touch $@

fftw-$(FFTW_VER)-double/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-double && \
	tar -C fftw-$(FFTW_VER)-double --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-double/config.status: fftw-$(FFTW_VER)-double/configure
	cd fftw-$(FFTW_VER)-double && \
	./configure --prefix=$(abspath $(USR)) $(FFTW_CONFIG) CC="$(CC)" CXX="$(CXX)" && \
	$(MAKE) clean
	touch $@
$(FFTW_DOUBLE_OBJ_TARGET): fftw-$(FFTW_VER)-double/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-double install
	$(INSTALL_NAME_CMD)libfftw3.$(SHLIB_EXT) $(USRLIB)/libfftw3.$(SHLIB_EXT) 
	$(INSTALL_NAME_CMD)libfftw3_threads.$(SHLIB_EXT) $(USRLIB)/libfftw3_threads.$(SHLIB_EXT) 
	$(INSTALL_NAME_CHANGE_CMD) $(USRLIB)/libfftw3.3.dylib $(INSTALL_NAME_ID_DIR)libfftw3.dylib $(USRLIB)/libfftw3_threads.dylib
ifeq ($(OS),WINNT)
	-rm $(USRLIB)/libfftw3.dll
	mv $(USRBIN)/libfftw3-3.dll $(USRLIB)/libfftw3.dll
endif
	touch $@

clean-fftw: clean-fftw-single clean-fftw-double
clean-fftw-single:
	-$(MAKE) -C fftw-$(FFTW_VER)-single clean
	rm -f $(FFTW_SINGLE_OBJ_TARGET)
clean-fftw-double:
	-$(MAKE) -C fftw-$(FFTW_VER)-double clean
	rm -f $(FFTW_DOUBLE_OBJ_TARGET)
distclean-fftw:
	rm -rf fftw-$(FFTW_VER).tar.gz fftw-$(FFTW_VER)-{single,double}


## SUITESPARSE ##

ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
SUITESPARSE_OBJ_SOURCE = SuiteSparse-$(SUITESPARSE_VER)/UMFPACK/Lib/libumfpack.a
SUITESPARSE_OBJ_TARGET = $(USRLIB)/libumfpack.$(SHLIB_EXT)
endif

ifeq ($(OS), Darwin)
SUITE_SPARSE_LIB = -lm -Wl,-rpath,'$(USRLIB)'
else
SUITE_SPARSE_LIB = -lm -lrt -Wl,-rpath,'$(USRLIB)'
endif

compile-suitesparse: $(SUITESPARSE_OBJ_SOURCE)
install-suitesparse: $(SUITESPARSE_OBJ_TARGET) $(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)

SuiteSparse-$(SUITESPARSE_VER).tar.gz:
	$(WGET) http://www.cise.ufl.edu/research/sparse/SuiteSparse/SuiteSparse-$(SUITESPARSE_VER).tar.gz
SuiteSparse-$(SUITESPARSE_VER)/Makefile: SuiteSparse-$(SUITESPARSE_VER).tar.gz
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)
	tar -C SuiteSparse-$(SUITESPARSE_VER) --strip-components 1 -zxf $<
	touch $@
$(SUITESPARSE_OBJ_SOURCE): $(OPENBLAS_OBJ_SOURCE) SuiteSparse-$(SUITESPARSE_VER)/Makefile
	cd SuiteSparse-$(SUITESPARSE_VER) && \
	$(MAKE) CC="$(CC)" CXX="$(CXX)" BLAS="$(LIBBLAS)" LAPACK="$(LIBLAPACK)" INSTALL_LIB="$(USRLIB)" INSTALL_INCLUDE="$(USRINC)" LIB="$(SUITE_SPARSE_LIB)"
	touch $@

ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
$(SUITESPARSE_OBJ_TARGET): $(SUITESPARSE_OBJ_SOURCE)
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)/lib && \
	cd SuiteSparse-$(SUITESPARSE_VER)/lib && \
	rm -f *.a && \
	cp -f `find .. -name libamd.a -o -name libcolamd.a -o -name libcholmod.a -o -name libumfpack.a -o -name libsuitesparseconfig.a 2>/dev/null` . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(USRLIB)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libcolamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(USRLIB)/libcolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libcholmod.$(SHLIB_EXT) -L$(USRLIB) -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(USRLIB)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(USRLIB)/libumfpack.$(SHLIB_EXT) -L$(USRLIB) -lcholmod -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(USRLIB)/libumfpack.$(SHLIB_EXT)
endif

clean-suitesparse:
	-$(MAKE) -C SuiteSparse-$(SUITESPARSE_VER) clean
	rm -fr SuiteSparse-$(SUITESPARSE_VER)/lib
distclean-suitesparse: clean-suitesparse
	rm -rf SuiteSparse-$(SUITESPARSE_VER).tar.gz SuiteSparse-$(SUITESPARSE_VER)

# SUITESPARSE WRAPPER

ifeq ($(USE_SYSTEM_SUITESPARSE), 1)
SUITESPARSE_INC = -I /usr/include/suitesparse
SUITESPARSE_LIB = -lumfpack -lcholmod -lamd -lcamd -lcolamd
else
SUITESPARSE_INC = -I SuiteSparse-$(SUITESPARSE_VER)/CHOLMOD/Include -I SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse_config
SUITESPARSE_LIB = -L$(USRLIB) -lcholmod $(RPATH_ORIGIN)
endif

$(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT): SuiteSparse_wrapper.c $(SUITESPARSE_OBJ_TARGET)
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(SUITESPARSE_INC) SuiteSparse_wrapper.c -o $(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT) $(SUITESPARSE_LIB)
	$(INSTALL_NAME_CMD)libsuitesparse_wrapper.$(SHLIB_EXT) $@
	touch $@
install-suitesparse-wrapper: $(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)

clean-suitesparse-wrapper:
	rm -f $(SUITESPARSE_OBJ_TARGET) $(USRLIB)/libsuitesparse_wrapper.$(SHLIB_EXT)
distclean-suitesparse-wrapper: clean-suitesparse-wrapper

## CLP ##

CLP_OBJ_TARGET = $(USRLIB)/libClp.$(SHLIB_EXT)

compile-clp: install-clp
install-clp: $(CLP_OBJ_TARGET)

clp-$(CLP_VER).tar.gz:
	$(WGET_DASH_O) $@ http://www.coin-or.org/download/source/Clp/Clp-1.14.5.tgz
clp-$(CLP_VER)/configure: clp-$(CLP_VER).tar.gz
	mkdir -p clp-$(CLP_VER)
	tar -C clp-$(CLP_VER) --strip-components 1 -zxf $<
	touch $@
clp-$(CLP_VER)/config.status: clp-$(CLP_VER)/configure
	cd clp-$(CLP_VER) && \
	./configure --prefix=$(abspath $(USR)) CC="$(CC)" CXX="$(CXX)"
$(CLP_OBJ_TARGET): clp-$(CLP_VER)/config.status
	$(MAKE) -C clp-$(CLP_VER) install
	touch $@

clean-clp:
	$(MAKE) -C clp-$(CLP_VER) clean
	rm -f $(CLP_OBJ_TARGET)
distclean-clp:
	rm -rf clp-$(CLP_VER).tar.gz clp-$(CLP_VER)

## UNWIND ##

ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
LIBUNWIND_TARGET_OBJ = $(USRLIB)/libunwind.a
LIBUNWIND_TARGET_SOURCE = libunwind-$(UNWIND_VER)/src/.libs/libunwind.a
else
LIBUNWIND_TARGET_OBJ = 
LIBUNWIND_TARGET_SOURCE = 
endif

LIBUNWIND_CFLAGS = $(CFLAGS) -U_FORTIFY_SOURCE $(fPIC)
ifeq ($(ARCH), ppc64)
LIBUNWIND_CFLAGS += -m64
endif

compile-unwind: $(LIBUNWIND_TARGET_SOURCE)
install-unwind: $(LIBUNWIND_TARGET_OBJ)

libunwind-$(UNWIND_VER).tar.gz:
	$(WGET) http://savannah.spinellicreations.com/libunwind/libunwind-$(UNWIND_VER).tar.gz
libunwind-$(UNWIND_VER)/Makefile: libunwind-$(UNWIND_VER).tar.gz
	tar xfz $<
	cd libunwind-$(UNWIND_VER) && ./configure  CFLAGS="$(LIBUNWIND_CFLAGS)" --prefix=$(abspath $(USR)) CC="$(CC)" CXX="$(CXX)"

$(LIBUNWIND_TARGET_SOURCE): libunwind-$(UNWIND_VER)/Makefile
	cd libunwind-$(UNWIND_VER) && $(MAKE)

$(LIBUNWIND_TARGET_OBJ): $(LIBUNWIND_TARGET_SOURCE)
	cd libunwind-$(UNWIND_VER) && $(MAKE) install

clean-unwind:
	$(MAKE) -C libunwind-$(UNWIND_VER) clean
	rm -f $(LIBUNWIND_TARGET_OBJ) $(LIBUNWIND_TARGET_SOURCE)

distclean-unwind:
	rm -rf libunwind-$(UNWIND_VER).tar.gz libunwind-$(UNWIND_VER)

## GNU LIGHTTPD ##

ifeq ($(USE_SYSTEM_LIGHTTPD), 0)
LIGHTTPD_OBJ_TARGET = $(USR)/sbin/lighttpd
else
LIGHTTPD_OBJ_TARGET = 
endif

compile-lighttpd: install-lighttpd
install-lighttpd: $(LIGHTTPD_OBJ_TARGET) lighttpd.conf
	mkdir -p $(USR)/etc
	cp -f lighttpd.conf $(USR)/etc/lighttpd.conf

lighttpd-$(LIGHTTPD_VER).tar.gz:
	$(WGET) http://download.lighttpd.net/lighttpd/releases-1.4.x/lighttpd-$(LIGHTTPD_VER).tar.gz
lighttpd-$(LIGHTTPD_VER)/configure: lighttpd-$(LIGHTTPD_VER).tar.gz
	tar zxf $<
	touch $@
lighttpd-$(LIGHTTPD_VER)/config.status: lighttpd-$(LIGHTTPD_VER)/configure
	cd lighttpd-$(LIGHTTPD_VER) && \
	./configure --prefix=$(abspath $(USR)) --without-pcre --without-zlib --without-bzip2 CC="$(CC)" CXX="$(CXX)"
$(LIGHTTPD_OBJ_TARGET): lighttpd-$(LIGHTTPD_VER)/config.status
	$(MAKE) -C lighttpd-$(LIGHTTPD_VER) install
	touch $@

clean-lighttpd:
	$(MAKE) -C lighttpd-$(LIGHTTPD_VER) clean
	rm -f $(LIGHTTPD_OBJ_TARGET)
distclean-lighttpd:
	rm -rf lighttpd-$(LIGHTTPD_VER).tar.gz lighttpd-$(LIGHTTPD_VER)

## GMP ##

ifeq ($(USE_SYSTEM_GMP), 0)
GMP_OBJ_TARGET = $(USRLIB)/libgmp.$(SHLIB_EXT)
else
GMP_OBJ_TARGET = 
endif

compile-gmp: install-gmp
install-gmp: $(GMP_OBJ_TARGET)

gmp-$(GMP_VER).tar.bz2:
	$(WGET) ftp://ftp.gmplib.org/pub/gmp-$(GMP_VER)/$@
gmp-$(GMP_VER)/configure: gmp-$(GMP_VER).tar.bz2
	tar jxf $<
	touch $@
gmp-$(GMP_VER)/config.status: gmp-$(GMP_VER)/configure
	cd gmp-$(GMP_VER) && \
	./configure --prefix=$(abspath $(USR)) CC="$(CC)" CXX="$(CXX)"
$(GMP_OBJ_TARGET): gmp-$(GMP_VER)/config.status
	$(MAKE) -C gmp-$(GMP_VER)
	$(MAKE) -C gmp-$(GMP_VER) check
	$(MAKE) -C gmp-$(GMP_VER) install
	$(INSTALL_NAME_CMD)libgmp.dylib $@
	touch $@

clean-gmp:
	$(MAKE) -C gmp-$(GMP_VER) clean
	rm -f $(GMP_OBJ_TARGET)
distclean-gmp:
	rm -rf gmp-$(GMP_VER).tar.bz2 gmp-$(GMP_VER)

## GMP Wrapper

ifeq ($(USE_SYSTEM_GMP), 1)
GMPW_INC =
GMPW_LIB = -lgmp
else
GMPW_INC = -I $(USRINC)
GMPW_LIB = -L$(USRLIB)/ -lgmp
endif

$(USRLIB)/libgmp_wrapper.$(SHLIB_EXT): gmp_wrapper.c $(GMP_OBJ_TARGET) | $(USRLIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(GMPW_INC) gmp_wrapper.c -o $(USRLIB)/libgmp_wrapper.$(SHLIB_EXT) -Wl,-rpath,$(USRLIB) $(GMPW_LIB)
	$(INSTALL_NAME_CMD)libgmp_wrapper.$(SHLIB_EXT) $@
	touch $@
install-gmp-wrapper: $(USRLIB)/libgmp_wrapper.$(SHLIB_EXT)

clean-gmp-wrapper:
	rm -f $(GMP_OBJ_TARGET) $(USRLIB)/libgmp_wrapper.$(SHLIB_EXT)
distclean-gmp-wrapper: clean-gmp-wrapper

## GLPK ##

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPK_OBJ_TARGET =
else
GLPK_OBJ_TARGET = $(USRLIB)/libglpk.$(SHLIB_EXT)
endif

compile-glpk: install-glpk
install-glpk: $(GLPK_OBJ_TARGET) $(USRLIB)/libglpk_wrapper.$(SHLIB_EXT)

glpk-$(GLPK_VER).tar.gz:
	$(WGET) http://ftp.gnu.org/gnu/glpk/$@

glpk-$(GLPK_VER)/configure: glpk-$(GLPK_VER).tar.gz
	tar zxf $<
	touch $@
glpk-$(GLPK_VER)/config.status: glpk-$(GLPK_VER)/configure
	cd glpk-$(GLPK_VER) && \
	./configure --prefix=$(abspath $(USR)) CC="$(CC)" CXX="$(CXX)"
$(GLPK_OBJ_TARGET): glpk-$(GLPK_VER)/config.status
	$(MAKE) -C glpk-$(GLPK_VER) install
	$(INSTALL_NAME_CMD)libglpk.dylib $@
	touch $@

clean-glpk:
	-$(MAKE) -C glpk-$(GLPK_VER) uninstall || true
	-$(MAKE) -C glpk-$(GLPK_VER) clean
distclean-glpk: clean-glpk
	rm -rf glpk-$(GLPK_VER).tar.bz2 glpk-$(GLPK_VER)


## GLPK Wrapper

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPKW_INC = -I /usr/include/
GLPKW_LIB = -lglpk
else
GLPKW_INC = -I $(USRINC)
GLPKW_LIB = -L$(USRLIB)/ -lglpk
endif


$(USRLIB)/libglpk_wrapper.$(SHLIB_EXT): glpk_wrapper.c $(GLPK_OBJ_TARGET)
	mkdir -p $(USRLIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(GLPKW_INC) glpk_wrapper.c $(GLPKW_LIB) -o $(USRLIB)/libglpk_wrapper.$(SHLIB_EXT) -Wl,-rpath,$(USRLIB)
	$(INSTALL_NAME_CMD)libglpk_wrapper.$(SHLIB_EXT) $@
	touch $@
install-glpk-wrapper: $(USRLIB)/libglpk_wrapper.$(SHLIB_EXT) glpk_wrapper.c

clean-glpk-wrapper:
	rm -f $(GLPK_OBJ_TARGET) $(USRLIB)/libglpk_wrapper.$(SHLIB_EXT)
distclean-glpk-wrapper: clean-glpk-wrapper

## ZLIB ##

ifeq ($(USE_SYSTEM_ZLIB), 1)
ZLIB_OBJ_TARGET =
else
ZLIB_OBJ_TARGET = $(USRLIB)/libz.$(SHLIB_EXT)
endif

compile-zlib: install-zlib
install-zlib: $(ZLIB_OBJ_TARGET)

zlib-$(ZLIB_VER).tar.gz:
	$(WGET) http://zlib.net/$@
zlib-$(ZLIB_VER)/configure: zlib-$(ZLIB_VER).tar.gz
	tar zxf $<
	touch $@
zlib-$(ZLIB_VER)/config.status: zlib-$(ZLIB_VER)/configure
	cd zlib-$(ZLIB_VER) && \
	./configure --prefix=$(abspath $(USR))
	touch $@
$(ZLIB_OBJ_TARGET): zlib-$(ZLIB_VER)/config.status
	$(MAKE) -C zlib-$(ZLIB_VER) CC="$(CC)"
	$(MAKE) -C zlib-$(ZLIB_VER) check
	$(MAKE) -C zlib-$(ZLIB_VER) install
	$(INSTALL_NAME_CMD)libz.dylib $@
	touch $@

clean-zlib:
	$(MAKE) -j$(PARALLEL_BUILD_JOBS) -C zlib-$(ZLIB_VER) clean
	rm -f $(ZLIB_OBJ_TARGET)
distclean-zlib:
	rm -rf zlib-$(ZLIB_VER).tar.gz zlib-$(ZLIB_VER)

## Tk wrapper ##

TKW_INC = -I $(USRINC) -I $(JULIAHOME)/src -I $(JULIAHOME)/src/support
TKW_LIB = -ltcl8.5 -ltk8.5 -L$(USRLIB)/ -ljulia-release

$(USRLIB)/libtk_wrapper.$(SHLIB_EXT): tk_wrapper.c
	mkdir -p $(USRLIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(TKW_INC) tk_wrapper.c $(TKW_LIB) -o $(USRLIB)/libtk_wrapper.$(SHLIB_EXT) -Wl,-rpath,$(USRLIB)
	$(INSTALL_NAME_CMD)libtk_wrapper.$(SHLIB_EXT) $@
	touch $@
install-tk-wrapper: $(USRLIB)/libtk_wrapper.$(SHLIB_EXT) tk_wrapper.c

clean-tk-wrapper:
	rm -f $(USRLIB)/libtk_wrapper.$(SHLIB_EXT)
distclean-tk-wrapper: clean-tk-wrapper

## phony targets ##

.PHONY: \
	default compile install cleanall distclean \
	compile-* install-* clean-* distclean-*
