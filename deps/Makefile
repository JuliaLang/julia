include Versions.make

## optional packages and options #
LLVM_ASSERTIONS = 0
# set to 1 to get clang and compiler-rt
BUILD_LLVM_CLANG = 0
# set to 1 to get lldb (does not work with llvm3.1 and earlier)
# see http://lldb.llvm.org/build.html for dependancies
BUILD_LLDB = 0

## high-level setup ##

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc
CONFIGURE_COMMON = --prefix=$(abspath $(BUILD)) F77="$(FC)" CC="$(CC)" CXX="$(CXX)"
ifneq ($(XC_HOST),)
CONFIG_GUESS := $(shell ./config.guess)
CONFIGURE_COMMON += --host=$(XC_HOST) --build=$(CONFIG_GUESS)
endif

#autoconf configure-driven scripts: llvm readline pcre arpack fftw clp unwind lighttpd gmp glpk patchelf
#custom configure-driven script: zlib nginx
#custom Makefile rules: openlibm Faddeeva-wrapper rmath double-conversion random gmp-wrapper suitesparse-wrapper suitesparse lapack openblas uv tk-wrapper

# prevent installing libs into usr/lib64 on opensuse
unexport CONFIG_SITE

STAGE1_DEPS = uv openlibm-extras random rmath double-conversion glpk
STAGE2_DEPS = gmp-wrapper Faddeeva-wrapper
STAGE3_DEPS = suitesparse-wrapper

ifeq ($(OS), Linux)
STAGE1_DEPS += unwind patchelf
endif

ifeq ($(OS), FreeBSD)
STAGE1_DEPS += unwind
endif

WGET = $(abspath .)/jldownload
WGET_DASH_O = $(WGET)

## USE_SYSTEM_LIBS options

ifeq ($(USE_SYSTEM_LIBM), 0)
STAGE1_DEPS += openlibm
endif

ifeq ($(USE_SYSTEM_LLVM), 0)
STAGE1_DEPS += llvm
endif

# Disable web repl
#ifneq ($(OS),WINNT)
#STAGE1_DEPS += lighttpd
#endif

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

#Platform specific flags

ifeq ($(OS), WINNT)
LIBTOOL_CCLD = CCLD="$(CC) -no-undefined -avoid-version"
AUTOTOOLS_PATH_CORRECT = -cp -f $(USRBIN)/$(@F) $(USR)/$(JL_LIBDIR)/$(@F)
endif

## Common build target prefixes

LIBS = $(STAGE1_DEPS) $(STAGE2_DEPS) $(STAGE3_DEPS)

default: $(BUILD) install
compile: $(addprefix compile-, $(LIBS))
install: $(addprefix install-, $(LIBS))
cleanall: $(addprefix clean-, $(LIBS))
distclean: $(addprefix distclean-, $(LIBS))
	rm -rf $(BUILD)

## PATHS ##
DIRS = $(addprefix $(BUILD)/,lib include bin share etc)

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))

$(BUILD): $(DIRS)

## LLVM ##
ifeq ($(BUILD_LLDB), 1)
BUILD_LLVM_CLANG = 1
# because it's a build requirement
endif
ifeq ($(LLVM_ASSERTIONS), 1)
LLVM_BUILDDIR = build/Release+Asserts
else
LLVM_BUILDDIR = build/Release
endif

LLVM_LIB_FILE = libLLVM-$(LLVM_VER)$(LLVM_LIB_SUFFIX).$(SHLIB_EXT)
LLVM_OBJ_TARGET = $(BUILD)/lib/$(LLVM_LIB_FILE)
ifeq ($(OS),WINNT)
	LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/$(LLVM_BUILDDIR)/bin/LLVM-$(LLVM_VER)$(LLVM_LIB_SUFFIX).$(SHLIB_EXT)
else
	LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/$(LLVM_BUILDDIR)/lib/$(LLVM_LIB_FILE)
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

ifeq ($(BUILD_LLDB), 1)
llvm-$(LLVM_VER)/tools/lldb:
llvm-$(LLVM_VER)/configure: llvm-$(LLVM_VER)/tools/lldb
endif

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
ifeq ($(BUILD_LLDB), 1)
	([ ! -d llvm-$(LLVM_VER)/tools/lldb ] && \
		(cd llvm-$(LLVM_VER)/tools && \
		git clone http://llvm.org/git/lldb.git)) || \
	([ -d llvm-$(LLVM_VER)/tools/lldb ] && \
		(cd llvm-$(LLVM_VER)/tools/lldb && \
		git pull))
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

LLVM_FLAGS =  --disable-threads --enable-optimized --disable-profiling --enable-shared --enable-targets=host --disable-bindings --disable-docs
LLVM_MFLAGS =
ifeq ($(LLVM_ASSERTIONS), 1)
LLVM_FLAGS += --enable-assertions
else
LLVM_FLAGS += --disable-assertions
endif
ifeq ($(BUILD_LLDB),1)
ifeq ($(USE_CLANG),1)
LLVM_FLAGS += --enable-cxx11
else
LLVM_MFLAGS += CXXFLAGS=-std=c++0x
endif
endif
ifeq ($(ARCH), ppc64)
  LLVM_FLAGS += CXXFLAGS=-mminimal-toc
  LLVM_MFLAGS += CXXFLAGS=-mminimal-toc
endif

ifeq (exists, $(shell [ -e llvm-$(LLVM_VER)/config.log ] && echo exists ))
# temporary hack to force rebuild of llvm if it was previously configured
# in the source directory
llvm-$(LLVM_VER)/configure: FORCE_LLVM_RECONFIGURE
FORCE_LLVM_RECONFIGURE:
	rm -rf llvm-$(LLVM_VER)
endif

$(LLVM_OBJ_SOURCE): llvm-$(LLVM_VER)/configure | llvm_python_workaround
	cd llvm-$(LLVM_VER) && \
	mkdir -p build && cd build && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	../configure $(CONFIGURE_COMMON) $(LLVM_FLAGS) && \
	$(MAKE) $(LLVM_MFLAGS)
$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE) | llvm_python_workaround
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) -C llvm-$(LLVM_VER)/build install
	$(INSTALL_NAME_CMD)$(LLVM_LIB_FILE) $(BUILD)/lib/$(LLVM_LIB_FILE)
	touch $@

clean-llvm:
	-$(MAKE) -C llvm-$(LLVM_VER)/build clean
	-rm -f $(BUILD)/bin/llvm-config
distclean-llvm:
	-rm -rf llvm-$(LLVM_VER).tar.gz llvm-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).tar.gz compiler-rt-$(LLVM_VER).src.tar.gz llvm-$(LLVM_VER)

## GNU readline ##

READLINE_OBJ_TARGET = $(BUILD)/lib/libreadline.$(SHLIB_EXT)
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
	chmod a+x ./configure && \
	./configure $(CONFIGURE_COMMON) $(READLINE_OPTS) && \
	$(MAKE) $(READLINE_CFLAGS)
	touch $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	$(MAKE) -C readline-$(READLINE_VER) $(READLINE_CFLAGS) install
	touch $@

clean-readline:
	-$(MAKE) -C readline-$(READLINE_VER) clean
	-rm -f $(READLINE_OBJ_TARGET)
distclean-readline:
	-rm -rf readline-$(READLINE_VER).tar.gz readline-$(READLINE_VER)

else

readline-$(READLINE_VER).tar.gz:
	$(WGET) http://ftp.gnu.org/gnu/readline/$@
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-001
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-002
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-003
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-004
readline-$(READLINE_VER)/configure: readline-$(READLINE_VER).tar.gz
	tar zxf $<
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-001
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-002
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-003
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-004
	touch $@
$(READLINE_OBJ_SOURCE): readline-$(READLINE_VER)/configure
	cd readline-$(READLINE_VER) && \
	./configure $(CONFIGURE_COMMON) --enable-shared --enable-static --with-curses && \
	$(MAKE)
	touch $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE)
	$(MAKE) -C readline-$(READLINE_VER) install
	chmod +w $(BUILD)/lib/libreadline.* $(BUILD)/lib/libhistory.*
ifeq ($(OS), Darwin)
	$(INSTALL_NAME_CMD)libreadline.$(SHLIB_EXT) $(BUILD)/lib/libreadline.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libhistory.dylib $(BUILD)/lib/libhistory.dylib
else ifeq ($(OS), Linux)
	for filename in $(BUILD)/lib/libhistory.so* $(BUILD)/lib/libreadline.so* ; do \
		$(BUILD)/bin/patchelf --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch $@

clean-readline:
	-$(MAKE) -C readline-$(READLINE_VER) clean
	-rm -f $(READLINE_OBJ_TARGET)
distclean-readline:
	-rm -rf readline-$(READLINE_VER).tar.gz readline-$(READLINE_VER)

endif

## LIBUV
uname_S=$(OS)
ifeq ($(uname_S),WINNT)
uname_S=MINGW
endif

UV_OBJ_TARGET = $(BUILD)/lib/libuv.a
UV_OBJ_SOURCE = libuv/libuv.a

libuv/Makefile:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d libuv/.git ] && echo exists ))
$(UV_OBJ_SOURCE): libuv/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/modules ] && echo exists ))
$(UV_OBJ_SOURCE): $(JULIAHOME)/.git/modules/deps/libuv/HEAD
endif
$(UV_OBJ_SOURCE): libuv/Makefile
ifneq ($(OS), WINNT)
	$(MAKE) libuv.${SHLIB_EXT} -C libuv CC="$(CC)" AR="$(AR)" uname_S=$(uname_S)
endif
	$(MAKE) libuv.a -C libuv CC="$(CC)" AR="$(AR)" uname_S=$(uname_S)
$(UV_OBJ_TARGET): $(UV_OBJ_SOURCE)
	mkdir -p $(BUILD)/include
	cp $(UV_OBJ_SOURCE) $(BUILD)/lib/
	cp -r libuv/include/* $(BUILD)/include
install-uv: $(UV_OBJ_TARGET)

clean-uv:
	-$(MAKE) -C libuv clean uname_S=$(uname_S)
	-rm -rf $(BUILD)/lib/libuv.a $(BUILD)/include/uv.h $(BUILD)/include/uv-private
distclean-uv: clean-uv

## PCRE ##

PCRE_OBJ_TARGET = $(BUILD)/lib/libpcre.$(SHLIB_EXT)

compile-pcre: install-pcre
install-pcre: $(PCRE_OBJ_TARGET)

pcre-$(PCRE_VER).tar.bz2:
	$(WGET) ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/$@
pcre-$(PCRE_VER)/configure: pcre-$(PCRE_VER).tar.bz2
	tar jxf $<
	touch $@
pcre-$(PCRE_VER)/config.status: pcre-$(PCRE_VER)/configure
	cd pcre-$(PCRE_VER) && \
	./configure $(CONFIGURE_COMMON) --enable-utf --enable-unicode-properties --enable-jit
$(PCRE_OBJ_TARGET): pcre-$(PCRE_VER)/config.status
	$(MAKE) -C pcre-$(PCRE_VER) $(LIBTOOL_CCLD) install
	$(INSTALL_NAME_CMD)libpcre.dylib $@
ifeq ($(OS),WINNT)
	-rm $(BUILD)/lib/libpcre.dll
	mv $(BUILD)/bin/libpcre.dll $(BUILD)/lib/libpcre.dll
endif
	touch $@

clean-pcre:
	-$(MAKE) -C pcre-$(PCRE_VER) clean
	-rm -f $(PCRE_OBJ_TARGET)
distclean-pcre:
	-rm -rf pcre-$(PCRE_VER).tar.bz2 pcre-$(PCRE_VER)

## Grisu floating-point printing library ##

GRISU_OPTS = $(CXXFLAGS) -O3 -fvisibility=hidden $(fPIC)

compile-double-conversion: double-conversion-$(GRISU_VER)/src/libgrisu_.$(SHLIB_EXT)
install-double-conversion: $(BUILD)/lib/libgrisu.$(SHLIB_EXT)

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
	$(CXX) $(GRISU_OPTS) src/*.o $(LDFLAGS) -shared -dead_strip -o src/libgrisu.$(SHLIB_EXT)
$(BUILD)/lib/libgrisu.$(SHLIB_EXT): double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
	cp -f $< $@
	$(INSTALL_NAME_CMD)libgrisu.dylib $@

clean-double-conversion:
	-rm -f double-conversion-$(GRISU_VER)/src/*.o
	-rm -f double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
distclean-double-conversion:
	-rm -rf double-conversion-$(GRISU_VER).tar.gz double-conversion-$(GRISU_VER)

## openlibm ##

ifeq ($(OS), WINNT) #needs more advanced detection once 64bit build is possible
OPENLIBM_FLAGS = ARCH=i386
else
OPENLIBM_FLAGS = ARCH="$(ARCH)"
endif

OPENLIBM_OBJ_TARGET = $(BUILD)/lib/libopenlibm.$(SHLIB_EXT)
OPENLIBM_OBJ_SOURCE = openlibm/libopenlibm.$(SHLIB_EXT)

openlibm/Makefile:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d openlibm/.git ] && echo exists ))
$(OPENLIBM_OBJ_SOURCE): openlibm/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/modules ] && echo exists ))
$(OPENLIBM_OBJ_SOURCE): $(JULIAHOME)/.git/modules/deps/openlibm/HEAD
endif
$(OPENLIBM_OBJ_SOURCE): openlibm/Makefile
	$(MAKE) -C openlibm $(OPENLIBM_FLAGS) CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC)
$(OPENLIBM_OBJ_TARGET): $(OPENLIBM_OBJ_SOURCE) | $(BUILD)/lib
	cp $< $@
	$(INSTALL_NAME_CMD)libopenlibm.$(SHLIB_EXT) $@
install-openlibm: $(OPENLIBM_OBJ_TARGET) 

clean-openlibm:
	-$(MAKE) -C openlibm distclean
	-rm $(OPENLIBM_OBJ_TARGET)
distclean-openlibm: clean-openlibm      

# openlibm-extras

OPENLIBMEXT_OBJ_TARGET = $(BUILD)/lib/libopenlibm-extras.$(SHLIB_EXT)
OPENLIBMEXT_OBJ_SOURCE = openlibm/libopenlibm-extras.$(SHLIB_EXT)

openlibm-extras/Makefile:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d openlibm/.git ] && echo exists ))
$(OPENLIBMEXT_OBJ_SOURCE): openlibm/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/modules ] && echo exists ))
$(OPENLIBMEXT_OBJ_SOURCE): $(JULIAHOME)/.git/modules/deps/openlibm/HEAD
endif
$(OPENLIBMEXT_OBJ_SOURCE): openlibm/Makefile.extras
	$(MAKE) -C openlibm -f Makefile.extras $(OPENLIBM_FLAGS) CC="$(CC)" FC="$(FC)" USECLANG=$(USECLANG) USEGCC=$(USEGCC)
$(OPENLIBMEXT_OBJ_TARGET): $(OPENLIBMEXT_OBJ_SOURCE) | $(BUILD)/lib
	cp $< $@
	$(INSTALL_NAME_CMD)libopenlibm-extras.$(SHLIB_EXT) $@
install-openlibm-extras: $(OPENLIBMEXT_OBJ_TARGET) $(BUILD)/lib/libFaddeeva_wrapper.$(SHLIB_EXT)

clean-openlibm-extras:
	-$(MAKE) -C openlibm -f Makefile.extras distclean
	-rm $(OPENLIBMEXT_OBJ_TARGET)
distclean-openlibm-extras: clean-openlibm-extras


# Wrapper for openlibm/Faddeeva since ccall doesn't support C99 complex:
$(BUILD)/lib/libFaddeeva_wrapper.$(SHLIB_EXT): Faddeeva_wrapper.c $(OPENLIBMEXT_OBJ_TARGET)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) -I openlibm/Faddeeva Faddeeva_wrapper.c -o $@ -L $(BUILD)/lib -lopenlibm-extras
	$(INSTALL_NAME_CMD)libFaddeeva_wrapper.$(SHLIB_EXT) $@
	touch $@
install-Faddeeva-wrapper: $(BUILD)/lib/libFaddeeva_wrapper.$(SHLIB_EXT)

clean-Faddeeva-wrapper:
	-rm -f $(OPENLIBMEXT_OBJ_TARGET) $(BUILD)/lib/libFaddeeva_wrapper.$(SHLIB_EXT)
distclean-Faddeeva-wrapper: clean-Faddeeva-wrapper

## Rmath ##

RMATH_OBJ_TARGET = $(BUILD)/lib/libRmath.$(SHLIB_EXT)
RMATH_OBJ_SOURCE = Rmath/src/libRmath.$(SHLIB_EXT)

compile-rmath: $(RMATH_OBJ_SOURCE)
install-rmath: $(RMATH_OBJ_TARGET)

$(RMATH_OBJ_SOURCE): Rmath/src/Makefile install-random
	cd Rmath/src && \
	$(MAKE) CC="$(CC)" && \
	$(INSTALL_NAME_CMD)libRmath.$(SHLIB_EXT) libRmath.$(SHLIB_EXT)
$(RMATH_OBJ_TARGET): $(RMATH_OBJ_SOURCE)
	cp $< $@

clean-rmath:
	-cd Rmath/src && $(MAKE) clean
	-rm -f $(RMATH_OBJ_SOURCE) $(RMATH_OBJ_TARGET)
distclean-rmath: clean-rmath

## LIBRANDOM ##

LIBRANDOM_OBJ_TARGET = $(BUILD)/lib/librandom.$(SHLIB_EXT)
LIBRANDOM_OBJ_SOURCE = random/librandom.$(SHLIB_EXT)

LIBRANDOM_CFLAGS = $(CFLAGS) -O3 -finline-functions -fomit-frame-pointer -DNDEBUG -fno-strict-aliasing \
				   --param max-inline-insns-single=1800 -Wmissing-prototypes -Wall  -std=c99 \
				   -DDSFMT_MEXP=19937 $(fPIC) -shared -DDSFMT_DO_NOT_USE_OLD_NAMES
ifeq ($(ARCH), x86_64)
LIBRANDOM_CFLAGS += -msse2 -DHAVE_SSE2
endif

compile-random: $(LIBRANDOM_OBJ_SOURCE)
install-random: $(LIBRANDOM_OBJ_TARGET)

random/dsfmt-$(DSFMT_VER).tar.gz:
	$(WGET_DASH_O) $@ http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz 
	touch $@
random/jl_random.c: random/dsfmt-$(DSFMT_VER).tar.gz
	cd random && \
	mkdir -p dsfmt-$(DSFMT_VER) && \
	tar -C dsfmt-$(DSFMT_VER) --strip-components 1 -xf dsfmt-$(DSFMT_VER).tar.gz && \
	cd dsfmt-$(DSFMT_VER) && patch < ../dSFMT.h.patch && patch < ../dSFMT.c.patch
	touch $@
$(LIBRANDOM_OBJ_SOURCE): random/jl_random.c random/randmtzig.c
	cd random && \
	$(CC) $(CPPFLAGS) $(LIBRANDOM_CFLAGS) $(LDFLAGS) jl_random.c -o librandom.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)librandom.$(SHLIB_EXT) librandom.$(SHLIB_EXT)

$(LIBRANDOM_OBJ_TARGET): $(LIBRANDOM_OBJ_SOURCE)
	cp $< $@

clean-random:
	-rm -f random/librandom.$(SHLIB_EXT)
distclean-random: clean-random
	-cd random && rm -rf *.tar.gz dsfmt-$(DSFMT_VER)

## OpenBLAS ##

ifeq ($(USE_SYSTEM_BLAS), 0)
OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT)
else
OPENBLAS_OBJ_SOURCE = 
endif

OPENBLAS_BUILD_OPTS = CC="$(CC)" FC="$(FC)" RANLIB="$(RANLIB)" FFLAGS="$(FFLAGS) $(JFFLAGS)" USE_THREAD=$(OPENBLAS_USE_THREAD) TARGET=$(OPENBLAS_TARGET_ARCH)
ifeq ($(OPENBLAS_DYNAMIC_ARCH), 1)
OPENBLAS_BUILD_OPTS += DYNAMIC_ARCH=1
endif

ifeq ($(USE_LIB64), 1)
OPENBLAS_BUILD_OPTS += INTERFACE64=1
endif

ifneq ($(BUILD_OS),$(OS))
OPENBLAS_BUILD_OPTS += OSNAME=$(OS) CROSS=1 
ifeq ($(ARCH),i686)
OPENBLAS_BUILD_OPTS += BINARY=32
else
$(error "unknown arch for openblas cross-compile")
endif
endif

compile-openblas: $(OPENBLAS_OBJ_SOURCE)
install-openblas: $(BUILD)/lib/libopenblas.$(SHLIB_EXT)

openblas-$(OPENBLAS_VER).tar.gz:
	$(WGET_DASH_O) $@ https://github.com/xianyi/OpenBLAS/tarball/$(OPENBLAS_VER) 
openblas-$(OPENBLAS_VER)/Makefile: openblas-$(OPENBLAS_VER).tar.gz
	mkdir -p openblas-$(OPENBLAS_VER) && \
	tar -C openblas-$(OPENBLAS_VER) --strip-components 1 -xf $<
	perl -i -ple 's/^\s*(EXTRALIB\s*\+=\s*-lSystemStubs)\s*$$/# $$1/g' $@.system
	touch $@
$(OPENBLAS_OBJ_SOURCE): openblas-$(OPENBLAS_VER)/Makefile
	$(MAKE) -C openblas-$(OPENBLAS_VER) $(OPENBLAS_BUILD_OPTS)
$(BUILD)/lib/libopenblas.$(SHLIB_EXT): $(OPENBLAS_OBJ_SOURCE) | $(BUILD)/lib
	cp -f openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT) $(BUILD)/lib
	$(INSTALL_NAME_CMD)libopenblas.$(SHLIB_EXT) $(BUILD)/lib/libopenblas.$(SHLIB_EXT)

clean-openblas:
	-$(MAKE) -C openblas-$(OPENBLAS_VER) clean
distclean-openblas:
	-rm -rf openblas-$(OPENBLAS_VER).tar.gz openblas-$(OPENBLAS_VER)

## LAPACK ##

ifeq ($(USE_SYSTEM_LAPACK), 0)
LAPACK_OBJ_TARGET = $(BUILD)/lib/liblapack.$(SHLIB_EXT)
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
	-$(MAKE) -C lapack-$(LAPACK_VER) clean
	-rm -f $(LAPACK_OBJ_SOURCE) $(LAPACK_OBJ_TARGET)
distclean-lapack:
	-rm -rf lapack-$(LAPACK_VER).tgz lapack-$(LAPACK_VER)

## ARPACK ##

ifeq ($(USE_LIB64), 1)
ARPACK_FFLAGS = -fdefault-integer-8
endif

ARPACK_OBJ_TARGET = $(BUILD)/lib/libarpack.$(SHLIB_EXT)
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
	./configure $(CONFIGURE_COMMON) --with-blas="$(LIBBLAS)" --with-lapack="$(LIBLAPACK)" --disable-mpi --enable-shared FFLAGS="$(ARPACK_FFLAGS)"
	touch $@
$(ARPACK_OBJ_TARGET): $(ARPACK_OBJ_SOURCE) | $(BUILD)/lib
	cd arpack-ng_$(ARPACK_VER) && \
	$(MAKE) install F77="$(FC)" MPIF77="$(FC)"
	$(INSTALL_NAME_CMD)libarpack.$(SHLIB_EXT) $(BUILD)/lib/libarpack.$(SHLIB_EXT)
ifeq ($(OS), Linux)
	for filename in $(BUILD)/lib/libarpack.so* ; do \
		$(BUILD)/bin/patchelf --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch $@

clean-arpack:
	-$(MAKE) -C arpack-ng_$(ARPACK_VER) clean
	-rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	-rm -rf arpack-ng_$(ARPACK_VER).tar.gz arpack-ng_$(ARPACK_VER)

## FFTW ##

FFTW_SINGLE_OBJ_TARGET = $(BUILD)/lib/libfftw3f.3.$(SHLIB_EXT)
FFTW_DOUBLE_OBJ_TARGET = $(BUILD)/lib/libfftw3.3.$(SHLIB_EXT)

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
	(./configure $(CONFIGURE_COMMON) $(FFTW_CONFIG) --enable-avx --enable-single || \
	  ./configure $(CONFIGURE_COMMON) $(FFTW_CONFIG) --enable-single) && \
	$(MAKE) clean
	touch $@
$(FFTW_SINGLE_OBJ_TARGET): fftw-$(FFTW_VER)-single/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-single install
ifeq ($(OS), Darwin)
	$(INSTALL_NAME_CMD)libfftw3f.dylib $(BUILD)/lib/libfftw3f.dylib
	$(INSTALL_NAME_CMD)libfftw3f_threads.$(SHLIB_EXT) $(BUILD)/lib/libfftw3f_threads.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) $(BUILD)/lib/libfftw3f.3.dylib $(INSTALL_NAME_ID_DIR)libfftw3f.dylib $(BUILD)/lib/libfftw3f_threads.dylib
else ifeq ($(OS),WINNT)
	-rm $(BUILD)/lib/libfftw3f.dll
	mv $(BUILD)/bin/libfftw3f-3.dll $(BUILD)/lib/libfftw3f.dll
else ifeq ($(OS), Linux)
	for filename in $(BUILD)/lib/libfftw3f_threads.so* ; do \
		$(BUILD)/bin/patchelf --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch $@

fftw-$(FFTW_VER)-double/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-double && \
	tar -C fftw-$(FFTW_VER)-double --strip-components 1 -xf $<
	touch $@
fftw-$(FFTW_VER)-double/config.status: fftw-$(FFTW_VER)-double/configure
	cd fftw-$(FFTW_VER)-double && \
	(./configure $(CONFIGURE_COMMON) $(FFTW_CONFIG) --enable-avx || \
	  ./configure $(CONFIGURE_COMMON) $(FFTW_CONFIG)) && \
	$(MAKE) clean
	touch $@
$(FFTW_DOUBLE_OBJ_TARGET): fftw-$(FFTW_VER)-double/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-double install
ifeq ($(OS), Darwin)
	$(INSTALL_NAME_CMD)libfftw3.$(SHLIB_EXT) $(BUILD)/lib/libfftw3.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libfftw3_threads.$(SHLIB_EXT) $(BUILD)/lib/libfftw3_threads.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) $(BUILD)/lib/libfftw3.3.dylib $(INSTALL_NAME_ID_DIR)libfftw3.dylib $(BUILD)/lib/libfftw3_threads.dylib
else ifeq ($(OS),WINNT)
	-rm $(BUILD)/lib/libfftw3.dll
	mv $(BUILD)/bin/libfftw3-3.dll $(BUILD)/lib/libfftw3.dll
else ifeq ($(OS), Linux)
	for filename in $(BUILD)/lib/libfftw3_threads.so* ; do \
		$(BUILD)/bin/patchelf --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch $@

clean-fftw: clean-fftw-single clean-fftw-double
clean-fftw-single:
	-$(MAKE) -C fftw-$(FFTW_VER)-single clean
	-rm -f $(FFTW_SINGLE_OBJ_TARGET)
clean-fftw-double:
	-$(MAKE) -C fftw-$(FFTW_VER)-double clean
	-rm -f $(FFTW_DOUBLE_OBJ_TARGET)
distclean-fftw:
	-rm -rf fftw-$(FFTW_VER).tar.gz fftw-$(FFTW_VER)-{single,double}


## SUITESPARSE ##

ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
SUITESPARSE_OBJ_SOURCE = SuiteSparse-$(SUITESPARSE_VER)/UMFPACK/Lib/libumfpack.a
SUITESPARSE_OBJ_TARGET = $(BUILD)/lib/libumfpack.$(SHLIB_EXT)
endif

ifeq ($(USE_LIB64), 1)
UMFPACK_CONFIG = -DLONGBLAS='long long' 
CHOLMOD_CONFIG = -DLONGBLAS='long long' 
endif

SUITE_SPARSE_LIB = -lm
ifneq ($(OS), Darwin)
ifneq ($(OS), WINNT)
  SUITE_SPARSE_LIB += -lrt
endif
endif

compile-suitesparse: $(SUITESPARSE_OBJ_SOURCE)
install-suitesparse: $(SUITESPARSE_OBJ_TARGET) $(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT)

SuiteSparse-$(SUITESPARSE_VER).tar.gz:
	$(WGET) http://www.cise.ufl.edu/research/sparse/SuiteSparse/SuiteSparse-$(SUITESPARSE_VER).tar.gz
SuiteSparse-$(SUITESPARSE_VER)/Makefile: SuiteSparse-$(SUITESPARSE_VER).tar.gz
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)
	tar -C SuiteSparse-$(SUITESPARSE_VER) --strip-components 1 -zxf $<
	touch $@
$(SUITESPARSE_OBJ_SOURCE): $(OPENBLAS_OBJ_SOURCE) SuiteSparse-$(SUITESPARSE_VER)/Makefile
	cd SuiteSparse-$(SUITESPARSE_VER) && \
	$(MAKE) CC="$(CC)" CXX="$(CXX)" F77="$(FC)" AR="$(AR)" RANLIB="$(RANLIB)" BLAS="$(LIBBLAS)" LAPACK="$(LIBLAPACK)" \
	  INSTALL_LIB="$(BUILD)/lib" INSTALL_INCLUDE="$(BUILD)/include" LIB="$(SUITE_SPARSE_LIB)" \
	  UMFPACK_CONFIG="$(UMFPACK_CONFIG)" CHOLMOD_CONFIG="$(CHOLMOD_CONFIG)"
	touch $@

ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
$(SUITESPARSE_OBJ_TARGET): $(SUITESPARSE_OBJ_SOURCE)
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)/lib && \
	cd SuiteSparse-$(SUITESPARSE_VER)/lib && \
	rm -f *.a && \
	cp -f `find .. -name libamd.a -o -name libcolamd.a -o -name libcholmod.a -o -name libumfpack.a -o -name libsuitesparseconfig.a -o -name libspqr.a 2>/dev/null` . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(BUILD)/lib/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libcolamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(BUILD)/lib/libcolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libcholmod.$(SHLIB_EXT) -L$(BUILD)/lib -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(BUILD)/lib/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libumfpack.$(SHLIB_EXT) -L$(BUILD)/lib -lcholmod -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(BUILD)/lib/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libspqr.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libspqr.$(SHLIB_EXT) -L$(BUILD)/lib -lcholmod -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(BUILD)/lib/libspqr.$(SHLIB_EXT)
endif

clean-suitesparse:
	-$(MAKE) -C SuiteSparse-$(SUITESPARSE_VER) clean
	-rm -fr SuiteSparse-$(SUITESPARSE_VER)/lib
distclean-suitesparse: clean-suitesparse
	-rm -rf SuiteSparse-$(SUITESPARSE_VER).tar.gz SuiteSparse-$(SUITESPARSE_VER)

# SUITESPARSE WRAPPER

ifeq ($(USE_SYSTEM_SUITESPARSE), 1)
SUITESPARSE_INC = -I /usr/include/suitesparse
SUITESPARSE_LIB = -lumfpack -lcholmod -lamd -lcamd -lcolamd -lspqr
else
SUITESPARSE_INC = -I SuiteSparse-$(SUITESPARSE_VER)/CHOLMOD/Include -I SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse_config -I SuiteSparse-$(SUITESPARSE_VER)/SPQR/Include
SUITESPARSE_LIB = -L$(BUILD)/lib -lcholmod -lumfpack -lspqr $(RPATH_ORIGIN)
endif

$(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT): SuiteSparse_wrapper.c $(SUITESPARSE_OBJ_TARGET)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(SUITESPARSE_INC) SuiteSparse_wrapper.c -o $(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT) $(SUITESPARSE_LIB)
	$(INSTALL_NAME_CMD)libsuitesparse_wrapper.$(SHLIB_EXT) $@
	touch $@
install-suitesparse-wrapper: $(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT)

clean-suitesparse-wrapper:
	-rm -f $(SUITESPARSE_OBJ_TARGET) $(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT)
distclean-suitesparse-wrapper: clean-suitesparse-wrapper

## CLP ##

CLP_OBJ_TARGET = $(BUILD)/lib/libClp.$(SHLIB_EXT)

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
	./configure $(CONFIGURE_COMMON) MPIF77="$(FC)" MPICXX="$(CXX)" MPICC="$(CC)"
$(CLP_OBJ_TARGET): clp-$(CLP_VER)/config.status
	$(MAKE) -C clp-$(CLP_VER) install
	touch $@

clean-clp:
	-$(MAKE) -C clp-$(CLP_VER) clean
	-rm -f $(CLP_OBJ_TARGET)
distclean-clp:
	-rm -rf clp-$(CLP_VER).tar.gz clp-$(CLP_VER)

## UNWIND ##

ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
LIBUNWIND_TARGET_OBJ = $(BUILD)/lib/libunwind.a
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
	cd libunwind-$(UNWIND_VER) && ./configure $(CONFIGURE_COMMON) CFLAGS="$(LIBUNWIND_CFLAGS)"

$(LIBUNWIND_TARGET_SOURCE): libunwind-$(UNWIND_VER)/Makefile
	cd libunwind-$(UNWIND_VER) && $(MAKE)

$(LIBUNWIND_TARGET_OBJ): $(LIBUNWIND_TARGET_SOURCE)
	cd libunwind-$(UNWIND_VER) && $(MAKE) install

clean-unwind:
	-$(MAKE) -C libunwind-$(UNWIND_VER) clean
	-rm -f $(LIBUNWIND_TARGET_OBJ) $(LIBUNWIND_TARGET_SOURCE)

distclean-unwind:
	-rm -rf libunwind-$(UNWIND_VER).tar.gz libunwind-$(UNWIND_VER)

## GNU LIGHTTPD ##

ifeq ($(USE_SYSTEM_LIGHTTPD), 0)
LIGHTTPD_OBJ_TARGET = $(BUILD)/sbin/lighttpd
else
LIGHTTPD_OBJ_TARGET = 
endif

compile-lighttpd: install-lighttpd
install-lighttpd: $(LIGHTTPD_OBJ_TARGET) lighttpd.conf
	mkdir -p $(BUILD)/etc
	cp -f lighttpd.conf $(BUILD)/etc/lighttpd.conf

lighttpd-$(LIGHTTPD_VER).tar.gz:
	$(WGET) http://download.lighttpd.net/lighttpd/releases-1.4.x/lighttpd-$(LIGHTTPD_VER).tar.gz
lighttpd-$(LIGHTTPD_VER)/configure: lighttpd-$(LIGHTTPD_VER).tar.gz
	tar zxf $<
	touch $@
lighttpd-$(LIGHTTPD_VER)/config.status: lighttpd-$(LIGHTTPD_VER)/configure
	mkdir -p $(BUILD)/lib/lighttpd && \
	cd lighttpd-$(LIGHTTPD_VER) && \
	./configure $(CONFIGURE_COMMON) --libdir=$(BUILD)/lib/lighttpd --without-pcre --without-zlib --without-bzip2
$(LIGHTTPD_OBJ_TARGET): lighttpd-$(LIGHTTPD_VER)/config.status
	$(MAKE) -C lighttpd-$(LIGHTTPD_VER) install
	$(INSTALL_NAME_CMD)lighttpd/liblightcomp.$(SHLIB_EXT) $(BUILD)/lib/lighttpd/liblightcomp.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) $(BUILD)/lib/lighttpd/liblightcomp.dylib @executable_path/../lib/lighttpd/liblightcomp.dylib  $(BUILD)/sbin/lighttpd
	touch $@

clean-lighttpd:
	-$(MAKE) -C lighttpd-$(LIGHTTPD_VER) clean
	-rm -f $(LIGHTTPD_OBJ_TARGET)
distclean-lighttpd:
	-rm -rf lighttpd-$(LIGHTTPD_VER).tar.gz lighttpd-$(LIGHTTPD_VER)

## GMP ##

ifeq ($(USE_SYSTEM_GMP), 0)
GMP_OBJ_TARGET = $(BUILD)/lib/libgmp.$(SHLIB_EXT)
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
	./configure --prefix=$(abspath $(BUILD)) CC="$(CC)" CXX="$(CXX)" --enable-shared --disable-static
$(GMP_OBJ_TARGET): gmp-$(GMP_VER)/config.status
	$(MAKE) -C gmp-$(GMP_VER) $(LIBTOOL_CCLD)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C gmp-$(GMP_VER) $(LIBTOOL_CCLD) check
endif
	$(MAKE) -C gmp-$(GMP_VER) $(LIBTOOL_CCLD) install
	$(INSTALL_NAME_CMD)libgmp.dylib $@
	touch $@

clean-gmp:
	-$(MAKE) -C gmp-$(GMP_VER) clean
	-rm -f $(GMP_OBJ_TARGET)
distclean-gmp:
	-rm -rf gmp-$(GMP_VER).tar.bz2 gmp-$(GMP_VER)

## GMP Wrapper

ifeq ($(USE_SYSTEM_GMP), 1)
GMPW_INC =
GMPW_LIB = -lgmp
else
GMPW_INC = -I $(BUILD)/include
GMPW_LIB = -L$(BUILD)/lib/ -lgmp
endif

$(BUILD)/lib/libgmp_wrapper.$(SHLIB_EXT): gmp_wrapper.c $(GMP_OBJ_TARGET) | $(BUILD)/lib
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(GMPW_INC) gmp_wrapper.c -o $(BUILD)/lib/libgmp_wrapper.$(SHLIB_EXT) $(RPATH_ORIGIN) $(GMPW_LIB)
	$(INSTALL_NAME_CMD)libgmp_wrapper.$(SHLIB_EXT) $@
	touch $@
install-gmp-wrapper: $(BUILD)/lib/libgmp_wrapper.$(SHLIB_EXT)

clean-gmp-wrapper:
	-rm -f $(GMP_OBJ_TARGET) $(BUILD)/lib/libgmp_wrapper.$(SHLIB_EXT)
distclean-gmp-wrapper: clean-gmp-wrapper

## GLPK ##

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPK_OBJ_TARGET =
else
GLPK_OBJ_TARGET = $(BUILD)/lib/libglpk.$(SHLIB_EXT)
endif

compile-glpk: install-glpk
install-glpk: $(GLPK_OBJ_TARGET) $(BUILD)/lib/libglpk_wrapper.$(SHLIB_EXT)

glpk-$(GLPK_VER).tar.gz:
	$(WGET) http://ftp.gnu.org/gnu/glpk/$@

glpk-$(GLPK_VER)/configure: glpk-$(GLPK_VER).tar.gz
	tar zxf $<
	touch $@
glpk-$(GLPK_VER)/config.status: glpk-$(GLPK_VER)/configure
	cd glpk-$(GLPK_VER) && \
	./configure $(CONFIGURE_COMMON)
$(GLPK_OBJ_TARGET): glpk-$(GLPK_VER)/config.status
	$(MAKE) -C glpk-$(GLPK_VER) $(LIBTOOL_CCLD) install
	$(INSTALL_NAME_CMD)libglpk.dylib $@
	touch $@

clean-glpk:
	-$(MAKE) -C glpk-$(GLPK_VER) uninstall || true
	-$(MAKE) -C glpk-$(GLPK_VER) clean
distclean-glpk: clean-glpk
	-rm -rf glpk-$(GLPK_VER).tar.bz2 glpk-$(GLPK_VER)


## GLPK Wrapper

ifeq ($(USE_SYSTEM_GLPK), 1)
GLPKW_INC = -I /usr/include/
GLPKW_LIB = -lglpk
else
GLPKW_INC = -I $(BUILD)/include
GLPKW_LIB = -L$(BUILD)/lib -lglpk
endif


$(BUILD)/lib/libglpk_wrapper.$(SHLIB_EXT): glpk_wrapper.c $(GLPK_OBJ_TARGET)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(GLPKW_INC) glpk_wrapper.c $(GLPKW_LIB) -o $(BUILD)/lib/libglpk_wrapper.$(SHLIB_EXT) $(RPATH_ORIGIN)
	$(INSTALL_NAME_CMD)libglpk_wrapper.$(SHLIB_EXT) $@
	touch $@
install-glpk-wrapper: $(BUILD)/lib/libglpk_wrapper.$(SHLIB_EXT) glpk_wrapper.c

clean-glpk-wrapper:
	-rm -f $(GLPK_OBJ_TARGET) $(BUILD)/lib/libglpk_wrapper.$(SHLIB_EXT)
distclean-glpk-wrapper: clean-glpk-wrapper

## ZLIB ##

ifeq ($(USE_SYSTEM_ZLIB), 1)
ZLIB_OBJ_TARGET =
else
ZLIB_OBJ_TARGET = $(BUILD)/lib/libz.$(SHLIB_EXT)
ZLIB_CONFIGFLAGS = CFLAGS="-O3 $(CFLAGS) -D_FILE_OFFSET_BITS=64"
ifneq ($(OS),$(BUILD_OS))
ZLIB_CONFIGFLAGS += CHOST=$(XC_HOST)
else
ZLIB_CONFIGFLAGS += CC="$(CC)"
endif
endif

compile-zlib: install-zlib
install-zlib: $(ZLIB_OBJ_TARGET)

zlib-$(ZLIB_VER).tar.gz:
	$(WGET) http://zlib.net/$@
zlib-$(ZLIB_VER)/configure: zlib-$(ZLIB_VER).tar.gz
	tar zxf $<
	touch $@
zlib-$(ZLIB_VER)/config.status: zlib-$(ZLIB_VER)/configure
ifeq ($(OS), WINNT)
	cd zlib-$(ZLIB_VER) && \
	cp win32/Makefile.gcc Makefile
else
	cd zlib-$(ZLIB_VER) && \
	$(ZLIB_CONFIGFLAGS) ./configure --prefix=$(abspath $(BUILD)) #not an autoconf script
endif
	touch $@
$(ZLIB_OBJ_TARGET): zlib-$(ZLIB_VER)/config.status
	$(MAKE) -C zlib-$(ZLIB_VER) PREFIX=$(CROSS_COMPILE)
ifeq ($(OS), WINNT)
	cp zlib-$(ZLIB_VER)/zlib1.dll $@
else
	$(MAKE) -C zlib-$(ZLIB_VER) check PREFIX=$(CROSS_COMPILE)
	$(MAKE) -C zlib-$(ZLIB_VER) install PREFIX=$(CROSS_COMPILE)
endif
	$(INSTALL_NAME_CMD)libz.dylib $@
	touch $@

clean-zlib:
	-$(MAKE) -j$(PARALLEL_BUILD_JOBS) -C zlib-$(ZLIB_VER) clean
	-rm -f $(ZLIB_OBJ_TARGET)
distclean-zlib:
	-rm -rf zlib-$(ZLIB_VER).tar.gz zlib-$(ZLIB_VER)

## Tk wrapper ##

TKW_INC = -I $(BUILD)/include -I $(JULIAHOME)/src -I $(JULIAHOME)/src/support
TKW_LIB =
ifeq ($(OS), Darwin)
  ifneq ($(wildcard /opt/local/bin/port),)
    TKW_INC += -I/opt/local/include
    TKW_LIB += -L/opt/local/lib
  else ifneq ($(wildcard /usr/local/bin/brew),)
    TKW_INC += -I/usr/local/include
    TKW_LIB += -L/usr/local/lib
  endif
  TKW_INC += -I/opt/X11/include -I/usr/X11/include
else
  TKW_INC += -I/usr/include/tcl
endif
TKW_LIB += -ltcl8.5 -ltk8.5 -L$(BUILD)/lib/ -ljulia-release

$(BUILD)/lib/libtk_wrapper.$(SHLIB_EXT): tk_wrapper.c
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(TKW_INC) tk_wrapper.c $(TKW_LIB) -o $(BUILD)/lib/libtk_wrapper.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libtk_wrapper.$(SHLIB_EXT) $@
	touch $@
install-tk-wrapper: $(BUILD)/lib/libtk_wrapper.$(SHLIB_EXT) tk_wrapper.c

clean-tk-wrapper:
	-rm -f $(BUILD)/lib/libtk_wrapper.$(SHLIB_EXT)
distclean-tk-wrapper: clean-tk-wrapper

## patchelf

PATCHELF_TARGET = $(BUILD)/bin/patchelf

compile-patchelf: install-patchelf
install-patchelf: $(PATCHELF_TARGET)

patchelf-$(PATCHELF_VER).tar.bz2:
	$(WGET) http://hydra.nixos.org/build/1524660/download/2/$@
patchelf-$(PATCHELF_VER)/configure: patchelf-$(PATCHELF_VER).tar.bz2
	tar jxf $<
	touch $@
patchelf-$(PATCHELF_VER)/config.status: patchelf-$(PATCHELF_VER)/configure
	cd patchelf-$(PATCHELF_VER) && \
	./configure $(CONFIGURE_COMMON)
$(PATCHELF_TARGET): patchelf-$(PATCHELF_VER)/config.status
	$(MAKE) -C patchelf-$(PATCHELF_VER) install
	touch $@

clean-patchelf:
	-$(MAKE) -C patchelf-$(PATCHELF_VER) clean
	-rm -f $(PATCHELF_OBJ_TARGET)
distclean-patchelf:
	-rm -rf patchelf-$(PATCHELF_VER).tar.bz2 patchelf-$(PATCHELF_VER)

## phony targets ##

.PHONY: \
	default compile install cleanall distclean \
	compile-* install-* clean-* distclean-*
