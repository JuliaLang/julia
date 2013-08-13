include Versions.make

## optional packages and options #
LLVM_ASSERTIONS = 0
LLVM_DEBUG = 0
# set to 1 to get clang and compiler-rt
BUILD_LLVM_CLANG = 0
# set to 1 to get lldb (does not work with llvm3.1 and earlier)
# see http://lldb.llvm.org/build.html for dependancies
BUILD_LLDB = 0

## high-level setup ##

JULIAHOME = $(abspath ..)
include $(JULIAHOME)/Make.inc
CONFIGURE_COMMON = --prefix=$(abspath $(BUILD)) F77="$(FC)" CC="$(CC)" CXX="$(CXX)" --build=$(BUILD_MACHINE)
ifneq ($(XC_HOST),)
CONFIGURE_COMMON += --host=$(XC_HOST)
endif
ifeq ($(OS),WINNT)
CONFIGURE_COMMON += LDFLAGS=-Wl,--stack,8388608
endif

#autoconf configure-driven scripts: llvm readline pcre arpack fftw unwind gmp mpfr patchelf uv
#custom configure-driven script: zlib
#custom Makefile rules: openlibm Rmath double-conversion random suitesparse-wrapper suitesparse lapack openblas

# prevent installing libs into usr/lib64 on opensuse
unexport CONFIG_SITE

STAGE1_DEPS = random double-conversion
STAGE2_DEPS =
STAGE3_DEPS = suitesparse-wrapper

ifeq ($(USE_SYSTEM_LIBUV), 0)
STAGE1_DEPS += uv
endif

ifeq ($(USE_SYSTEM_RMATH), 0)
STAGE1_DEPS += Rmath
endif

ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
ifeq ($(OS), Linux)
STAGE1_DEPS += unwind
else ifeq ($(OS), FreeBSD)
STAGE1_DEPS += unwind
endif
endif

ifeq ($(OS), Linux)
ifneq ($(shell patchelf --version 2>/dev/null), patchelf 0.6)
STAGE1_DEPS += patchelf
PATCHELF=$(BUILD)/bin/patchelf
else
PATCHELF=patchelf
endif
endif

## USE_SYSTEM_LIBS options

ifeq ($(USE_SYSTEM_OPENLIBM), 0)
# No matter if we are using a system-libm or not, if we don't have a system-openlibm, we need the extras
STAGE1_DEPS += openlibm-extras
ifeq ($(USE_SYSTEM_LIBM), 0)
STAGE1_DEPS += openlibm
endif
endif

ifeq ($(USE_SYSTEM_LLVM), 0)
STAGE1_DEPS += llvm
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

#ifeq ($(USE_SYSTEM_ZLIB), 0)
#STAGE1_DEPS += zlib
#endif

ifeq ($(USE_SYSTEM_MPFR), 0)
STAGE2_DEPS += mpfr
ifeq ($(USE_SYSTEM_GMP), 0)
MPFR_OPTS = --with-gmp=$(abspath $(BUILD))
endif
endif
ifeq ($(BUILD_OS),WINNT)
ifeq ($(OS),WINNT)
MPFR_OPTS += --disable-thread-safe CFLAGS="-DNPRINTF_L -DNPRINTF_T -DNPRINTF_J"
endif
endif

ifeq ($(USE_SYSTEM_ARPACK), 0)
STAGE2_DEPS += arpack
endif

ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
STAGE2_DEPS += suitesparse
endif

# Only compile standalone LAPACK if we are not using OpenBLAS.
# OpenBLAS otherwise compiles LAPACK as part of its build.
# This is useful where one wants to use the vendor BLAS, but 
# build LAPACK as the vendor LAPACK may be too old (eg. Apple vecLib)
ifeq ($(USE_SYSTEM_BLAS), 1)
ifeq ($(USE_SYSTEM_LAPACK), 0)
STAGE2_DEPS += lapack
endif
endif

#Platform specific flags

ifeq ($(OS), WINNT)
LIBTOOL_CCLD = CCLD="$(CC) -no-undefined -avoid-version"
AUTOTOOLS_PATH_CORRECT = mv $(BUILD)/bin/$(@F) $(BUILD)/lib/$(@F)
endif

## Common build target prefixes

LIBS = $(STAGE1_DEPS) $(STAGE2_DEPS) $(STAGE3_DEPS)

default: $(BUILD) install
get: $(addprefix get-, $(LIBS))
configure: $(addprefix configure-, $(LIBS))
compile: $(addprefix compile-, $(LIBS))
check: $(addprefix check-, $(LIBS))
install: $(addprefix install-, $(LIBS))
cleanall: $(addprefix clean-, $(LIBS))
distclean: $(addprefix distclean-, $(LIBS))
	rm -rf $(BUILD)
getall: get-llvm get-readline get-uv get-pcre get-double-conversion get-openlibm get-random get-openblas get-lapack get-fftw get-suitesparse get-arpack get-unwind get-gmp get-mpfr get-zlib get-patchelf

## PATHS ##
DIRS = $(addprefix $(BUILD)/,lib include bin share etc)

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))

$(BUILD): $(DIRS)

## LLVM ##
ifeq ($(BUILD_LLDB), 1)
BUILD_LLVM_CLANG = 1
# because it's a build requirement
endif

ifeq ($(LLVM_DEBUG),1)
LLVM_BUILDTYPE = Debug
else
LLVM_BUILDTYPE = Release
endif
ifeq ($(LLVM_ASSERTIONS),1)
LLVM_BUILDTYPE := $(LLVM_BUILDTYPE)+Asserts
endif

LLVM_LIB_FILE = libLLVMJIT.a
LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/$(LLVM_BUILDTYPE)/lib/$(LLVM_LIB_FILE)
LLVM_OBJ_TARGET = $(BUILD)/lib/$(LLVM_LIB_FILE)

ifneq ($(LLVM_VER),svn)
ifeq ($(LLVM_VER), 3.0)
LLVM_TAR=llvm-$(LLVM_VER).tar.gz
else
LLVM_TAR=llvm-$(LLVM_VER).src.tar.gz
endif
endif

ifeq ($(BUILD_LLVM_CLANG),1)
ifeq ($(LLVM_VER), 3.0)
LLVM_CLANG_TAR=clang-$(LLVM_VER).tar.gz
LLVM_COMPILER_RT_TAR=
else ifeq ($(LLVM_VER), 3.3)
LLVM_CLANG_TAR=cfe-$(LLVM_VER).src.tar.gz
LLVM_COMPILER_RT_TAR=compiler-rt-$(LLVM_VER).src.tar.gz
else
LLVM_CLANG_TAR=clang-$(LLVM_VER).src.tar.gz
LLVM_COMPILER_RT_TAR=compiler-rt-$(LLVM_VER).src.tar.gz
endif
else
LLVM_CLANG_TAR=
LLVM_COMPILER_RT_TAR=
endif

LLVM_TARGET_FLAGS= --enable-targets=host
LLVM_FLAGS =  --disable-threads --disable-profiling --enable-shared --enable-static $(LLVM_TARGET_FLAGS) --disable-bindings --disable-docs
LLVM_MFLAGS =
ifeq ($(LLVM_ASSERTIONS), 1)
LLVM_FLAGS += --enable-assertions 
ifeq ($(OS), WINNT)
LLVM_FLAGS += --disable-embed-stdcxx
endif
else
LLVM_FLAGS += --disable-assertions
endif
ifeq ($(LLVM_DEBUG), 1)
LLVM_FLAGS += --disable-optimized
else
LLVM_FLAGS += --enable-optimized
endif
ifeq ($(OS), WINNT)
LLVM_FLAGS += --with-extra-ld-options="-Wl,--stack,8388608" LDFLAGS="" --disable-shared
LLVM_MFLAGS += CPPFLAGS="-D__USING_SJLJ_EXCEPTIONS__ -D__CRT__NO_INLINE"
endif

ifeq ($(BUILD_LLDB),1)
ifeq ($(USECLANG),1)
LLVM_FLAGS += --enable-cxx11
else
LLVM_MFLAGS += CXXFLAGS=-std=c++0x
endif
endif

ifeq ($(ARCH), ppc64)
  LLVM_FLAGS += CXXFLAGS=-mminimal-toc
  LLVM_MFLAGS += CXXFLAGS=-mminimal-toc
endif

ifneq ($(LLVM_CLANG_TAR),)
$(LLVM_CLANG_TAR):
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$@
endif
ifneq ($(LLVM_COMPILER_RT_TAR),)
$(LLVM_COMPILER_RT_TAR):
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$@
endif
ifneq ($(LLVM_VER),svn)
$(LLVM_TAR):
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$@
endif

ifeq ($(BUILD_LLDB),1)
llvm-$(LLVM_VER)/tools/lldb:
llvm-$(LLVM_VER)/configure: llvm-$(LLVM_VER)/tools/lldb
endif

## LLVM needs python 2.x, but doesn't check for it, so we have to use an ugly workaround to make it compile
llvm-$(LLVM_VER)/python2_path:
	mkdir -p llvm-$(LLVM_VER)/python2_path
	-python -c 'import sys; sys.exit(not sys.version_info > (3, 0))' && \
	/usr/bin/python2 -c 'import sys; sys.exit(not sys.version_info < (3, 0))' && \
	ln -sf /usr/bin/python2 "llvm-$(LLVM_VER)/python2_path/python"
llvm_python_workaround=llvm-$(LLVM_VER)/python2_path


llvm-$(LLVM_VER)/configure: $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR)
ifneq ($(LLVM_VER),svn)
	mkdir -p llvm-$(LLVM_VER) && \
	tar -C llvm-$(LLVM_VER) --strip-components 1 -xf $(LLVM_TAR)
else
	([ ! -d llvm-$(LLVM_VER) ] && \
		git clone http://llvm.org/git/llvm.git llvm-$(LLVM_VER) ) || \
		(cd llvm-$(LLVM_VER) && \
		git pull --ff-only)
endif
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
		git fetch))
	cd llvm-$(LLVM_VER)/tools/lldb && git checkout origin/release_32
endif
ifeq ($(OS),WINNT)
ifeq ($(ARCH),x86_64)
ifeq ($(LLVM_VER),3.3)
	patch -p0 < win64-int128.llvm-3.3.patch
endif
endif
endif
	touch -c $@

llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/config.status: llvm-$(LLVM_VER)/configure | $(llvm_python_workaround)
	cd llvm-$(LLVM_VER) && \
	mkdir -p build_$(LLVM_BUILDTYPE) && cd build_$(LLVM_BUILDTYPE) && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	../configure $(CONFIGURE_COMMON) $(LLVM_FLAGS)
	touch -c $@

$(LLVM_OBJ_SOURCE): llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/config.status | $(llvm_python_workaround)
	cd llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE) && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) $(LLVM_MFLAGS)
	touch -c $@

llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/checked: $(LLVM_OBJ_SOURCE) | $(llvm_python_workaround)
ifeq ($(OS),$(BUILD_OS))
	cd llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE) && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) $(LLVM_MFLAGS) check 
endif
	echo 1 > $@

$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE) | $(llvm_python_workaround)
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) -C llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE) install
	touch -c $@

clean-llvm:
	-$(MAKE) -C llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE) clean
	-rm -f $(BUILD)/bin/llvm-config
distclean-llvm:
	-rm -rf llvm-$(LLVM_VER).tar.gz llvm-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).tar.gz compiler-rt-$(LLVM_VER).src.tar.gz llvm-$(LLVM_VER)

get-llvm: $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR)
configure-llvm: llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/config.status
compile-llvm: $(LLVM_OBJ_SOURCE)
check-llvm: llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/checked
install-llvm: $(LLVM_OBJ_TARGET)
#todo: LLVM make check target is broken on julia.mit.edu (and really slow elsewhere)


## GNU readline ##


ifeq ($(OS),WINNT)
READLINE_OBJ_TARGET = $(BUILD)/lib/libreadline.a
READLINE_OBJ_SOURCE = readline-$(READLINE_VER)/libreadline.a
READLINE_URL = https://github.com/JuliaLang/readline/tarball/master
READLINE_OPTS = --disable-shared --enable-static --with-curses
READLINE_CFLAGS = LOCAL_DEFS="-include $(abspath .)/readline-win.h"
readline-$(READLINE_VER).tar.gz:
	$(JLDOWNLOAD) $@ $(READLINE_URL)
	touch -c $@
readline-$(READLINE_VER)/configure: readline-$(READLINE_VER).tar.gz
	mkdir readline-$(READLINE_VER)
	tar -C readline-$(READLINE_VER) --strip-components 1 -xf $<
	touch -c $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE) readline-$(READLINE_VER)/checked
	$(MAKE) -C readline-$(READLINE_VER) $(READLINE_CFLAGS) install
	touch -c $@
else
READLINE_OBJ_TARGET = $(BUILD)/lib/libreadline.$(SHLIB_EXT)
ifneq ($(OS),Darwin)
READLINE_OBJ_SOURCE = readline-$(READLINE_VER)/shlib/libreadline.$(SHLIB_EXT).$(READLINE_VER)
else
READLINE_OBJ_SOURCE = readline-$(READLINE_VER)/shlib/libreadline.$(READLINE_VER).$(SHLIB_EXT)
endif
READLINE_URL = ftp://ftp.gnu.org/gnu/readline/readline-$(READLINE_VER).tar.gz
READLINE_OPTS = --enable-shared --enable-static --with-curses
READLINE_CFLAGS = 
readline-$(READLINE_VER).tar.gz:
	$(JLDOWNLOAD) readline62-001 http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-001
	$(JLDOWNLOAD) readline62-002 http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-002
	$(JLDOWNLOAD) readline62-003 http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-003
	$(JLDOWNLOAD) readline62-004 http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-004
	$(JLDOWNLOAD) $@ http://ftp.gnu.org/gnu/readline/$@
	touch -c $@
readline-$(READLINE_VER)/configure: readline-$(READLINE_VER).tar.gz
	tar zxf $<
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-001
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-002
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-003
	cd readline-$(READLINE_VER) && patch -p0 < ../readline62-004
	touch -c $@
$(READLINE_OBJ_TARGET): $(READLINE_OBJ_SOURCE) readline-$(READLINE_VER)/checked
	$(MAKE) -C readline-$(READLINE_VER) install
	chmod +w $(BUILD)/lib/libreadline.* $(BUILD)/lib/libhistory.*
ifeq ($(OS), Darwin)
	$(INSTALL_NAME_CMD)libreadline.$(SHLIB_EXT) $(BUILD)/lib/libreadline.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libhistory.dylib $(BUILD)/lib/libhistory.dylib
else ifeq ($(OS), Linux)
	for filename in $(BUILD)/lib/libhistory.so* $(BUILD)/lib/libreadline.so* ; do \
		$(PATCHELF) --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch -c $@
endif

readline-$(READLINE_VER)/config.status: readline-$(READLINE_VER)/configure
	cd readline-$(READLINE_VER) && \
	chmod a+x ./configure && \
	./configure $(CONFIGURE_COMMON) $(READLINE_OPTS)
	touch -c $@
$(READLINE_OBJ_SOURCE): readline-$(READLINE_VER)/config.status
	cd readline-$(READLINE_VER) && \
	$(MAKE) $(READLINE_CFLAGS)
	touch -c $@
readline-$(READLINE_VER)/checked: $(READLINE_OBJ_SOURCE)
	cd readline-$(READLINE_VER) && \
	$(MAKE) check $(READLINE_CFLAGS)
	echo 1 > $@

clean-readline:
	-$(MAKE) -C readline-$(READLINE_VER) clean
	-rm -f $(READLINE_OBJ_TARGET)
distclean-readline:
	-rm -rf readline-$(READLINE_VER).tar.gz readline-$(READLINE_VER)

get-readline: readline-$(READLINE_VER).tar.gz
configure-readline: readline-$(READLINE_VER)/config.status
compile-readline: $(READLINE_OBJ_SOURCE)
check-readline: readline-$(READLINE_VER)/checked
install-readline: $(READLINE_OBJ_TARGET) 


## LIBUV ##

UV_SRC_TARGET = libuv/.libs/libuv.a
UV_OBJ_TARGET = $(BUILD)/lib/libuv.a

libuv/configure:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d libuv/.git ] && echo exists ))
libuv/config.status: libuv/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/.git/modules/deps/libuv ] && echo exists ))
libuv/config.status: $(JULIAHOME)/.git/modules/deps/libuv/HEAD
endif
libuv/config.status: libuv/configure
	cd libuv && \
	./configure --with-pic $(CONFIGURE_COMMON)
	touch -c $@
$(UV_SRC_TARGET): libuv/config.status
	touch -c libuv/aclocal.m4
	touch -c libuv/Makefile.in
	touch -c libuv/configure
	touch -c libuv/config.status
	$(MAKE) -C libuv
	touch -c $@
libuv/checked: $(UV_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	-$(MAKE) -C libuv check
endif
	echo 1 > $@
$(UV_OBJ_TARGET): $(UV_SRC_TARGET)
	$(MAKE) -C libuv install
	$(INSTALL_NAME_CMD)libuv.dylib $(BUILD)/lib/libuv.dylib

clean-uv:
	-$(MAKE) -C libuv clean
	-rm -rf $(BUILD)/lib/libuv.a $(BUILD)/include/uv.h $(BUILD)/include/uv-private
distclean-uv: clean-uv
	-$(MAKE) -C libuv distclean

get-uv: libuv/configure
configure-uv: libuv/config.status
compile-uv: $(UV_SRC_TARGET)
check-uv: libuv/checked
install-uv: $(UV_OBJ_TARGET)


## PCRE ##

PCRE_SRC_TARGET = pcre-$(PCRE_VER)/.libs/libpcre.$(SHLIB_EXT)
PCRE_OBJ_TARGET = $(BUILD)/lib/libpcre.$(SHLIB_EXT)

pcre-$(PCRE_VER).tar.bz2:
	$(JLDOWNLOAD) ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/$@
pcre-$(PCRE_VER)/configure: pcre-$(PCRE_VER).tar.bz2
	tar jxf $<
	touch -c $@
pcre-$(PCRE_VER)/config.status: pcre-$(PCRE_VER)/configure
	cd pcre-$(PCRE_VER) && \
	./configure $(CONFIGURE_COMMON) --enable-utf --enable-unicode-properties --enable-jit
	touch -c $@
$(PCRE_SRC_TARGET): pcre-$(PCRE_VER)/config.status
	$(MAKE) -C pcre-$(PCRE_VER) $(LIBTOOL_CCLD)
	touch -c $@
pcre-$(PCRE_VER)/checked: $(PCRE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C pcre-$(PCRE_VER) check
endif
	echo 1 > $@
$(PCRE_OBJ_TARGET): $(PCRE_SRC_TARGET) pcre-$(PCRE_VER)/checked
	$(MAKE) -C pcre-$(PCRE_VER) $(LIBTOOL_CCLD) install
	$(INSTALL_NAME_CMD)libpcre.dylib $@
ifeq ($(OS),WINNT)
	-rm $(BUILD)/lib/libpcre.dll
	mv $(BUILD)/bin/libpcre.dll $(BUILD)/lib/libpcre.dll
endif
	touch -c $@

clean-pcre:
	-$(MAKE) -C pcre-$(PCRE_VER) clean
	-rm -f $(PCRE_OBJ_TARGET)
distclean-pcre:
	-rm -rf pcre-$(PCRE_VER).tar.bz2 pcre-$(PCRE_VER)

get-pcre: pcre-$(PCRE_VER).tar.bz2
configure-pcre: pcre-$(PCRE_VER)/config.status
compile-pcre: $(PCRE_SRC_TARGET)
check-pcre: pcre-$(PCRE_VER)/checked
install-pcre: $(PCRE_OBJ_TARGET)


## Grisu floating-point printing library ##

GRISU_OPTS = $(CXXFLAGS) -O3 -fvisibility=hidden $(fPIC)
GRISU_OBJ_TARGET = $(BUILD)/lib/libgrisu.$(SHLIB_EXT)

double-conversion-$(GRISU_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://double-conversion.googlecode.com/files/$@
	touch -c $@
double-conversion-$(GRISU_VER)/Makefile: double-conversion-$(GRISU_VER).tar.gz
	mkdir -p double-conversion-$(GRISU_VER) && \
	tar -C double-conversion-$(GRISU_VER) --strip-components 1 -xf double-conversion-$(GRISU_VER).tar.gz
	touch -c $@

ifeq ($(USE_SYSTEM_GRISU), 0)
$(GRISU_OBJ_TARGET): double-conversion-$(GRISU_VER)/Makefile
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
	$(CXX) $(GRISU_OPTS) src/*.o $(LDFLAGS) -shared -dead_strip -o $@ && \
	$(INSTALL_NAME_CMD)libgrisu.dylib $@
else
$(GRISU_OBJ_TARGET):
	$(CXX) $(GRISU_OPTS) -I/usr/include/double-conversion double_conversion_wrapper.cpp -shared $(LDFLAGS) -o $@ -ldouble-conversion && \
	$(INSTALL_NAME_CMD)libgrisu.dylib $@
endif

clean-double-conversion:
	-rm -f double-conversion-$(GRISU_VER)/src/*.o
	-rm -f $(GRISU_OBJ_TARGET)
distclean-double-conversion:
	-rm -rf double-conversion-$(GRISU_VER).tar.gz double-conversion-$(GRISU_VER) $(GRISU_OBJ_TARGET)

get-double-conversion: double-conversion-$(GRISU_VER).tar.gz
configure-double-conversion: get-double-conversion
compile-double-conversion: double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
check-double-conversion: compile-double-conversion
install-double-conversion: $(BUILD)/lib/libgrisu.$(SHLIB_EXT)


## openlibm && openlibm-extras ##

OPENLIBM_FLAGS = ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC)

OPENLIBM_OBJ_TARGET = $(BUILD)/lib/libopenlibm.$(SHLIB_EXT)
OPENLIBM_OBJ_SOURCE = openlibm/libopenlibm.$(SHLIB_EXT)
OPENLIBMEXT_OBJ_TARGET = $(BUILD)/lib/libopenlibm-extras.$(SHLIB_EXT)
OPENLIBMEXT_OBJ_SOURCE = openlibm/libopenlibm-extras.$(SHLIB_EXT)

openlibm/Makefile openlibm/Makefile.extras:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d openlibm/.git ] && echo exists ))
$(OPENLIBM_OBJ_SOURCE): openlibm/.git/HEAD
$(OPENLIBMEXT_OBJ_SOURCE): openlibm/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/.git/modules/deps/openlibm ] && echo exists ))
$(OPENLIBM_OBJ_SOURCE): $(JULIAHOME)/.git/modules/deps/openlibm/HEAD
$(OPENLIBMEXT_OBJ_SOURCE): $(JULIAHOME)/.git/modules/deps/openlibm/HEAD
endif
$(OPENLIBM_OBJ_SOURCE): openlibm/Makefile
	$(MAKE) -C openlibm $(OPENLIBM_FLAGS)
	touch -c $@
$(OPENLIBMEXT_OBJ_SOURCE): openlibm/Makefile.extras
	$(MAKE) -C openlibm -f Makefile.extras $(OPENLIBM_FLAGS)
	touch -c $@
$(OPENLIBM_OBJ_TARGET): $(OPENLIBM_OBJ_SOURCE) | $(BUILD)/lib
	cp openlibm/libopenlibm.a $(BUILD)/lib/libopenlibm.a
	cp $< $@
	$(INSTALL_NAME_CMD)libopenlibm.$(SHLIB_EXT) $@
$(OPENLIBMEXT_OBJ_TARGET): $(OPENLIBMEXT_OBJ_SOURCE) | $(BUILD)/lib
	cp $< $@
	$(INSTALL_NAME_CMD)libopenlibm-extras.$(SHLIB_EXT) $@

clean-openlibm:
	-$(MAKE) -C openlibm distclean $(OPENLIBM_FLAGS)
	-rm $(OPENLIBM_OBJ_TARGET)
distclean-openlibm: clean-openlibm

clean-openlibm-extras:
	-$(MAKE) -C openlibm -f Makefile.extras distclean
	-rm $(OPENLIBMEXT_OBJ_TARGET)
distclean-openlibm-extras: clean-openlibm-extras

get-openlibm: openlibm/Makefile
configure-openlibm: get-openlibm
compile-openlibm: $(OPENLIBM_OBJ_SOURCE)
check-openlibm: compile-openlibm
install-openlibm: $(OPENLIBM_OBJ_TARGET) 

get-openlibm-extras: openlibm/Makefile.extras
configure-openlibm-extras: get-openlibm-extras
compile-openlibm-extras: $(OPENLIBMEXT_OBJ_SOURCE)
check-openlibm-extras: compile-openlibm-extras
install-openlibm-extras: $(OPENLIBMEXT_OBJ_TARGET)

## LIBRANDOM ##

LIBRANDOM_OBJ_TARGET = $(BUILD)/lib/librandom.$(SHLIB_EXT)
LIBRANDOM_OBJ_SOURCE = random/librandom.$(SHLIB_EXT)

LIBRANDOM_CFLAGS = $(CFLAGS) -O3 -finline-functions -fomit-frame-pointer -DNDEBUG -fno-strict-aliasing \
				   --param max-inline-insns-single=1800 -Wmissing-prototypes -Wall  -std=c99 \
				   -DDSFMT_MEXP=19937 $(fPIC) -shared -DDSFMT_DO_NOT_USE_OLD_NAMES
ifeq ($(ARCH), x86_64)
LIBRANDOM_CFLAGS += -msse2 -DHAVE_SSE2
endif

random/dsfmt-$(DSFMT_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz 
	touch -c $@
random/dsfmt-$(DSFMT_VER)/config.status: random/dsfmt-$(DSFMT_VER).tar.gz
	cd random && \
	mkdir -p dsfmt-$(DSFMT_VER) && \
	tar -C dsfmt-$(DSFMT_VER) --strip-components 1 -xf dsfmt-$(DSFMT_VER).tar.gz && \
	cd dsfmt-$(DSFMT_VER) && patch < ../dSFMT.h.patch && patch < ../dSFMT.c.patch
	echo 1 > $@
$(LIBRANDOM_OBJ_SOURCE): random/jl_random.c random/randmtzig.c random/dsfmt-$(DSFMT_VER)/config.status
	cd random && \
	$(CC) $(CPPFLAGS) $(LIBRANDOM_CFLAGS) $(LDFLAGS) jl_random.c -o librandom.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)librandom.$(SHLIB_EXT) librandom.$(SHLIB_EXT)
$(LIBRANDOM_OBJ_TARGET): $(LIBRANDOM_OBJ_SOURCE)
	cp $< $@

clean-random:
	-rm -f random/librandom.$(SHLIB_EXT)
distclean-random: clean-random
	-cd random && rm -rf *.tar.gz dsfmt-$(DSFMT_VER)

get-random: random/dsfmt-$(DSFMT_VER).tar.gz
configure-random: random/dsfmt-$(DSFMT_VER)/config.status
compile-random: $(LIBRANDOM_OBJ_SOURCE)
check-random: compile-random
install-random: $(LIBRANDOM_OBJ_TARGET)


## Rmath ##

RMATH_OBJ_TARGET = $(BUILD)/lib/libRmath.$(SHLIB_EXT)
RMATH_OBJ_SOURCE = Rmath/src/libRmath.$(SHLIB_EXT)

RMATH_FLAGS += CC="$(CC)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) \
			   OS="$(OS)" ARCH="$(ARCH)" \
			   USE_LIBRANDOM=1 LIBRANDOM_PATH="$(BUILD)/lib"

Rmath/Make.inc:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d Rmath/.git ] && echo exists ))
$(RMATH_OBJ_SOURCE): Rmath/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/.git/modules/deps/Rmath ] && echo exists ))
$(RMATH_OBJ_SOURCE): $(JULIAHOME)/.git/modules/deps/Rmath/HEAD
endif
$(RMATH_OBJ_SOURCE): Rmath/Make.inc $(LIBRANDOM_OBJ_TARGET)
	$(MAKE) -C Rmath/src $(RMATH_FLAGS)
	touch -c $@
$(RMATH_OBJ_TARGET): $(RMATH_OBJ_SOURCE) | $(BUILD)/lib
	cp $< $@
	$(INSTALL_NAME_CMD)libRmath.$(SHLIB_EXT) $@

clean-Rmath:
	-$(MAKE) -C Rmath/src clean
	-rm $(RMATH_OBJ_TARGET)
distclean-Rmath: clean-Rmath

get-Rmath: Rmath/Make.inc
configure-Rmath: get-Rmath
compile-Rmath: $(RMATH_OBJ_SOURCE)
check-Rmath: compile-Rmath
install-Rmath: $(RMATH_OBJ_TARGET) 


## OpenBLAS ##
# LAPACK is built into OpenBLAS by default

OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT)
OPENBLAS_OBJ_TARGET = $(BUILD)/lib/libopenblas.$(SHLIB_EXT)

OPENBLAS_BUILD_OPTS = CC="$(CC)" FC="$(FC)" RANLIB="$(RANLIB)" FFLAGS="$(FFLAGS) $(JFFLAGS)" TARGET=$(OPENBLAS_TARGET_ARCH)

# Thread support
ifeq ($(OPENBLAS_USE_THREAD), 1)
OPENBLAS_BUILD_OPTS += USE_THREAD=1
endif

# Maximum number of threads for parallelism
ifneq ($(ARCH),x86_64)
# Assume we can't address much memory to spawn many threads
# It is also unlikely that 32-bit architectures have too many cores
OPENBLAS_BUILD_OPTS += NUM_THREADS=8
else ifeq ($(OS),WINNT)
# Windows seems unable to handle very many
OPENBLAS_BUILD_OPTS += NUM_THREADS=24
else ifeq ($(OS),Darwin)
# This should suffice for the largest macs
OPENBLAS_BUILD_OPTS += NUM_THREADS=24
else
# On linux, try to provision for the largest possible machine currently
OPENBLAS_BUILD_OPTS += NUM_THREADS=128
endif

# don't touch scheduler affinity since we manage this ourselves
OPENBLAS_BUILD_OPTS += NO_AFFINITY=1

# Build for all architectures - required for distribution
ifeq ($(OPENBLAS_DYNAMIC_ARCH), 1)
OPENBLAS_BUILD_OPTS += DYNAMIC_ARCH=1
endif

# 64-bit BLAS interface
ifeq ($(USE_BLAS64), 1)
OPENBLAS_BUILD_OPTS += INTERFACE64=1
endif

# Decide whether to build for 32-bit or 64-bit arch
ifneq ($(BUILD_OS),$(OS))
OPENBLAS_BUILD_OPTS += OSNAME=$(OS) CROSS=1 HOSTCC=$(HOSTCC)
endif
ifeq ($(ARCH),i386)
OPENBLAS_BUILD_OPTS += BINARY=32
else ifeq ($(ARCH),i387)
OPENBLAS_BUILD_OPTS += BINARY=32
else ifeq ($(ARCH),i486)
OPENBLAS_BUILD_OPTS += BINARY=32
else ifeq ($(ARCH),i686)
OPENBLAS_BUILD_OPTS += BINARY=32
else ifeq ($(ARCH),x86_64)
OPENBLAS_BUILD_OPTS += BINARY=64
else
$(error "unknown arch for openblas cross-compile")
endif

ifeq ($(OS),WINNT)
ifneq ($(ARCH),x86_64)
OPENBLAS_BUILD_OPTS += CFLAGS="$(CFLAGS) -mincoming-stack-boundary=2"
OPENBLAS_BUILD_OPTS += FFLAGS="$(FFLAGS) -mincoming-stack-boundary=2"
endif
endif

# Debug OpenBLAS
ifeq ($(OPENBLAS_DEBUG), 1)
OPENBLAS_BUILD_OPTS += DEBUG=1
endif

openblas-$(OPENBLAS_VER).tar.gz: lapack-$(LAPACK_VER).tgz
	$(JLDOWNLOAD) $@ https://github.com/xianyi/OpenBLAS/tarball/$(OPENBLAS_VER) 
openblas-$(OPENBLAS_VER)/config.status: openblas-$(OPENBLAS_VER).tar.gz
	mkdir -p openblas-$(OPENBLAS_VER) && \
	tar -C openblas-$(OPENBLAS_VER) --strip-components 1 -xf $<
	perl -i -ple 's/^\s*(EXTRALIB\s*\+=\s*-lSystemStubs)\s*$$/# $$1/g' $@.system
	cp lapack-$(LAPACK_VER).tgz openblas-$(OPENBLAS_VER)
	echo 1 > $@
$(OPENBLAS_OBJ_SOURCE): openblas-$(OPENBLAS_VER)/config.status
	$(MAKE) -C openblas-$(OPENBLAS_VER) $(OPENBLAS_BUILD_OPTS) || (echo "*** Clean the OpenBLAS build with 'make -C deps clean-openblas'. Rebuild with 'make OPENBLAS_USE_THREAD=0 if OpenBLAS had trouble linking libpthread.so, and with 'make OPENBLAS_TARGET_ARCH=NEHALEM' if there were errors building SandyBridge support. Both these options can also be used simultaneously. ***" && false)
	touch -c $@
$(OPENBLAS_OBJ_TARGET): $(OPENBLAS_OBJ_SOURCE) | $(BUILD)/lib
	cp -f openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT) $(BUILD)/lib
ifeq ($(OS), Linux)
	ln -sf $(BUILD)/lib/libopenblas.$(SHLIB_EXT) $(BUILD)/lib/libopenblas.$(SHLIB_EXT).0
endif
	$(INSTALL_NAME_CMD)libopenblas.$(SHLIB_EXT) $(BUILD)/lib/libopenblas.$(SHLIB_EXT)

clean-openblas:
	-$(MAKE) -C openblas-$(OPENBLAS_VER) clean
distclean-openblas:
	-rm -rf openblas-$(OPENBLAS_VER).tar.gz openblas-$(OPENBLAS_VER)

get-openblas: openblas-$(OPENBLAS_VER).tar.gz
configure-openblas: get-openblas
compile-openblas: $(OPENBLAS_OBJ_SOURCE)
check-openblas: compile-openblas
install-openblas: $(OPENBLAS_OBJ_TARGET)


## ATLAS (currently 3.10.0) ##

# no threading, with full lapack, shared library
# should always be compiled with (a real) gcc, it's
# configure script will search for the best match
# (gcc 4.7, gcc, clang,ICC/microsoft/others)
ATLAS_OBJ_SOURCE = atlas/build/lib/libsatlas.$(SHLIB_EXT)
ATLAS_OBJ_TARGET = $(BUILD)/lib/libsatlas.$(SHLIB_EXT)
ATLAS_FLAGS = --shared --prefix=$(BUILD) --cc=gcc -t 0 \
	--with-netlib-lapack-tarfile=$(JULIAHOME)/deps/lapack-$(LAPACK_VER).tgz
ifeq ($(OS), WINNT)
ATLAS_FLAGS += -b 32 
endif

#force backwards compatibility (pick any 1)
#ATLAS_FLAGS += -V 192 -A 13  # requires SSE2 (P4 & later)
#ATLAS_FLAGS += -V 128 -A 12 # requires SSE1 (P3 & later)
#ATLAS_FLAGS += -V -1 -A 11 # any x87 (PentiumPro or Athlon & later)
#ATLAS_FLAGS += -A 25  # requires Corei132 (Corei232 doesn't have definition yet)

atlas/configure:
	git clone git://github.com/vtjnash/atlas-3.10.0.git atlas
ifeq "$(MAKECMDGOALS)" "compile-atlas"
# only allow building atlas as the sole target (without -jN)
# since it internally handles parallelism, for tuning timing accuracy
atlas/build/Make.top: atlas/configure lapack-$(LAPACK_VER).tgz
	cd atlas && \
	(test -d build || mkdir build) && cd build && \
	../configure $(ATLAS_FLAGS)
	touch -c $@
$(ATLAS_OBJ_SOURCE): atlas/build/Make.top
	$(MAKE) -C atlas/build -j1
	touch -c $@
else
$(ATLAS_OBJ_SOURCE):
	$(error cannot build atlas in parallel with anything else)
endif

$(ATLAS_OBJ_TARGET): $(ATLAS_OBJ_SOURCE)
	cp -f $(ATLAS_OBJ_SOURCE) $@
	$(INSTALL_NAME_CMD)libsatlas.$(SHLIB_EXT) $@

clean-atlas:
	rm -rf atlas/build
distclean-atlas: clean-atlas
	rm -rf atlas

get-atlas: atlas/configure
configure-atlas: atlas/build/Make.top
compile-atlas: $(ATLAS_OBJ_SOURCE)
check-atlas: compile-atlas
install-atlas: $(ATLAS_OBJ_TARGET)

## Mac gfortran BLAS wrapper ##
GFORTBLAS_FFLAGS =
ifeq ($(OS),Darwin)
ifeq ($(USE_SYSTEM_BLAS),1)
ifeq ($(USE_SYSTEM_LAPACK),0)
GFORTBLAS_FFLAGS += -cpp -ffree-line-length-0 -ffixed-line-length-0 \
			    -Dsasum=sasum_gfort -Dscasum=scasum_gfort \
				-Dscnrm2=scnrm2_gfort -Dsdot=sdot_gfort \
				-Dsdsdot=sdsdot_gfort -Dsnrm2=snrm2_gfort \
				-Dcdotc=cdotc_gfort -Dcdotu=cdotu_gfort \
				-Dzdotc=zdotc_gfort -Dzdotu=zdotu_gfort \
				\
			    -DSASUM=SASUM_GFORT -DSCASUM=SCASUM_GFORT \
				-DSCNRM2=SCNRM2_GFORT -DSDOT=SDOT_GFORT \
				-DSDSDOT=SDSDOT_GFORT -DSNRM2=SNRM2_GFORT \
				-DCDOTC=CDOTC_GFORT -DCDOTU=CDOTU_GFORT \
				-DZDOTC=ZDOTC_GFORT -DZDOTU=ZDOTU_GFORT
endif
endif

libgfortblas.dylib: gfortblas.c gfortblas.alias
	$(CC) -Wall -O3 $(CPPFLAGS) $(CFLAGS) $(fPIC) -shared $< -o $@ -pipe \
				-Wl,-reexport_framework,vecLib -Wl,-alias_list,gfortblas.alias
$(BUILD)/lib/libgfortblas.dylib: libgfortblas.dylib
	cp -f $< $@
	$(INSTALL_NAME_CMD)libgfortblas.dylib $@
endif

## LAPACK ##

ifeq ($(USE_SYSTEM_LAPACK), 0)
LAPACK_OBJ_TARGET = $(BUILD)/lib/liblapack.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE = lapack-$(LAPACK_VER)/liblapack.$(SHLIB_EXT)
else
LAPACK_OBJ_TARGET =
LAPACK_OBJ_SOURCE =
endif

LAPACK_MFLAGS = NOOPT="$(FFLAGS) $(JFFLAGS) $(GFORTBLAS_FFLAGS) -O0" OPTS="$(FFLAGS) $(JFFLAGS) $(GFORTBLAS_FFLAGS)" FORTRAN="$(FC)" LOADER="$(FC)"
ifneq ($(OS),WINNT)
LAPACK_MFLAGS += BLASLIB="-Wl,-rpath,'$(BUILD)/lib' $(LIBBLAS)"
endif

lapack-$(LAPACK_VER).tgz:
	$(JLDOWNLOAD) $@ http://www.netlib.org/lapack/$@
lapack-$(LAPACK_VER)/Makefile: lapack-$(LAPACK_VER).tgz
	tar zxf $<
	touch -c $@
ifeq ($(USE_SYSTEM_BLAS), 0)
lapack-$(LAPACK_VER)/liblapack.a: | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(OS),Darwin)
lapack-$(LAPACK_VER)/liblapack.a: | $(BUILD)/lib/libgfortblas.dylib
endif
lapack-$(LAPACK_VER)/liblapack.a: lapack-$(LAPACK_VER)/Makefile
	cd lapack-$(LAPACK_VER) && \
	cp INSTALL/make.inc.gfortran ./make.inc && \
	$(MAKE) lapacklib $(LAPACK_MFLAGS)
	touch -c $@
lapack-$(LAPACK_VER)/checked: lapack-$(LAPACK_VER)/liblapack.a
	cd lapack-$(LAPACK_VER) && \
	$(MAKE) lapack_testing $(LAPACK_MFLAGS) -k
	touch $@
$(LAPACK_OBJ_SOURCE): lapack-$(LAPACK_VER)/liblapack.a
	cd lapack-$(LAPACK_VER) && \
	$(FC) -shared $(FFLAGS) $(JFFLAGS) SRC/*.o INSTALL/dlamch.o INSTALL/dsecnd_INT_ETIME.o INSTALL/ilaver.o INSTALL/slamch.o $(LIBBLAS) -o liblapack.$(SHLIB_EXT)
$(LAPACK_OBJ_TARGET): $(LAPACK_OBJ_SOURCE)
	cp $< $@
	$(INSTALL_NAME_CMD)liblapack.dylib $@

clean-lapack:
	-$(MAKE) -C lapack-$(LAPACK_VER) clean
	-rm -f $(LAPACK_OBJ_SOURCE) $(LAPACK_OBJ_TARGET)
distclean-lapack:
	-rm -rf lapack-$(LAPACK_VER).tgz lapack-$(LAPACK_VER)

get-lapack: lapack-$(LAPACK_VER).tgz
configure-lapack: get-lapack
compile-lapack: $(LAPACK_OBJ_SOURCE)
check-lapack: lapack-$(LAPACK_VER)/checked
install-lapack: $(LAPACK_OBJ_TARGET)


## ARPACK ##
ARPACK_FFLAGS = $(GFORTBLAS_FFLAGS)

ifeq ($(USE_BLAS64), 1)
ARPACK_FFLAGS += -fdefault-integer-8
endif

ARPACK_OBJ_TARGET = $(BUILD)/lib/libarpack.$(SHLIB_EXT)
ifeq ($(OS),WINNT)
ARPACK_OBJ_SOURCE = arpack-ng-$(ARPACK_VER)/libarpack.$(SHLIB_EXT)
else
ARPACK_OBJ_SOURCE = arpack-ng-$(ARPACK_VER)/.libs/libarpack.$(SHLIB_EXT)
endif
ARPACK_MFLAGS = F77="$(FC)" MPIF77="$(FC)"
ARPACK_OPTS = $(FFLAGS) $(JFFLAGS)
ARPACK_FLAGS = --with-blas="$(LIBBLAS)" --with-lapack="$(LIBLAPACK)" --disable-mpi --enable-shared FFLAGS="$(ARPACK_FFLAGS)"
ifneq ($(OS),WINNT)
ARPACK_FLAGS += LDFLAGS="-Wl,-rpath,'$(BUILD)/lib'"
endif

arpack-ng-$(ARPACK_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://forge.scilab.org/index.php/p/arpack-ng/downloads/get/arpack-ng-$(ARPACK_VER).tar.gz
	touch -c $@
arpack-ng-$(ARPACK_VER)/configure: arpack-ng-$(ARPACK_VER).tar.gz
	tar zxf $< 
	touch -c $@

ifeq ($(USE_ATLAS), 1)
arpack-ng-$(ARPACK_VER)/config.status: | $(ATLAS_OBJ_TARGET)
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
arpack-ng-$(ARPACK_VER)/config.status: | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(USE_SYSTEM_LAPACK), 0)
arpack-ng-$(ARPACK_VER)/config.status: | $(LAPACK_OBJ_SOURCE)
endif

arpack-ng-$(ARPACK_VER)/config.status: arpack-ng-$(ARPACK_VER)/configure
ifeq ($(OS),WINNT)
	cd arpack-ng-$(ARPACK_VER) && \
	rm -f UTIL/second_NONE.f
	echo 1 > $@
else
	cd arpack-ng-$(ARPACK_VER) && \
	./configure $(CONFIGURE_COMMON) $(ARPACK_FLAGS)
endif
	touch -c $@
$(ARPACK_OBJ_SOURCE): arpack-ng-$(ARPACK_VER)/config.status
ifeq ($(OS),WINNT)
	cd arpack-ng-$(ARPACK_VER) && \
	$(FC) -shared $(ARPACK_OPTS) SRC/*.f UTIL/*.f $(LIBLAPACK) $(LIBBLAS) -o libarpack.$(SHLIB_EXT)
else
	cd arpack-ng-$(ARPACK_VER) && \
	$(MAKE) $(ARPACK_MFLAGS)
endif
	touch -c $@
arpack-ng-$(ARPACK_VER)/checked: $(ARPACK_OBJ_SOURCE)
ifneq ($(OS),WINNT)
	cd arpack-ng-$(ARPACK_VER) && \
	$(MAKE) check $(ARPACK_MFLAGS) && \
	cd TESTS && ./dnsimp
endif
	echo 1 > $@
$(ARPACK_OBJ_TARGET): $(ARPACK_OBJ_SOURCE) arpack-ng-$(ARPACK_VER)/checked | $(BUILD)/lib
ifeq ($(OS),WINNT)
	cd arpack-ng-$(ARPACK_VER) && \
	cp libarpack.$(SHLIB_EXT) $(BUILD)/lib/libarpack.$(SHLIB_EXT)
else
	cd arpack-ng-$(ARPACK_VER) && \
	$(MAKE) install $(ARPACK_MFLAGS)
	$(INSTALL_NAME_CMD)libarpack.$(SHLIB_EXT) $(BUILD)/lib/libarpack.$(SHLIB_EXT)
ifeq ($(OS), Linux)
	for filename in $(BUILD)/lib/libarpack.so* ; do \
		[ -L $$filename ] || $(PATCHELF) --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
endif
	touch -c $@

clean-arpack:
	-$(MAKE) -C arpack-ng-$(ARPACK_VER) clean
	-rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	-rm -rf arpack-ng-$(ARPACK_VER).tar.gz arpack-ng-$(ARPACK_VER)

get-arpack: arpack-ng-$(ARPACK_VER).tar.gz
configure-arpack: arpack-ng-$(ARPACK_VER)/config.status
compile-arpack: $(ARPACK_OBJ_SOURCE)
check-arpack: arpack-ng-$(ARPACK_VER)/checked
install-arpack: $(ARPACK_OBJ_TARGET)


## FFTW ##
ifeq ($(OS),WINNT)
FFTW_SINGLE_SRC_TARGET = fftw-$(FFTW_VER)-single/.libs/libfftw3f-3.$(SHLIB_EXT)
FFTW_DOUBLE_SRC_TARGET = fftw-$(FFTW_VER)-double/.libs/libfftw3-3.$(SHLIB_EXT)
else
FFTW_SINGLE_SRC_TARGET = fftw-$(FFTW_VER)-single/.libs/libfftw3f.$(SHLIB_EXT)
FFTW_DOUBLE_SRC_TARGET = fftw-$(FFTW_VER)-double/.libs/libfftw3.$(SHLIB_EXT)
endif
FFTW_SINGLE_OBJ_TARGET = $(BUILD)/lib/libfftw3f.$(SHLIB_EXT)
FFTW_DOUBLE_OBJ_TARGET = $(BUILD)/lib/libfftw3.$(SHLIB_EXT)

FFTW_CONFIG = --enable-shared --disable-fortran --disable-mpi --enable-fma --enable-threads
ifneq ($(ARCH), ppc64)
  FFTW_CONFIG += --enable-sse2
endif
ifeq ($(OS),WINNT)
FFTW_CONFIG += --with-our-malloc --with-combined-threads
ifneq ($(OS),x86_64)
FFTW_CONFIG += --with-incoming-stack-boundary=2
endif
endif

fftw-$(FFTW_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://www.fftw.org/$@

fftw-$(FFTW_VER)-single/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-single && \
	tar -C fftw-$(FFTW_VER)-single --strip-components 1 -xf $<
ifeq ($(OS),WINNT)
	patch fftw-$(FFTW_VER)-single/configure < fftw-config-nopthreads.patch
	patch fftw-$(FFTW_VER)-single/kernel/ifftw.h < ifftw.h.patch
endif
	touch -c $@
fftw-$(FFTW_VER)-single/config.status: fftw-$(FFTW_VER)-single/configure
	cd fftw-$(FFTW_VER)-single && \
	(./configure $(CONFIGURE_COMMON) $(FFTW_CONFIG) --enable-avx --enable-single || \
	  ./configure $(CONFIGURE_COMMON) $(FFTW_CONFIG) --enable-single) && \
	$(MAKE) clean
	touch -c $@
$(FFTW_SINGLE_SRC_TARGET): fftw-$(FFTW_VER)-single/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-single
fftw-$(FFTW_VER)-single/checked: $(FFTW_SINGLE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C fftw-$(FFTW_VER)-single check
endif
	echo 1 > $@
$(FFTW_SINGLE_OBJ_TARGET): $(FFTW_SINGLE_SRC_TARGET) fftw-$(FFTW_VER)-single/checked
	$(MAKE) -C fftw-$(FFTW_VER)-single install
	touch -c $@
ifeq ($(OS), Darwin)
	$(INSTALL_NAME_CMD)libfftw3f.dylib $(BUILD)/lib/libfftw3f.dylib
	$(INSTALL_NAME_CMD)libfftw3f_threads.$(SHLIB_EXT) $(BUILD)/lib/libfftw3f_threads.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) $(BUILD)/lib/libfftw3f.3.dylib $(INSTALL_NAME_ID_DIR)libfftw3f.dylib $(BUILD)/lib/libfftw3f_threads.dylib
else ifeq ($(OS),WINNT)
	-rm $(BUILD)/lib/libfftw3f.dll
	mv $(BUILD)/bin/libfftw3f-3.dll $(BUILD)/lib/libfftw3f.dll
else ifeq ($(OS), Linux)
	for filename in $(BUILD)/lib/libfftw3f_threads.so* ; do \
		[ -L $$filename ] || $(PATCHELF) --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch -c $@

fftw-$(FFTW_VER)-double/configure: fftw-$(FFTW_VER).tar.gz
	mkdir -p fftw-$(FFTW_VER)-double && \
	tar -C fftw-$(FFTW_VER)-double --strip-components 1 -xf $<
ifeq ($(OS),WINNT)
	patch fftw-$(FFTW_VER)-double/configure < fftw-config-nopthreads.patch
	patch fftw-$(FFTW_VER)-double/kernel/ifftw.h < ifftw.h.patch
endif
	touch -c $@
fftw-$(FFTW_VER)-double/config.status: fftw-$(FFTW_VER)-double/configure
	cd fftw-$(FFTW_VER)-double && \
	(./configure $(CONFIGURE_COMMON) $(FFTW_CONFIG) --enable-avx || \
	  ./configure $(CONFIGURE_COMMON) $(FFTW_CONFIG)) && \
	$(MAKE) clean
	touch -c $@
$(FFTW_DOUBLE_SRC_TARGET): fftw-$(FFTW_VER)-double/config.status
	$(MAKE) -C fftw-$(FFTW_VER)-double
	touch -c $@
fftw-$(FFTW_VER)-double/checked: $(FFTW_DOUBLE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C fftw-$(FFTW_VER)-double check
endif
	echo 1 > $@
$(FFTW_DOUBLE_OBJ_TARGET): $(FFTW_DOUBLE_SRC_TARGET) fftw-$(FFTW_VER)-double/checked
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
		[ -L $$filename ] || $(PATCHELF) --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch -c $@

clean-fftw: clean-fftw-single clean-fftw-double
clean-fftw-single:
	-$(MAKE) -C fftw-$(FFTW_VER)-single clean
	-rm -f $(FFTW_SINGLE_OBJ_TARGET)
clean-fftw-double:
	-$(MAKE) -C fftw-$(FFTW_VER)-double clean
	-rm -f $(FFTW_DOUBLE_OBJ_TARGET)
distclean-fftw:
	-rm -rf fftw-$(FFTW_VER).tar.gz fftw-$(FFTW_VER)-{single,double}

get-fftw: get-fftw-single get-fftw-double
configure-fftw: configure-fftw-single configure-fftw-double
compile-fftw: compile-fftw-single compile-fftw-double
check-fftw: check-fftw-single check-fftw-double
install-fftw: install-fftw-single install-fftw-double

get-fftw-single: fftw-$(FFTW_VER).tar.gz
configure-fftw-single: fftw-$(FFTW_VER)-single/config.status
compile-fftw-single: $(FFTW_SINGLE_OBJ_TARGET)
check-fftw-single: fftw-$(FFTW_VER)-single/checked
install-fftw-single: $(FFTW_SINGLE_OBJ_TARGET)

get-fftw-double: fftw-$(FFTW_VER).tar.gz
configure-fftw-double: fftw-$(FFTW_VER)-double/config.status
compile-fftw-double: $(FFTW_DOUBLE_OBJ_TARGET)
check-fftw-double: fftw-$(FFTW_VER)-double/checked
install-fftw-double: $(FFTW_DOUBLE_OBJ_TARGET)


## SUITESPARSE ##

SUITESPARSE_OBJ_SOURCE = SuiteSparse-$(SUITESPARSE_VER)/UMFPACK/Lib/libumfpack.a
SUITESPARSE_OBJ_TARGET = $(BUILD)/lib/libspqr.$(SHLIB_EXT)

ifeq ($(USE_BLAS64), 1)
UMFPACK_CONFIG = -DLONGBLAS='long long' 
CHOLMOD_CONFIG = -DLONGBLAS='long long' 
endif

SUITE_SPARSE_LIB = -lm
ifneq ($(OS), Darwin)
ifneq ($(OS), WINNT)
SUITE_SPARSE_LIB += -lrt
endif
endif
ifneq ($(OS), WINNT)
SUITE_SPARSE_LIB += -Wl,-rpath,'$(BUILD)/lib'
endif
SUITESPARSE_MFLAGS = CC="$(CC)" CXX="$(CXX)" F77="$(FC)" AR="$(AR)" RANLIB="$(RANLIB)" BLAS="$(LIBBLAS)" LAPACK="$(LIBLAPACK)" \
	  INSTALL_LIB="$(BUILD)/lib" INSTALL_INCLUDE="$(BUILD)/include" LIB="$(SUITE_SPARSE_LIB)" \
	  UMFPACK_CONFIG="$(UMFPACK_CONFIG)" CHOLMOD_CONFIG="$(CHOLMOD_CONFIG)"

SuiteSparse-$(SUITESPARSE_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://www.cise.ufl.edu/research/sparse/SuiteSparse/$@ 
SuiteSparse-$(SUITESPARSE_VER)/Makefile: SuiteSparse-$(SUITESPARSE_VER).tar.gz
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)
	tar -C SuiteSparse-$(SUITESPARSE_VER) --strip-components 1 -zxf $<
	touch -c $@

ifeq ($(USE_ATLAS), 1)
$(SUITESPARSE_OBJ_SOURCE): | $(ATLAS_OBJ_TARGET)
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
$(SUITESPARSE_OBJ_SOURCE): | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(USE_SYSTEM_LAPACK), 0)
$(SUITESPARSE_OBJ_SOURCE): | $(LAPACK_OBJ_SOURCE)
endif
$(SUITESPARSE_OBJ_SOURCE): SuiteSparse-$(SUITESPARSE_VER)/Makefile
	cd SuiteSparse-$(SUITESPARSE_VER) && \
	$(MAKE) library $(SUITESPARSE_MFLAGS)
	touch -c $@
SuiteSparse-$(SUITESPARSE_VER)/checked: $(SUITESPARSE_OBJ_SOURCE)
	cd SuiteSparse-$(SUITESPARSE_VER) && \
	$(MAKE) default $(SUITESPARSE_MFLAGS)
	touch $@
$(SUITESPARSE_OBJ_TARGET): $(SUITESPARSE_OBJ_SOURCE)
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)/lib && \
	cd SuiteSparse-$(SUITESPARSE_VER)/lib && \
	rm -f *.a && \
	cp -f `find .. -name libamd.a -o -name libcolamd.a -o -name libcamd.a -o -name libccolamd.a -o -name libcholmod.a -o -name libumfpack.a -o -name libsuitesparseconfig.a -o -name libspqr.a 2>/dev/null` . && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(BUILD)/lib/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libcolamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(BUILD)/lib/libcolamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcamd.a $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libcamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libcamd.$(SHLIB_EXT) $(BUILD)/lib/libcamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libccolamd.a $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libccolamd.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libccolamd.$(SHLIB_EXT) $(BUILD)/lib/libccolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libcholmod.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libcholmod.$(SHLIB_EXT) -L$(BUILD)/lib -lcolamd -lamd -lcamd -lccolamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(BUILD)/lib/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libumfpack.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libumfpack.$(SHLIB_EXT) -L$(BUILD)/lib -lcholmod -lcolamd -lamd $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(BUILD)/lib/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a libspqr.a  $(NO_WHOLE_ARCHIVE) -o $(BUILD)/lib/libspqr.$(SHLIB_EXT) -L$(BUILD)/lib -lcholmod -lcolamd -lamd $(LIBLAPACK) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(BUILD)/lib/libspqr.$(SHLIB_EXT)

clean-suitesparse:
	-$(MAKE) -C SuiteSparse-$(SUITESPARSE_VER) clean
	-rm -fr SuiteSparse-$(SUITESPARSE_VER)/lib
distclean-suitesparse: clean-suitesparse
	-rm -rf SuiteSparse-$(SUITESPARSE_VER).tar.gz SuiteSparse-$(SUITESPARSE_VER)

get-suitesparse: SuiteSparse-$(SUITESPARSE_VER).tar.gz
configure-suitesparse: get-suitesparse
compile-suitesparse: $(SUITESPARSE_OBJ_SOURCE)
check-suitesparse: SuiteSparse-$(SUITESPARSE_VER)/checked
install-suitesparse: $(SUITESPARSE_OBJ_TARGET) install-suitesparse-wrapper

# SUITESPARSE WRAPPER

ifeq ($(USE_SYSTEM_SUITESPARSE), 1)
SUITESPARSE_INC = -I /usr/include/suitesparse
SUITESPARSE_LIB = -lumfpack -lcholmod -lamd -lcamd -lcolamd -lspqr
else
SUITESPARSE_INC = -I SuiteSparse-$(SUITESPARSE_VER)/CHOLMOD/Include -I SuiteSparse-$(SUITESPARSE_VER)/SuiteSparse_config -I SuiteSparse-$(SUITESPARSE_VER)/SPQR/Include
SUITESPARSE_LIB = -L$(BUILD)/lib -lcholmod -lumfpack -lspqr $(RPATH_ORIGIN)
$(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT):  $(SUITESPARSE_OBJ_TARGET)
endif

$(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT): SuiteSparse_wrapper.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(SUITESPARSE_INC) SuiteSparse_wrapper.c -o $(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT) $(SUITESPARSE_LIB)
	$(INSTALL_NAME_CMD)libsuitesparse_wrapper.$(SHLIB_EXT) $@
	touch -c $@

clean-suitesparse-wrapper:
	-rm -f $(SUITESPARSE_OBJ_TARGET) $(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT)
distclean-suitesparse-wrapper: clean-suitesparse-wrapper

get-suitesparse-wrapper:
configure-suitesparse-wrapper:
compile-suitesparse-wrapper:
check-suitesparse-wrapper:
install-suitesparse-wrapper: $(BUILD)/lib/libsuitesparse_wrapper.$(SHLIB_EXT)


## UNWIND ##

LIBUNWIND_TARGET_OBJ = $(BUILD)/lib/libunwind.a
LIBUNWIND_TARGET_SOURCE = libunwind-$(UNWIND_VER)/src/.libs/libunwind.a
LIBUNWIND_CFLAGS = $(CFLAGS) -U_FORTIFY_SOURCE $(fPIC)

libunwind-$(UNWIND_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://download.savannah.gnu.org/releases/libunwind/$@ 
libunwind-$(UNWIND_VER)/configure: libunwind-$(UNWIND_VER).tar.gz
	tar xfz $<
	touch -c $@
libunwind-$(UNWIND_VER)/config.status: libunwind-$(UNWIND_VER)/configure
	cd libunwind-$(UNWIND_VER) && \
	./configure $(CONFIGURE_COMMON) CFLAGS="$(LIBUNWIND_CFLAGS)" --disable-shared --disable-minidebuginfo
	touch -c $@
$(LIBUNWIND_TARGET_SOURCE): libunwind-$(UNWIND_VER)/config.status
	$(MAKE) -C libunwind-$(UNWIND_VER)
	touch -c $@
libunwind-$(UNWIND_VER)/checked: $(LIBUNWIND_TARGET_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C libunwind-$(UNWIND_VER) check
endif
	echo 1 > $@
#todo: libunwind tests known to fail
$(LIBUNWIND_TARGET_OBJ): $(LIBUNWIND_TARGET_SOURCE)
	$(MAKE) install -C libunwind-$(UNWIND_VER)
ifeq ($(ARCH), ppc64)
	# workaround for configure script bug
	mv $(BUILD)/lib64/libunwind*.a $(BUILD)/lib/
endif

clean-unwind:
	-$(MAKE) -C libunwind-$(UNWIND_VER) clean
	-rm -f $(LIBUNWIND_TARGET_OBJ) $(LIBUNWIND_TARGET_SOURCE)
distclean-unwind:
	-rm -rf libunwind-$(UNWIND_VER).tar.gz libunwind-$(UNWIND_VER)

get-unwind: libunwind-$(UNWIND_VER).tar.gz
configure-unwind: libunwind-$(UNWIND_VER)/config.status
compile-unwind: $(LIBUNWIND_TARGET_SOURCE)
check-unwind: libunwind-$(UNWIND_VER)/checked
install-unwind: $(LIBUNWIND_TARGET_OBJ)


## GMP ##

GMP_SRC_TARGET = gmp-$(GMP_VER)/.libs/libgmp.$(SHLIB_EXT)
GMP_OBJ_TARGET = $(BUILD)/lib/libgmp.$(SHLIB_EXT)

gmp-$(GMP_VER).tar.bz2:
	$(JLDOWNLOAD) $@ ftp://ftp.gmplib.org/pub/gmp-$(GMP_VER)/$@
gmp-$(GMP_VER)/configure: gmp-$(GMP_VER).tar.bz2
	tar jxf $<
	touch -c $@
gmp-$(GMP_VER)/config.status: gmp-$(GMP_VER)/configure
	cd gmp-$(GMP_VER) && \
	./configure $(CONFIGURE_COMMON) F77= --enable-shared --disable-static
	touch -c $@
$(GMP_SRC_TARGET): gmp-$(GMP_VER)/config.status
	$(MAKE) -C gmp-$(GMP_VER) $(LIBTOOL_CCLD)
gmp-$(GMP_VER)/checked: $(GMP_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C gmp-$(GMP_VER) $(LIBTOOL_CCLD) check
endif
	echo 1 > $@
$(GMP_OBJ_TARGET): $(GMP_SRC_TARGET) gmp-$(GMP_VER)/checked
	$(MAKE) -C gmp-$(GMP_VER) $(LIBTOOL_CCLD) install
	$(INSTALL_NAME_CMD)libgmp.dylib $@
	touch -c $@

clean-gmp:
	-$(MAKE) -C gmp-$(GMP_VER) clean
	-rm -f $(GMP_OBJ_TARGET)
distclean-gmp:
	-rm -rf gmp-$(GMP_VER).tar.bz2 gmp-$(GMP_VER)

get-gmp: gmp-$(GMP_VER).tar.bz2
configure-gmp: gmp-$(GMP_VER)/config.status
compile-gmp: $(GMP_SRC_TARGET)
check-gmp: gmp-$(GMP_VER)/checked
install-gmp: $(GMP_OBJ_TARGET)

ifeq ($(USE_SYSTEM_GMP), 0)
MPFR_DEPS = $(GMP_OBJ_TARGET)
endif

## MPFR ##

MPFR_SRC_TARGET = mpfr-$(MPFR_VER)/src/.libs/libmpfr.$(SHLIB_EXT)
MPFR_OBJ_TARGET = $(BUILD)/lib/libmpfr.$(SHLIB_EXT)
ifeq ($(OS),Darwin)
MPFR_CHECK_MFLAGS = LDFLAGS="-Wl,-rpath,'$(BUILD)/$(JL_LIBDIR)'"
endif

mpfr-$(MPFR_VER).tar.bz2:
	$(JLDOWNLOAD) $@ http://www.mpfr.org/mpfr-current/$@
mpfr-$(MPFR_VER)/configure: mpfr-$(MPFR_VER).tar.bz2
	tar jxf $<
	touch -c $@
mpfr-$(MPFR_VER)/config.status: mpfr-$(MPFR_VER)/configure $(MPFR_DEPS)
	cd mpfr-$(MPFR_VER) && \
	./configure $(CONFIGURE_COMMON) $(MPFR_OPTS) F77= --enable-shared --disable-static
	touch -c $@
$(MPFR_SRC_TARGET): mpfr-$(MPFR_VER)/config.status
	$(MAKE) -C mpfr-$(MPFR_VER) $(LIBTOOL_CCLD)
mpfr-$(MPFR_VER)/checked: $(MPFR_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C mpfr-$(MPFR_VER) $(LIBTOOL_CCLD) check $(MPFR_CHECK_MFLAGS)
endif
	echo 1 > $@
$(MPFR_OBJ_TARGET): $(MPFR_SRC_TARGET) mpfr-$(MPFR_VER)/checked
	$(MAKE) -C mpfr-$(MPFR_VER) $(LIBTOOL_CCLD) install
	$(INSTALL_NAME_CMD)libmpfr.dylib $@
	$(AUTOTOOLS_PATH_CORRECT) 
	touch -c $@

clean-mpfr:
	-$(MAKE) -C mpfr-$(MPFR_VER) clean
	-rm -f $(MPFR_OBJ_TARGET)
distclean-mpfr:
	-rm -rf mpfr-$(MPFR_VER).tar.bz2 mpfr-$(MPFR_VER)

get-mpfr: mpfr-$(MPFR_VER).tar.bz2
configure-mpfr: mpfr-$(MPFR_VER)/config.status
compile-mpfr: $(MPFR_SRC_TARGET)
check-mpfr: mpfr-$(MPFR_VER)/checked
install-mpfr: $(MPFR_OBJ_TARGET)

## ZLIB ##

ifeq ($(OS),WINNT)
ZLIB_SRC_TARGET = zlib-$(ZLIB_VER)/zlib1.$(SHLIB_EXT)
else
ZLIB_SRC_TARGET = zlib-$(ZLIB_VER)/libz.$(SHLIB_EXT)
endif
ZLIB_OBJ_TARGET = $(BUILD)/lib/libz.$(SHLIB_EXT)
ZLIB_CONFIGFLAGS = CFLAGS="-O3 $(CFLAGS) -D_FILE_OFFSET_BITS=64"
ifneq ($(OS),$(BUILD_OS))
ZLIB_CONFIGFLAGS += CHOST=$(XC_HOST)
else
ZLIB_CONFIGFLAGS += CC="$(CC)"
endif

zlib-$(ZLIB_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://zlib.net/$@
zlib-$(ZLIB_VER)/configure: zlib-$(ZLIB_VER).tar.gz
	tar zxf $<
	touch -c $@
zlib-$(ZLIB_VER)/config.status: zlib-$(ZLIB_VER)/configure
ifeq ($(OS), WINNT)
	cd zlib-$(ZLIB_VER) && \
	cp win32/Makefile.gcc Makefile
else
	cd zlib-$(ZLIB_VER) && \
	$(ZLIB_CONFIGFLAGS) ./configure --prefix=$(abspath $(BUILD)) #not an autoconf script
endif
	echo 1 > $@
$(ZLIB_SRC_TARGET): zlib-$(ZLIB_VER)/config.status
	$(MAKE) -C zlib-$(ZLIB_VER) PREFIX=$(CROSS_COMPILE)
	touch -c $@
zlib-$(ZLIB_VER)/checked: $(ZLIB_SRC_TARGET)
ifneq ($(OS), WINNT)
	$(MAKE) -C zlib-$(ZLIB_VER) check PREFIX=$(CROSS_COMPILE)
endif
	echo 1 > $@
$(ZLIB_OBJ_TARGET): $(ZLIB_SRC_TARGET) zlib-$(ZLIB_VER)/checked
ifeq ($(OS), WINNT)
	cp zlib-$(ZLIB_VER)/zlib1.dll $@
else
	$(MAKE) -C zlib-$(ZLIB_VER) install PREFIX=$(CROSS_COMPILE)
endif
	$(INSTALL_NAME_CMD)libz.dylib $@
	touch -c $@

clean-zlib:
	-$(MAKE) -j$(PARALLEL_BUILD_JOBS) -C zlib-$(ZLIB_VER) clean
	-rm -f $(ZLIB_OBJ_TARGET)
distclean-zlib:
	-rm -rf zlib-$(ZLIB_VER).tar.gz zlib-$(ZLIB_VER)

get-zlib: zlib-$(ZLIB_VER).tar.gz
configure-zlib: zlib-$(ZLIB_VER)/config.status
compile-zlib: $(ZLIB_SRC_TARGET)
check-zlib: zlib-$(ZLIB_VER)/checked
install-zlib: $(ZLIB_OBJ_TARGET)


## patchelf ##

PATCHELF_SOURCE = patchelf-$(PATCHELF_VER)/src/patchelf
PATCHELF_TARGET = $(BUILD)/bin/patchelf

compile-patchelf: install-patchelf
install-patchelf: $(PATCHELF_TARGET)

patchelf-$(PATCHELF_VER).tar.bz2:
	$(JLDOWNLOAD) $@ http://hydra.nixos.org/build/1524660/download/2/$@
patchelf-$(PATCHELF_VER)/configure: patchelf-$(PATCHELF_VER).tar.bz2
	tar jxf $<
	touch -c $@
patchelf-$(PATCHELF_VER)/config.status: patchelf-$(PATCHELF_VER)/configure
	cd patchelf-$(PATCHELF_VER) && \
	./configure $(CONFIGURE_COMMON)
	touch -c $@
$(PATCHELF_SOURCE): patchelf-$(PATCHELF_VER)/config.status
	$(MAKE) -C patchelf-$(PATCHELF_VER)
	touch -c $@
patchelf-$(PATCHELF_VER)/checked: $(PATCHELF_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	# disabled due to bug in v0.6
	#$(MAKE) -C patchelf-$(PATCHELF_VER) check
endif
	echo 1 > $@
$(PATCHELF_TARGET): $(PATCHELF_SOURCE) patchelf-$(PATCHELF_VER)/checked
	$(MAKE) -C patchelf-$(PATCHELF_VER) install
	touch -c $@

clean-patchelf:
	-$(MAKE) -C patchelf-$(PATCHELF_VER) clean
	-rm -f $(PATCHELF_OBJ_TARGET)
distclean-patchelf:
	-rm -rf patchelf-$(PATCHELF_VER).tar.bz2 patchelf-$(PATCHELF_VER)

get-patchelf: patchelf-$(PATCHELF_VER).tar.bz2
configure-patchelf: patchelf-$(PATCHELF_VER)/config.status
compile-patchelf: $(PATCHELF_SOURCE)
check-patchelf: patchelf-$(PATCHELF_VER)/checked
install-patchelf: $(PATCHELF_TARGET)

## Git

GIT_SOURCE = git-$(GIT_VER)/src/git
GIT_TARGET = $(BUILD)/git

git-$(GIT_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://git-core.googlecode.com/files/$@
git-$(GIT_VER)/configure: git-$(GIT_VER).tar.gz
	tar zxf $<
	touch -c $@
git-$(GIT_VER)/config.status: git-$(GIT_VER)/configure
	cd git-$(GIT_VER) && \
	./configure $(CONFIGURE_COMMON)
	touch -c $@
$(GIT_SOURCE): git-$(GIT_VER)/config.status
	$(MAKE) -C git-$(GIT_VER)
	touch -c $@
git-$(GIT_VER)/checked: $(GIT_SOURCE)
	echo 1 > $@
$(GIT_TARGET): $(GIT_SOURCE) git-$(GIT_VER)/checked
	$(MAKE) -C git-$(GIT_VER) install NO_INSTALL_HARDLINKS=1
	touch -c $@

clean-git:
	-$(MAKE) -C git-$(GIT_VER) clean
	-rm -f $(GIT_OBJ_TARGET)
distclean-git:
	-rm -rf git-$(GIT_VER).tar.gz git-$(GIT_VER)

get-git: git-$(GIT_VER).tar.gz
configure-git: git-$(GIT_VER)/config.status
compile-git: $(GIT_SOURCE)
check-git: git-$(GIT_VER)/checked
install-git: $(GIT_TARGET)

## phony targets ##

.PHONY: default compile install cleanall distclean \
	get-* configure-* compile-* check-* install-* \
	clean-* distclean-*
