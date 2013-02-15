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

#autoconf configure-driven scripts: llvm readline pcre arpack fftw unwind gmp patchelf
#custom configure-driven script: zlib nginx
#custom Makefile rules: openlibm Rmath double-conversion random suitesparse-wrapper suitesparse lapack openblas uv tk-wrapper

# prevent installing libs into usr/lib64 on opensuse
unexport CONFIG_SITE

STAGE1_DEPS = uv openlibm-extras random Rmath double-conversion
STAGE2_DEPS =
STAGE3_DEPS = suitesparse-wrapper

ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
ifeq ($(OS), Linux)
STAGE1_DEPS += unwind
else ifeq ($(OS), FreeBSD)
STAGE1_DEPS += unwind
endif
endif

ifeq ($(OS), Linux)
STAGE1_DEPS += patchelf
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

ifeq ($(USE_SYSTEM_SUITESPARSE), 0)
STAGE2_DEPS += suitesparse
endif

#ifeq ($(USE_SYSTEM_LAPACK), 0)
#STAGE2_DEPS += lapack
#endif

#Platform specific flags

ifeq ($(OS), WINNT)
LIBTOOL_CCLD = CCLD="$(CC) -no-undefined -avoid-version"
AUTOTOOLS_PATH_CORRECT = -cp -f $(BUILD)/bin/$(@F) $(BUILD)/lib/$(@F)
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
getall: get-llvm get-readline get-uv get-pcre get-double-conversion get-openlibm get-random get-openblas get-fftw get-suitesparse get-clp get-unwind get-nginx get-gmp get-zlib get-patchelf

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

ifeq ($(OS),WINNT)
LLVM_LIB_FILE = LLVM-$(LLVM_VER)$(LLVM_LIB_SUFFIX).$(SHLIB_EXT)
LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/$(LLVM_BUILDDIR)/bin/$(LLVM_LIB_FILE)
else
LLVM_LIB_FILE = libLLVM-$(LLVM_VER)$(LLVM_LIB_SUFFIX).$(SHLIB_EXT)
LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/$(LLVM_BUILDDIR)/lib/$(LLVM_LIB_FILE)
endif
LLVM_OBJ_TARGET = $(BUILD)/lib/$(LLVM_LIB_FILE)


ifeq ($(LLVM_VER), 3.0)
LLVM_TAR=llvm-$(LLVM_VER).tar.gz
else
LLVM_TAR=llvm-$(LLVM_VER).src.tar.gz
endif

ifeq ($(BUILD_LLVM_CLANG),1)
ifeq ($(LLVM_VER), 3.0)
LLVM_CLANG_TAR=clang-$(LLVM_VER).tar.gz
LLVM_COMPILER_RT_TAR=
else
LLVM_CLANG_TAR=clang-$(LLVM_VER).src.tar.gz
LLVM_COMPILER_RT_TAR=compiler-rt-$(LLVM_VER).src.tar.gz
endif
else
LLVM_CLANG_TAR=
LLVM_COMPILER_RT_TAR=
endif

LLVM_FLAGS =  --disable-threads --disable-profiling --enable-shared --enable-targets=host --disable-bindings --disable-docs
LLVM_MFLAGS =
ifeq ($(LLVM_ASSERTIONS), 1)
LLVM_FLAGS += --enable-assertions --enable-optimized
ifeq ($(OS), WINNT)
LLVM_FLAGS += --disable-embed-stdcxx
endif
else
LLVM_FLAGS += --disable-assertions --enable-optimized
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
	$(WGET) http://llvm.org/releases/$(LLVM_VER)/$@
endif
ifneq ($(LLVM_COMPILER_RT_TAR),)
$(LLVM_COMPILER_RT_TAR):
	$(WGET) http://llvm.org/releases/$(LLVM_VER)/$@
endif
$(LLVM_TAR):
	$(WGET) http://llvm.org/releases/$(LLVM_VER)/$@

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
		git fetch))
	cd llvm-$(LLVM_VER)/tools/lldb && git checkout origin/release_32
endif
	touch -c $@

llvm-$(LLVM_VER)/build/config.status: llvm-$(LLVM_VER)/configure | $(llvm_python_workaround)
	cd llvm-$(LLVM_VER) && \
	mkdir -p build && cd build && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	../configure $(CONFIGURE_COMMON) $(LLVM_FLAGS)
	touch -c $@
$(LLVM_OBJ_SOURCE): llvm-$(LLVM_VER)/build/config.status | $(llvm_python_workaround)
	cd llvm-$(LLVM_VER)/build && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) $(LLVM_MFLAGS)
	touch -c $@
llvm-$(LLVM_VER)/build/checked: $(LLVM_OBJ_SOURCE) | $(llvm_python_workaround)
ifeq ($(OS),$(BUILD_OS))
	cd llvm-$(LLVM_VER)/build && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) $(LLVM_MFLAGS) check 
endif
	echo 1 > $@
$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE) | $(llvm_python_workaround)
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) -C llvm-$(LLVM_VER)/build install
	$(INSTALL_NAME_CMD)$(LLVM_LIB_FILE) $(BUILD)/lib/$(LLVM_LIB_FILE)
	$(AUTOTOOLS_PATH_CORRECT) 
	touch -c $@

clean-llvm:
	-$(MAKE) -C llvm-$(LLVM_VER)/build clean
	-rm -f $(BUILD)/bin/llvm-config
distclean-llvm:
	-rm -rf llvm-$(LLVM_VER).tar.gz llvm-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).tar.gz compiler-rt-$(LLVM_VER).src.tar.gz llvm-$(LLVM_VER)

get-llvm: $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR)
configure-llvm: llvm-$(LLVM_VER)/build/config.status
compile-llvm: $(LLVM_OBJ_SOURCE)
check-llvm: llvm-$(LLVM_VER)/build/checked
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
	$(WGET_DASH_O) $@ $(READLINE_URL)
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
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-001
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-002
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-003
	$(WGET) http://ftp.gnu.org/gnu/readline/readline-6.2-patches/readline62-004
	$(WGET) http://ftp.gnu.org/gnu/readline/$@
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
		$(BUILD)/bin/patchelf --set-rpath '$$ORIGIN' $$filename ;\
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
LIBUV_MAKE_FLAGS = -C libuv CC="$(CC)" AR="$(AR)" uname_S=$(uname_S)
$(UV_OBJ_SOURCE): libuv/Makefile
ifneq ($(OS), WINNT)
	$(MAKE) libuv.${SHLIB_EXT} $(LIBUV_MAKE_FLAGS)
endif
	$(MAKE) libuv.a $(LIBUV_MAKE_FLAGS)
	touch -c $@
libuv/checked: $(LIBUV_OBJ_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) test $(LIBUV_MAKE_FLAGS)
endif
	echo 1 > $@
#todo: libuv tests are really slow
$(UV_OBJ_TARGET): $(UV_OBJ_SOURCE)
	mkdir -p $(BUILD)/include
	cp $(UV_OBJ_SOURCE) $(BUILD)/lib/
	cp -r libuv/include/* $(BUILD)/include

clean-uv:
	-$(MAKE) -C libuv clean uname_S=$(uname_S)
	-rm -rf $(BUILD)/lib/libuv.a $(BUILD)/include/uv.h $(BUILD)/include/uv-private
distclean-uv: clean-uv

get-uv: libuv/Makefile
configure-uv: get-uv
compile-uv: $(UV_OBJ_SOURCE)
check-uv: libuv/checked
install-uv: $(UV_OBJ_TARGET)


## PCRE ##

PCRE_SRC_TARGET = pcre-$(PCRE_VER)/.libs/libpcre.$(SHLIB_EXT)
PCRE_OBJ_TARGET = $(BUILD)/lib/libpcre.$(SHLIB_EXT)

pcre-$(PCRE_VER).tar.bz2:
	$(WGET) ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/$@
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

double-conversion-$(GRISU_VER).tar.gz:
	$(WGET) http://double-conversion.googlecode.com/files/double-conversion-$(GRISU_VER).tar.gz
	touch -c $@
double-conversion-$(GRISU_VER)/Makefile: double-conversion-$(GRISU_VER).tar.gz
	mkdir -p double-conversion-$(GRISU_VER) && \
	tar -C double-conversion-$(GRISU_VER) --strip-components 1 -xf double-conversion-$(GRISU_VER).tar.gz
	touch -c $@
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

get-double-conversion: double-conversion-$(GRISU_VER).tar.gz
configure-double-conversion: get-double-conversion
compile-double-conversion: double-conversion-$(GRISU_VER)/src/libgrisu.$(SHLIB_EXT)
check-double-conversion: compile-double-conversion
install-double-conversion: $(BUILD)/lib/libgrisu.$(SHLIB_EXT)


## openlibm && openlibm-extras ##

ifeq ($(OS), WINNT) #needs more advanced detection once 64bit build is possible
OPENLIBM_FLAGS = ARCH=i386
else
OPENLIBM_FLAGS = ARCH="$(ARCH)"
endif

OPENLIBM_FLAGS += CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC)

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
ifeq (exists, $(shell [ -d $(JULIAHOME)/modules ] && echo exists ))
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
	cp $< $@
	$(INSTALL_NAME_CMD)libopenlibm.$(SHLIB_EXT) $@
$(OPENLIBMEXT_OBJ_TARGET): $(OPENLIBMEXT_OBJ_SOURCE) | $(BUILD)/lib
	cp $< $@
	$(INSTALL_NAME_CMD)libopenlibm-extras.$(SHLIB_EXT) $@

clean-openlibm:
	-$(MAKE) -C openlibm distclean
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
	$(WGET_DASH_O) $@ http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz 
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
ifeq (exists, $(shell [ -d $(JULIAHOME)/modules ] && echo exists ))
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

OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT)
OPENBLAS_OBJ_TARGET = $(BUILD)/lib/libopenblas.$(SHLIB_EXT)

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

openblas-$(OPENBLAS_VER).tar.gz:
	$(WGET_DASH_O) $@ https://github.com/xianyi/OpenBLAS/tarball/$(OPENBLAS_VER) 
openblas-$(OPENBLAS_VER)/config.status: openblas-$(OPENBLAS_VER).tar.gz
	mkdir -p openblas-$(OPENBLAS_VER) && \
	tar -C openblas-$(OPENBLAS_VER) --strip-components 1 -xf $<
	perl -i -ple 's/^\s*(EXTRALIB\s*\+=\s*-lSystemStubs)\s*$$/# $$1/g' $@.system
	echo 1 > $@
$(OPENBLAS_OBJ_SOURCE): openblas-$(OPENBLAS_VER)/config.status
	$(MAKE) -C openblas-$(OPENBLAS_VER) $(OPENBLAS_BUILD_OPTS)
	touch -c $@
$(OPENBLAS_OBJ_TARGET): $(OPENBLAS_OBJ_SOURCE) | $(BUILD)/lib
	cp -f openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT) $(BUILD)/lib
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
ifeq "$(MAKECMDGOALS)" "build-atlas"
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
$(BUILD)/lib/libsatlas.$(SHLIB_EXT): $(ATLAS_OBJ_SOURCE)
	cp -f atlas/build/lib/libsatlas.$(SHLIB_EXT) $(BUILD)/lib
	$(INSTALL_NAME_CMD)libsatlas.$(SHLIB_EXT) $(BUILD)/lib/libsatlas.$(SHLIB_EXT)

clean-atlas:
	rm -rf atlas/build
distclean-atlas: clean-atlas
	rm -rf atlas

get-atlas: atlas/configure
configure-atlas: atlas/build/Make.top
compile-atlas: $(ATLAS_OBJ_SOURCE)
check-atlas: compile-atlas
install-atlas: $(BUILD)/lib/libsatlas.$(SHLIB_EXT)


## LAPACK ##

ifeq ($(USE_SYSTEM_LAPACK), 0)
LAPACK_OBJ_TARGET = $(BUILD)/lib/liblapack.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE = lapack-$(LAPACK_VER)/liblapack.$(SHLIB_EXT)
else
LAPACK_OBJ_TARGET =
LAPACK_OBJ_SOURCE =
endif

lapack-$(LAPACK_VER).tgz:
	$(WGET) http://www.netlib.org/lapack/$@
lapack-$(LAPACK_VER)/Makefile: lapack-$(LAPACK_VER).tgz
	tar zxf $<
	touch -c $@
ifeq ($(USE_SYSTEM_BLAS), 0)
lapack-$(LAPACK_VER)/INSTALL/dlamch.o: $(OPENBLAS_OBJ_TARGET)
endif
lapack-$(LAPACK_VER)/INSTALL/dlamch.o: lapack-$(LAPACK_VER)/Makefile
	cd lapack-$(LAPACK_VER) && \
	cp INSTALL/make.inc.gfortran ./make.inc && \
	$(MAKE) lapacklib NOOPT="-O0 $(fPIC)" OPTS="$(FFLAGS) $(JFFLAGS)" FORTRAN="$(FC)" LOADER="$(FC)"
	touch -c $@
ifeq ($(USE_SYSTEM_BLAS), 0)
$(LAPACK_OBJ_SOURCE): $(OPENBLAS_OBJ_TARGET)
endif
$(LAPACK_OBJ_SOURCE): lapack-$(LAPACK_VER)/INSTALL/dlamch.o
	cd lapack-$(LAPACK_VER) && \
	$(FC) -shared $(FFLAGS) $(JFFLAGS) SRC/*.o INSTALL/dlamch.o INSTALL/dsecnd_INT_ETIME.o INSTALL/ilaver.o INSTALL/slamch.o $(LIBBLAS) -o liblapack.$(SHLIB_EXT)
$(LAPACK_OBJ_TARGET): $(LAPACK_OBJ_SOURCE)
	cp $< $@

clean-lapack:
	-$(MAKE) -C lapack-$(LAPACK_VER) clean
	-rm -f $(LAPACK_OBJ_SOURCE) $(LAPACK_OBJ_TARGET)
distclean-lapack:
	-rm -rf lapack-$(LAPACK_VER).tgz lapack-$(LAPACK_VER)

get-lapack: lapack-$(LAPACK_VER).tgz
configure-lapack: get-lapack
compile-lapack: $(LAPACK_OBJ_SOURCE)
check-lapack: compile-lapack
install-lapack: $(LAPACK_OBJ_TARGET)


## ARPACK ##

ifeq ($(USE_LIB64), 1)
ARPACK_FFLAGS = -fdefault-integer-8
endif

ARPACK_OBJ_TARGET = $(BUILD)/lib/libarpack.$(SHLIB_EXT)
ifeq ($(OS),WINNT)
ARPACK_OBJ_SOURCE = arpack-ng_$(ARPACK_VER)/libarpack.$(SHLIB_EXT)
else
ARPACK_OBJ_SOURCE = arpack-ng_$(ARPACK_VER)/.libs/libarpack.$(SHLIB_EXT)
endif
ARPACK_ARGS = F77="$(FC)" MPIF77="$(FC)"
ARPACK_OPTS = $(FFLAGS) $(JFFLAGS)

arpack-ng_$(ARPACK_VER).tar.gz:
	$(WGET_DASH_O) $@ http://forge.scilab.org/index.php/p/arpack-ng/downloads/get/arpack-ng_$(ARPACK_VER).tar.gz
	touch -c $@
arpack-ng_$(ARPACK_VER)/configure: arpack-ng_$(ARPACK_VER).tar.gz
	tar zxf $< 
	touch -c $@
ifeq ($(USE_SYSTEM_BLAS), 0)
arpack-ng_$(ARPACK_VER)/config.status: $(OPENBLAS_OBJ_TARGET)
endif
arpack-ng_$(ARPACK_VER)/config.status: arpack-ng_$(ARPACK_VER)/configure
ifeq ($(OS),WINNT)
	cd arpack-ng_$(ARPACK_VER) && \
	rm -f UTIL/second_NONE.f
	echo 1 > $@
else
	cd arpack-ng_$(ARPACK_VER) && \
	./configure $(CONFIGURE_COMMON) --with-blas="$(LIBBLAS)" --with-lapack="$(LIBLAPACK)" --disable-mpi --enable-shared FFLAGS="$(ARPACK_FFLAGS)"
endif
	touch -c $@
$(ARPACK_OBJ_SOURCE): arpack-ng_$(ARPACK_VER)/config.status
ifeq ($(OS),WINNT)
	cd arpack-ng_$(ARPACK_VER) && \
	$(FC) -shared $(ARPACK_OPTS) SRC/*.f UTIL/*.f $(LIBLAPACK) $(LIBBLAS) -o libarpack.$(SHLIB_EXT)
else
	cd arpack-ng_$(ARPACK_VER) && \
	$(MAKE) $(ARPACK_ARGS)
endif
	touch -c $@
arpack-ng_$(ARPACK_VER)/checked: $(ARPACK_OBJ_SOURCE)
ifneq ($(OS),WINNT)
	cd arpack-ng_$(ARPACK_VER) && \
	$(MAKE) check $(ARPACK_ARGS)
endif
	echo 1 > $@
$(ARPACK_OBJ_TARGET): $(ARPACK_OBJ_SOURCE) arpack-ng_$(ARPACK_VER)/checked | $(BUILD)/lib
ifeq ($(OS),WINNT)
	cd arpack-ng_$(ARPACK_VER) && \
	cp libarpack.$(SHLIB_EXT) $(BUILD)/lib/libarpack.$(SHLIB_EXT)
else
	cd arpack-ng_$(ARPACK_VER) && \
	$(MAKE) install $(ARPACK_ARGS)
	$(INSTALL_NAME_CMD)libarpack.$(SHLIB_EXT) $(BUILD)/lib/libarpack.$(SHLIB_EXT)
ifeq ($(OS), Linux)
	for filename in $(BUILD)/lib/libarpack.so* ; do \
		[ -L $$filename ] || $(BUILD)/bin/patchelf --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
endif
	touch -c $@

clean-arpack:
	-$(MAKE) -C arpack-ng_$(ARPACK_VER) clean
	-rm -f $(ARPACK_OBJ_SOURCE) $(ARPACK_OBJ_TARGET)
distclean-arpack:
	-rm -rf arpack-ng_$(ARPACK_VER).tar.gz arpack-ng_$(ARPACK_VER)

get-arpack: arpack-ng_$(ARPACK_VER).tar.gz
configure-arpack: arpack-ng_$(ARPACK_VER)/config.status
compile-arpack: $(ARPACK_OBJ_SOURCE)
check-arpack: arpack-ng_$(ARPACK_VER)/checked
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
endif

fftw-$(FFTW_VER).tar.gz:
	$(WGET) http://www.fftw.org/$@

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
		[ -L $$filename ] || $(BUILD)/bin/patchelf --set-rpath '$$ORIGIN' $$filename ;\
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
		[ -L $$filename ] || $(BUILD)/bin/patchelf --set-rpath '$$ORIGIN' $$filename ;\
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
SUITESPARSE_OBJ_TARGET = $(BUILD)/lib/libumfpack.$(SHLIB_EXT)

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
ifneq ($(OS), WINNT)
SUITE_SPARSE_LIB += -Wl,-rpath,'$(BUILD)/lib'
endif

SuiteSparse-$(SUITESPARSE_VER).tar.gz:
	$(WGET) http://www.cise.ufl.edu/research/sparse/SuiteSparse/SuiteSparse-$(SUITESPARSE_VER).tar.gz
SuiteSparse-$(SUITESPARSE_VER)/Makefile: SuiteSparse-$(SUITESPARSE_VER).tar.gz
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)
	tar -C SuiteSparse-$(SUITESPARSE_VER) --strip-components 1 -zxf $<
	touch -c $@
ifeq ($(USE_SYSTEM_BLAS), 0)
$(SUITESPARSE_OBJ_SOURCE): $(OPENBLAS_OBJ_TARGET)
endif
#todo: rpath is missing on mac for tests
$(SUITESPARSE_OBJ_SOURCE): SuiteSparse-$(SUITESPARSE_VER)/Makefile
	cd SuiteSparse-$(SUITESPARSE_VER) && \
	$(MAKE) CC="$(CC)" CXX="$(CXX)" F77="$(FC)" AR="$(AR)" RANLIB="$(RANLIB)" BLAS="$(LIBBLAS)" LAPACK="$(LIBLAPACK)" \
	  INSTALL_LIB="$(BUILD)/lib" INSTALL_INCLUDE="$(BUILD)/include" LIB="$(SUITE_SPARSE_LIB)" \
	  UMFPACK_CONFIG="$(UMFPACK_CONFIG)" CHOLMOD_CONFIG="$(CHOLMOD_CONFIG)"
	touch -c $@
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

clean-suitesparse:
	-$(MAKE) -C SuiteSparse-$(SUITESPARSE_VER) clean
	-rm -fr SuiteSparse-$(SUITESPARSE_VER)/lib
distclean-suitesparse: clean-suitesparse
	-rm -rf SuiteSparse-$(SUITESPARSE_VER).tar.gz SuiteSparse-$(SUITESPARSE_VER)

get-suitesparse: SuiteSparse-$(SUITESPARSE_VER).tar.gz
configure-suitesparse: get-suitesparse
compile-suitesparse: $(SUITESPARSE_OBJ_SOURCE)
check-suitesparse: compile-suitesparse
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
	$(WGET) http://savannah.spinellicreations.com/libunwind/libunwind-$(UNWIND_VER).tar.gz
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


## NGINX ##

ifneq ($(OS), WINNT)
NGINX_OBJ_TARGET = $(BUILD)/sbin/nginx
NGINX_SRC_TARGET = nginx/objs/nginx
NGINX_FLAGS = --with-ld-opt="-L'$(BUILD)/lib' -lpcre -Wl,-rpath,'$(BUILD)/lib' $(RPATH)"

nginx/.gitignore:
	rm -f $@
	for x in .gitignore Makefile config.status objs; do echo /$$x >> $@; done
nginx/auto/configure: | compile-pcre
	(cd .. && git submodule update --init)
	touch -c $@
nginx/config.status: nginx/auto/configure nginx/.gitignore
	cd nginx && \
	./auto/configure --without-http_gzip_module --without-http_rewrite_module $(NGINX_FLAGS) --with-debug --prefix=".."
	echo 1 > $@
$(NGINX_SRC_TARGET): nginx/config.status nginx/.gitignore
	$(MAKE) -C nginx
	touch -c $@
$(NGINX_OBJ_TARGET): $(NGINX_SRC_TARGET) nginx/.gitignore
	$(MAKE) -C nginx install DESTDIR="$(abspath $(BUILD)/bin)/"
	touch -c $@

clean-nginx:
	-$(MAKE) -C nginx clean
distclean-nginx:
	-rm -rf nginx

get-nginx: nginx/auto/configure
configure-nginx: nginx/config.status
compile-nginx: $(NGINX_SRC_TARGET)
check-nginx: $(NGINX_SRC_TARGET)
install-nginx: $(NGINX_OBJ_TARGET)
else
NGINX_VER=1.2.6
NGINX_SRC_TARGET = nginx-$(NGINX_VER).zip
NGINX_OBJ_TARGET = nginx-$(NGINX_VER)/nginx.exe
$(NGINX_SRC_TARGET):
	$(WGET) http://nginx.org/download/$@
$(NGINX_OBJ_TARGET): $(NGINX_SRC_TARGET)
	unzip $<
	touch -c $@
$(BUILD)/sbin/nginx.exe: $(NGINX_OBJ_TARGET)
	mkdir -p $(BUILD)/sbin
	cp -a nginx-$(NGINX_VER)/* $(BUILD)/sbin
	touch -c $@
get-nginx: $(NGINX_SRC_TARGET)
configure-nginx: get-nginx
compile-nginx: $(NGINX_OBJ_TARGET)
check-nginx: compile-nginx
install-nginx: $(BUILD)/sbin/nginx.exe
endif


## GMP ##

GMP_SRC_TARGET = gmp-$(GMP_VER)/.libs/libgmp.$(SHLIB_EXT)
GMP_OBJ_TARGET = $(BUILD)/lib/libgmp.$(SHLIB_EXT)

gmp-$(GMP_VER).tar.bz2:
	$(WGET) ftp://ftp.gmplib.org/pub/gmp-$(GMP_VER)/$@
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
	$(WGET) http://zlib.net/$@
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
ifeq ($(OS), Linux)
TKW_LIB += -ltcl8.5 -ltk8.5 -L$(BUILD)/lib/
else
TKW_LIB += -ltcl -ltk -L$(BUILD)/lib/
endif

$(BUILD)/lib/libtk_wrapper.$(SHLIB_EXT): tk_wrapper.c
	$(CC) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(TKW_INC) tk_wrapper.c $(TKW_LIB) -o $(BUILD)/lib/libtk_wrapper.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libtk_wrapper.$(SHLIB_EXT) $@
	touch -c $@

clean-tk-wrapper:
	-rm -f $(BUILD)/lib/libtk_wrapper.$(SHLIB_EXT)
distclean-tk-wrapper: clean-tk-wrapper

get-tk-wrapper: 
configure-tk-wrapper: 
compile-tk-wrapper: 
check-tk-wrapper:
install-tk-wrapper: $(BUILD)/lib/libtk_wrapper.$(SHLIB_EXT)


## patchelf ##

PATCHELF_SOURCE = patchelf-$(PATCHELF_VER)/src/patchelf
PATCHELF_TARGET = $(BUILD)/bin/patchelf

compile-patchelf: install-patchelf
install-patchelf: $(PATCHELF_TARGET)

patchelf-$(PATCHELF_VER).tar.bz2:
	$(WGET) http://hydra.nixos.org/build/1524660/download/2/$@
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


## phony targets ##

.PHONY: \
	default compile install cleanall distclean \
	get-* configure-* compile-* check-* install-* \
	clean-* distclean-*

