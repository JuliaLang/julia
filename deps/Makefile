## high-level setup ##
JULIAHOME = $(abspath ..)
include Versions.make
include $(JULIAHOME)/Make.inc

CONFIGURE_COMMON = --prefix=$(abspath $(build_prefix)) --build=$(BUILD_MACHINE) --libdir=$(abspath $(build_libdir))
ifneq ($(XC_HOST),)
CONFIGURE_COMMON += --host=$(XC_HOST)
endif
ifeq ($(OS),WINNT)
ifneq ($(USEMSVC), 1)
CONFIGURE_COMMON += LDFLAGS=-Wl,--stack,8388608
endif
endif
CONFIGURE_COMMON += F77="$(FC)" CC="$(CC)" CXX="$(CXX)"

CMAKE_COMMON = -DCMAKE_INSTALL_PREFIX:PATH=$(build_prefix) -DCMAKE_BUILD_TYPE=Release
ifneq ($(VERBOSE), 0)
CMAKE_COMMON += -DCMAKE_VERBOSE_MAKEFILE=ON
endif
CMAKE_COMMON += -DCMAKE_C_COMPILER="$(CC_BASE)"
ifdef CC_ARG
CMAKE_COMMON += -DCMAKE_C_COMPILER_ARG1="$(CC_ARG)"
endif
CMAKE_COMMON += -DCMAKE_CXX_COMPILER="$(CXX_BASE)"
ifdef CXX_ARG
CMAKE_COMMON += -DCMAKE_CXX_COMPILER_ARG1="$(CXX_ARG)"
endif

# If the top-level Makefile is called with environment variables,
# they will override the values passed above to ./configure
MAKE_COMMON = DESTDIR="" prefix=$(build_prefix) bindir=$(build_bindir) libdir=$(build_libdir) libexecdir=$(build_libexecdir) datarootdir=$(build_datarootdir) includedir=$(build_includedir) sysconfdir=$(build_sysconfdir)

#autoconf configure-driven scripts: llvm pcre arpack fftw unwind gmp mpfr patchelf uv
#custom Makefile rules: openlibm Rmath dsfmt suitesparse-wrapper suitesparse lapack openblas mojibake objconv
#CMake libs: libgit2

# prevent installing libs into usr/lib64 on opensuse
unexport CONFIG_SITE

STAGE1_DEPS =
STAGE2_DEPS = Rmath
STAGE3_DEPS = suitesparse-wrapper

ifeq ($(USE_SYSTEM_LIBUV), 0)
STAGE1_DEPS += uv
endif

ifeq ($(USE_SYSTEM_LIBUNWIND), 0)
ifeq ($(OS), Linux)
STAGE1_DEPS += unwind
else ifeq ($(OS), FreeBSD)
STAGE1_DEPS += unwind
else ifeq ($(OS), Darwin)
STAGE1_DEPS += osxunwind
endif
endif

ifeq ($(OS), Linux)
ifeq ($(shell which patchelf 2>/dev/null),)
STAGE1_DEPS += patchelf
PATCHELF=$(build_bindir)/patchelf
else
PATCHELF=patchelf
endif
endif

## USE_SYSTEM_LIBS options

ifeq ($(USE_SYSTEM_OPENLIBM), 0)
ifeq ($(USE_SYSTEM_LIBM), 0)
STAGE1_DEPS += openlibm
endif
endif

ifeq ($(USE_SYSTEM_OPENSPECFUN), 0)
STAGE1_DEPS += openspecfun
endif

ifeq ($(USE_SYSTEM_DSFMT), 0)
STAGE1_DEPS += dsfmt
endif

ifeq ($(USE_SYSTEM_LLVM), 0)
STAGE1_DEPS += llvm
endif

ifeq ($(USE_SYSTEM_PCRE), 0)
STAGE1_DEPS += pcre
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
STAGE1_DEPS += openblas
ifeq ($(USE_BLAS64), 1)
ifeq ($(OS), Darwin)
STAGE1_DEPS += objconv
endif
endif
endif

ifeq ($(USE_SYSTEM_FFTW), 0)
STAGE1_DEPS += fftw
endif

ifeq ($(USE_SYSTEM_GMP), 0)
STAGE1_DEPS += gmp
endif

ifeq ($(USE_SYSTEM_LIBGIT2), 0)
STAGE1_DEPS += libgit2
endif

ifeq ($(USE_SYSTEM_MPFR), 0)
STAGE2_DEPS += mpfr
ifeq ($(USE_SYSTEM_GMP), 0)
MPFR_OPTS = --with-gmp-include=$(abspath $(build_includedir)) --with-gmp-lib=$(abspath $(build_shlibdir))
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

ifeq ($(USE_SYSTEM_MOJIBAKE), 0)
STAGE2_DEPS += mojibake
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
endif

## Common build target prefixes

DEP_LIBS = $(STAGE1_DEPS) $(STAGE2_DEPS) $(STAGE3_DEPS)

default: $(build_prefix) install
get: $(addprefix get-, $(DEP_LIBS))
configure: $(addprefix configure-, $(DEP_LIBS))
compile: $(addprefix compile-, $(DEP_LIBS))
check: $(addprefix check-, $(DEP_LIBS))
install: $(addprefix install-, $(DEP_LIBS))
cleanall: $(addprefix clean-, $(DEP_LIBS))
distcleanall: $(addprefix distclean-, $(DEP_LIBS))
	rm -rf $(build_prefix)
getall: get-llvm get-uv get-pcre get-openlibm get-openspecfun get-dsfmt get-Rmath get-openblas get-lapack get-fftw get-suitesparse get-arpack get-unwind get-osxunwind get-gmp get-mpfr get-patchelf get-mojibake get-virtualenv get-objconv get-libgit2

## PATHS ##
# sort is used to remove potential duplicates
DIRS = $(sort $(build_bindir) $(build_libdir) $(build_includedir) $(build_sysconfdir) $(build_datarootdir))

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))

$(build_prefix): $(DIRS)

## LLVM ##
LLVM_GIT_URL_BASE ?= http://llvm.org/git
LLVM_GIT_URL_LLVM ?= $(LLVM_GIT_URL_BASE)/llvm.git
LLVM_GIT_URL_CLANG ?= $(LLVM_GIT_URL_BASE)/clang.git
LLVM_GIT_URL_COMPILER_RT ?= $(LLVM_GIT_URL_BASE)/compiler-rt.git
LLVM_GIT_URL_LLDB ?= $(LLVM_GIT_URL_BASE)/lldb.git
LLVM_GIT_URL_LIBCXX ?= $(LLVM_GIT_URL_BASE)/libcxx.git
LLVM_GIT_URL_LIBCXXABI ?= $(LLVM_GIT_URL_BASE)/libcxxabi.git

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
LLVM_FLAVOR := $(LLVM_BUILDTYPE)
ifeq ($(LLVM_SANITIZE),1)
LLVM_BUILDTYPE := $(LLVM_BUILDTYPE)+Sanitize
endif

LLVM_LIB_FILE = libLLVMCodeGen.a
LLVM_OBJ_SOURCE = llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/$(LLVM_FLAVOR)/lib/$(LLVM_LIB_FILE)
LLVM_OBJ_TARGET = $(build_libdir)/$(LLVM_LIB_FILE)

ifneq ($(LLVM_VER),svn)
ifeq ($(LLVM_VER), 3.3)
LLVM_TAR=llvm-$(LLVM_VER).src.tar.gz
else
LLVM_TAR=llvm-$(LLVM_VER).src.tar.xz
endif

ifeq ($(BUILD_LLDB),1)
ifeq ($(LLVM_VER), 3.5.0)
LLVM_LLDB_TAR=lldb-$(LLVM_VER).src.tar.xz
else
LLVM_LLDB_TAR=lldb-$(LLVM_VER).src.tar.gz
endif # LLVM_VER == 3.5.0
endif # BUILD_LLDB

ifeq ($(BUILD_LLVM_CLANG),1)
ifeq ($(LLVM_VER), 3.0)
LLVM_CLANG_TAR=clang-$(LLVM_VER).tar.gz
LLVM_COMPILER_RT_TAR=
else ifeq ($(LLVM_VER), 3.3)
LLVM_CLANG_TAR=cfe-$(LLVM_VER).src.tar.gz
LLVM_COMPILER_RT_TAR=compiler-rt-$(LLVM_VER).src.tar.gz
else ifeq ($(LLVM_VER), 3.5.0)
LLVM_CLANG_TAR=cfe-$(LLVM_VER).src.tar.xz
LLVM_COMPILER_RT_TAR=compiler-rt-$(LLVM_VER).src.tar.xz
else
LLVM_CLANG_TAR=clang-$(LLVM_VER).src.tar.gz
LLVM_COMPILER_RT_TAR=compiler-rt-$(LLVM_VER).src.tar.gz
endif # LLVM_VER
else
LLVM_CLANG_TAR=
LLVM_COMPILER_RT_TAR=
LLVM_LIBCXX_TAR=
endif # BUILD_LLVM_CLANG

ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LLVM_LIBCXX_TAR=libcxx-$(LLVM_VER).src.tar.gz
endif
endif # LLVM_VER != svn

LLVM_CXXFLAGS = $(CXXFLAGS)
LLVM_CPPFLAGS = $(CPPFLAGS)
LLVM_LDFLAGS = $(LDFLAGS)
LLVM_TARGET_FLAGS= --enable-targets=host
LLVM_FLAGS += --disable-profiling --enable-shared --enable-static $(LLVM_TARGET_FLAGS) --disable-bindings --disable-docs
LLVM_MFLAGS =
ifeq ($(LLVM_ASSERTIONS), 1)
LLVM_FLAGS += --enable-assertions
ifeq ($(OS), WINNT)
LLVM_FLAGS += --disable-embed-stdcxx
endif # OS == WINNT
else
LLVM_FLAGS += --disable-assertions
endif # LLVM_ASSERTIONS
ifeq ($(LLVM_DEBUG), 1)
LLVM_FLAGS += --disable-optimized --enable-debug-symbols --enable-keep-symbols
else
LLVM_FLAGS += --enable-optimized
endif # LLVM_DEBUG
ifeq ($(USE_LIBCPP), 1)
LLVM_FLAGS += --enable-libcpp
endif # USE_LIBCPP
ifeq ($(OS), WINNT)
LLVM_FLAGS += --with-extra-ld-options="-Wl,--stack,8388608" LDFLAGS="" --disable-shared
LLVM_CPPFLAGS += -D__USING_SJLJ_EXCEPTIONS__ -D__CRT__NO_INLINE
endif # OS == WINNT
ifeq ($(USE_INTEL_JITEVENTS), 1)
LLVM_FLAGS += --with-intel-jitevents
ifeq ($(OS), WINNT)
LLVM_FLAGS += --disable-threads
endif # OS == WINNT
else
LLVM_FLAGS += --disable-threads
endif # USE_INTEL_JITEVENTS

ifeq ($(BUILD_LLDB),1)
ifeq ($(USECLANG),1)
LLVM_FLAGS += --enable-cxx11
else
LLVM_CXXFLAGS += -std=c++0x
endif # USECLANG
ifeq ($(LLDB_DISABLE_PYTHON),1)
LLVM_CXXFLAGS += -DLLDB_DISABLE_PYTHON
endif # LLDB_DISABLE_PYTHON
endif # BUILD_LLDB

ifeq ($(ARCH), ppc64)
LLVM_CXXFLAGS += -mminimal-toc
endif # ARCH == ppc64


ifeq ($(LLVM_SANITIZE),1)
LLVM_CC = CFLAGS="-fsanitize=address"
LLVM_LDFLAGS += -fsanitize=address
LLVM_CXXFLAGS += -fsanitize=address
LLVM_MFLAGS += TOOL_NO_EXPORTS= HAVE_LINK_VERSION_SCRIPT=0
else
LLVM_CC =
endif # LLVM_SANITIZE

ifneq ($(LLVM_CXXFLAGS),)
LLVM_FLAGS += CXXFLAGS="$(LLVM_CXXFLAGS)"
LLVM_MFLAGS += CXXFLAGS="$(LLVM_CXXFLAGS)"
endif # LLVM_CXXFLAGS
LLVM_MFLAGS += $(LLVM_CC)

ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LLVM_LDFLAGS += -Wl,-R$(build_libdir)
ifeq ($(USEICC),1)
LLVM_LDFLAGS += -no_cpprt -lc++ -lc++abi
endif # USEICC
endif # BUILD_CUSTOM_LIBCXX

ifneq ($(LLVM_CPPFLAGS),)
LLVM_FLAGS += CPPFLAGS="$(LLVM_CPPFLAGS)"
LLVM_MFLAGS += CPPFLAGS="$(LLVM_CPPFLAGS)"
endif
ifneq ($(LLVM_LDFLAGS),)
LLVM_FLAGS += LDFLAGS="$(LLVM_LDFLAGS)"
LLVM_MFLAGS += LDFLAGS="$(LLVM_LDFLAGS)"
endif

ifneq ($(LLVM_CLANG_TAR),)
$(LLVM_CLANG_TAR):
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$@
endif
ifneq ($(LLVM_COMPILER_RT_TAR),)
$(LLVM_COMPILER_RT_TAR):
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$@
endif

ifneq ($(LLVM_LIBCXX_TAR),)
$(LLVM_LIBCXX_TAR):
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$@
endif
ifneq ($(LLVM_VER),svn)
$(LLVM_TAR):
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$@
endif

ifneq ($(LLVM_LLDB_TAR),)
$(LLVM_LLDB_TAR):
	$(JLDOWNLOAD) $@ http://llvm.org/releases/$(LLVM_VER)/$@
endif
ifeq ($(BUILD_LLDB),1)
llvm-$(LLVM_VER)/tools/lldb:
llvm-$(LLVM_VER)/configure: llvm-$(LLVM_VER)/tools/lldb
endif

llvm-$(LLVM_VER)/python2_path:
	mkdir -p llvm-$(LLVM_VER)/python2_path
	-python -c 'import sys; sys.exit(not sys.version_info > (3, 0))' && \
	/usr/bin/python2 -c 'import sys; sys.exit(not sys.version_info < (3, 0))' && \
	ln -sf /usr/bin/python2 "llvm-$(LLVM_VER)/python2_path/python" && \
	ln -sf /usr/bin/python2-config "llvm-$(LLVM_VER)/python2_path/python-config"
ifeq ($(LLVM_VER), 3.2)
## LLVM needs python 2.x, but doesn't check for it, so we have to use an ugly workaround to make it compile
llvm_python_workaround=llvm-$(LLVM_VER)/python2_path
else
  LLVM_FLAGS += --with-python="$(shell ./find_python2)"
endif
# LLDB still relies on plenty of python 2.x infrastructure, without checking
ifeq ($(BUILD_LLDB),1)
llvm_python_workaround=llvm-$(LLVM_VER)/python2_path
endif

ifeq ($(BUILD_CUSTOM_LIBCXX),1)

llvm-$(LLVM_VER)/projects/libcxx: $(LLVM_LIBCXX_TAR) | llvm-svn/configure
	([ ! -d llvm-$(LLVM_VER)/projects/libcxx ] && \
	git clone $(LLVM_GIT_URL_LIBCXX) llvm-$(LLVM_VER)/projects/libcxx  ) || \
	(cd llvm-$(LLVM_VER)/projects/libcxx  && \
	git pull --ff-only)
llvm-$(LLVM_VER)/projects/libcxxabi: $(LLVM_LIBCXXABI_TAR) | llvm-svn/configure
	([ ! -d llvm-$(LLVM_VER)/projects/libcxxabi ] && \
	git clone $(LLVM_GIT_URL_LIBCXXABI) llvm-$(LLVM_VER)/projects/libcxxabi  ) || \
	(cd llvm-$(LLVM_VER)/projects/libcxxabi  && \
	git pull --ff-only)
libcxx-build:
	mkdir -p libcxx-build
libcxx-build/Makefile: llvm-$(LLVM_VER)/projects/libcxx | llvm-$(LLVM_VER)/projects/libcxxabi libcxx-build
	cd libcxx-build && \
		$(CMAKE) -G "Unix Makefiles" $(CMAKE_COMMON) -DLIBCXX_CXX_ABI=libcxxabi -DLIBCXX_LIBCXXABI_INCLUDE_PATHS="../llvm-$(LLVM_VER)/projects/libcxxabi/include" ../llvm-$(LLVM_VER)/projects/libcxx -DCMAKE_SHARED_LINKER_FLAGS="-L$(build_libdir) -Bstatic -lirc -Bdynamic" -DCMAKE_CXX_FLAGS="$(CXXFLAGS)"
libcxxabi-build:
	mkdir -p libcxxabi-build
libcxxabi-build/Makefile: llvm-$(LLVM_VER)/projects/libcxxabi | libcxxabi-build
	cd libcxxabi-build && \
        $(CMAKE) -G "Unix Makefiles" $(CMAKE_COMMON) -DLLVM_MAIN_SRC_DIR=../llvm-svn ../llvm-$(LLVM_VER)/projects/libcxxabi -DLIBCXXABI_CXX_ABI_LIBRARIES="-Bstatic -lirc -Bdynamic" -DCMAKE_CXX_FLAGS="$(CXXFLAGS) -std=c++11"
llvm-$(LLVM_VER)/projects/libcxxabi/lib/libc++abi.so.1.0: llvm-$(LLVM_VER)/projects/libcxxabi | llvm-$(LLVM_VER)/projects/libcxx
libcxxabi-build/libc++abi.so.1.0: | libcxxabi-build/Makefile
	cd libcxxabi-build && $(MAKE)
$(build_libdir)/libc++abi.so.1.0: libcxxabi-build/lib/libc++abi.so.1.0
	cd libcxxabi-build && $(MAKE) install
	touch $@
install-libcxxabi: $(build_libdir)/libc++abi.so.1.0
libcxx-build/lib/libc++.so.1.0: | $(build_libdir)/libc++abi.so.1.0 libcxx-build/Makefile
	cd libcxx-build && $(MAKE)
$(build_libdir)/libc++.so.1.0: libcxx-build/lib/libc++.so.1.0
	cd libcxx-build && $(MAKE) install
	touch $@
install-libcxx: $(build_libdir)/libc++.so.1.0
get-libcxx: llvm-$(LLVM_VER)/projects/libcxx
get-libcxxabi: llvm-$(LLVM_VER)/projects/libcxxabi
endif

ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LIBCXX_DEPENDENCY = $(build_libdir)/libc++abi.so.1.0 $(build_libdir)/libc++.so.1.0
LIBCXX_GET_DEPENDENCY = get-libcxx get-libcxxabi
endif

llvm-$(LLVM_VER)/configure: $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR) $(LLVM_LIBCXX_TAR) $(LLVM_LLDB_TAR)
ifneq ($(LLVM_CLANG_TAR),)
	$(JLCHECKSUM) $(LLVM_CLANG_TAR)
endif
ifneq ($(LLVM_COMPILER_RT_TAR),)
	$(JLCHECKSUM) $(LLVM_COMPILER_RT_TAR)
endif
ifneq ($(LLVM_LIBCXX_TAR),)
	$(JLCHECKSUM) $(LLVM_LIBCXX_TAR)
endif
ifneq ($(LLVM_VER),svn)
	$(JLCHECKSUM) $(LLVM_TAR)
endif
ifneq ($(LLVM_LLDB_TAR),)
	$(JLCHECKSUM) $(LLVM_LLDB_TAR)
endif
ifneq ($(LLVM_VER),svn)
	mkdir -p llvm-$(LLVM_VER) && \
	$(TAR) -C llvm-$(LLVM_VER) --strip-components 1 -xf $(LLVM_TAR)
else
	([ ! -d llvm-$(LLVM_VER) ] && \
		git clone $(LLVM_GIT_URL_LLVM) llvm-$(LLVM_VER) ) || \
		(cd llvm-$(LLVM_VER) && \
		git pull --ff-only)
ifneq ($(LLVM_GIT_VER),)
	(cd llvm-$(LLVM_VER) && \
		git checkout $(LLVM_GIT_VER))
endif
endif
ifneq ($(LLVM_VER),svn)
ifneq ($(LLVM_CLANG_TAR),)
	mkdir -p llvm-$(LLVM_VER)/tools/clang && \
	$(TAR) -C llvm-$(LLVM_VER)/tools/clang --strip-components 1 -xf $(LLVM_CLANG_TAR)
endif
ifneq ($(LLVM_COMPILER_RT_TAR),)
	mkdir -p llvm-$(LLVM_VER)/projects/compiler-rt && \
	$(TAR) -C llvm-$(LLVM_VER)/projects/compiler-rt --strip-components 1 -xf $(LLVM_COMPILER_RT_TAR)
endif
ifneq ($(LLVM_LLDB_TAR),)
	mkdir -p llvm-$(LLVM_VER)/tools/lldb && \
	$(TAR) -C llvm-$(LLVM_VER)/tools/lldb --strip-components 1 -xf $(LLVM_LLDB_TAR)
endif
else
ifeq ($(BUILD_LLVM_CLANG),1)
	([ ! -d llvm-$(LLVM_VER)/tools/clang ] && \
		git clone $(LLVM_GIT_URL_CLANG) llvm-$(LLVM_VER)/tools/clang  ) || \
		(cd llvm-$(LLVM_VER)/tools/clang  && \
		git pull --ff-only)
endif
ifeq ($(BUILD_LLDB),1)
	([ ! -d llvm-$(LLVM_VER)/tools/lldb ] && \
		git clone $(LLVM_GIT_URL_LLDB) llvm-$(LLVM_VER)/tools/lldb  ) || \
		(cd llvm-$(LLVM_VER)/tools/lldb  && \
		git pull --ff-only)
endif
endif
ifeq ($(LLVM_VER),svn)
ifeq ($(BUILD_LLVM_CLANG),1)
	([ ! -d llvm-$(LLVM_VER)/tools/clang ] && \
		git clone $(LLVM_GIT_URL_CLANG) llvm-$(LLVM_VER)/tools/clang  ) || \
		(cd llvm-$(LLVM_VER)/tools/clang  && \
		git pull --ff-only)
	([ ! -d llvm-$(LLVM_VER)/projects/compiler-rt ] && \
		git clone $(LLVM_GIT_URL_COMPILER_RT) llvm-$(LLVM_VER)/projects/compiler-rt  ) || \
		(cd llvm-$(LLVM_VER)/projects/compiler-rt  && \
		git pull --ff-only)
ifneq ($(LLVM_CLANG_VER),)
	(cd llvm-$(LLVM_VER) && \
		git checkout $(LLVM_GIT_VER))
endif
endif
endif

ifeq ($(LLVM_VER),3.3)
	patch -p0 < llvm-3.3.patch
	patch -p0 < instcombine-llvm-3.3.patch
	patch -p0 < int128-vector.llvm-3.3.patch
	(cd llvm-$(LLVM_VER) && patch < ../osx-10.10.llvm-3.3.patch)
ifeq ($(OS),WINNT)
ifeq ($(ARCH),x86_64)
	patch -p0 < win64-int128.llvm-3.3.patch
endif
endif
endif
	touch -c $@

llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/config.status: llvm-$(LLVM_VER)/configure | $(llvm_python_workaround) $(LIBCXX_DEPENDENCY)
	cd llvm-$(LLVM_VER) && \
	mkdir -p build_$(LLVM_BUILDTYPE) && cd build_$(LLVM_BUILDTYPE) && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	../configure $(CONFIGURE_COMMON) $(LLVM_CC) $(LLVM_FLAGS)
	touch -c $@

$(LLVM_OBJ_SOURCE): llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/config.status | $(llvm_python_workaround)
	cd llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE) && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) $(LLVM_MFLAGS) $(MAKE_COMMON)
	touch -c $@

llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/checked: $(LLVM_OBJ_SOURCE) | $(llvm_python_workaround)
ifeq ($(OS),$(BUILD_OS))
	cd llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE) && \
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) $(LLVM_MFLAGS) check
endif
	echo 1 > $@

$(LLVM_OBJ_TARGET): $(LLVM_OBJ_SOURCE) | $(llvm_python_workaround)
	# LLVM has weird install prefixes (see llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/Makefile.config for the full list)
	# We map them here to the "normal" ones, which means just prefixing "PROJ_" to the variable name.
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) -C llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE) install $(LLVM_MFLAGS) $(MAKE_COMMON) PROJ_libdir=$(build_libdir) PROJ_bindir=$(build_bindir) PROJ_includedir=$(build_includedir)
	touch -c $@

reinstall-llvm:
	-rm $(build_bindir)/llvm-config
	export PATH=$(abspath llvm-$(LLVM_VER)/python2_path):$$PATH && \
	$(MAKE) -C llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE) install $(LLVM_MFLAGS) $(MAKE_COMMON) PROJ_libdir=$(build_libdir) PROJ_bindir=$(build_bindir)

clean-llvm:
	-$(MAKE) -C llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE) clean
	-rm -f $(build_bindir)/llvm-config
distclean-llvm:
	-rm -rf llvm-$(LLVM_VER).tar.gz llvm-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).src.tar.gz clang-$(LLVM_VER).tar.gz libcxx-$(LLVM_VER).src.tar.gz compiler-rt-$(LLVM_VER).src.tar.gz llvm-$(LLVM_VER)

ifneq ($(LLVM_VER),svn)
get-llvm: $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR) $(LLVM_LIBCXX_TAR) $(LLVM_LLDB_TAR)
else
get-llvm: llvm-$(LLVM_VER)/configure $(LIBCXX_GET_DEPENDENCY)
endif
configure-llvm: llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/config.status
compile-llvm: $(LLVM_OBJ_SOURCE)
check-llvm: llvm-$(LLVM_VER)/build_$(LLVM_BUILDTYPE)/checked
install-llvm: $(LLVM_OBJ_TARGET)
#todo: LLVM make check target is broken on julia.mit.edu (and really slow elsewhere)

update-llvm:
	(cd llvm-$(LLVM_VER); git pull --ff-only)
	([ -d llvm-$(LLVM_VER)/tools/clang ] && (cd llvm-$(LLVM_VER)/tools/clang; git pull --ff-only))
	([ -d llvm-$(LLVM_VER)/projects/compiler-rt ] && (cd llvm-$(LLVM_VER)/projects/compiler-rt; git pull --ff-only))
	([ -d llvm-$(LLVM_VER)/tools/lldb ] && (cd llvm-$(LLVM_VER)/tools/lldb; git pull --ff-only))

## LIBUV ##

UV_SRC_TARGET = libuv/.libs/libuv.a
UV_OBJ_TARGET = $(build_libdir)/libuv.a

UV_CFLAGS =
ifeq ($(USEMSVC), 1)
UV_CFLAGS += -DBUILDING_UV_SHARED
endif
ifeq ($(USEICC), 1)
UV_CFLAGS += -static-intel
endif

UV_MFLAGS += LDFLAGS="$(LDFLAGS) $(CLDFLAGS) -v"
ifneq ($(UV_CFLAGS),)
UV_MFLAGS += CFLAGS="$(UV_CFLAGS)"
endif
ifneq ($(USEMSVC), 1)
UV_FLAGS = $(UV_MFLAGS)
else
UV_FLAGS = --disable-shared $(UV_MFLAGS)
endif

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
	./configure --with-pic $(CONFIGURE_COMMON) $(UV_FLAGS)
	touch -c $@
$(UV_SRC_TARGET): libuv/config.status
	touch -c libuv/aclocal.m4
	touch -c libuv/Makefile.in
	touch -c libuv/configure
	touch -c libuv/config.status
	$(MAKE) -C libuv $(UV_MFLAGS)
	touch -c $@
libuv/checked: $(UV_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	-$(MAKE) -C libuv check
endif
	echo 1 > $@
$(UV_OBJ_TARGET): $(UV_SRC_TARGET)
	$(MAKE) -C libuv install $(MAKE_COMMON)
	$(INSTALL_NAME_CMD)libuv.$(SHLIB_EXT) $(build_shlibdir)/libuv.$(SHLIB_EXT)

clean-uv:
	-$(MAKE) -C libuv clean
	-rm -rf $(build_shlibdir)/libuv.a $(build_includedir)/uv.h $(build_includedir)/uv-private
distclean-uv: clean-uv
	-$(MAKE) -C libuv distclean

get-uv: libuv/configure
configure-uv: libuv/config.status
compile-uv: $(UV_SRC_TARGET)
check-uv: libuv/checked
install-uv: $(UV_OBJ_TARGET)


## PCRE ##

PCRE_SRC_TARGET = pcre-$(PCRE_VER)/.libs/libpcre.$(SHLIB_EXT)
PCRE_OBJ_TARGET = $(build_shlibdir)/libpcre.$(SHLIB_EXT)

pcre-$(PCRE_VER).tar.bz2:
	$(JLDOWNLOAD) $@ http://sourceforge.net/projects/pcre/files/pcre/$(PCRE_VER)/$@/download
pcre-$(PCRE_VER)/configure: pcre-$(PCRE_VER).tar.bz2
	$(JLCHECKSUM) $<
	$(TAR) jxf $<
	touch -c $@
pcre-$(PCRE_VER)/config.status: pcre-$(PCRE_VER)/configure
	cd pcre-$(PCRE_VER) && \
	./configure $(CONFIGURE_COMMON) --enable-utf --enable-unicode-properties --enable-jit --includedir=$(build_includedir)
	touch -c $@
$(PCRE_SRC_TARGET): pcre-$(PCRE_VER)/config.status
	$(MAKE) -C pcre-$(PCRE_VER) $(LIBTOOL_CCLD)
	touch -c $@
pcre-$(PCRE_VER)/checked: $(PCRE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
ifneq ($(OS),WINNT)
	$(MAKE) -C pcre-$(PCRE_VER) check -j1
endif
endif
	echo 1 > $@
$(PCRE_OBJ_TARGET): $(PCRE_SRC_TARGET) pcre-$(PCRE_VER)/checked
	$(MAKE) -C pcre-$(PCRE_VER) $(LIBTOOL_CCLD) install $(MAKE_COMMON)
	$(INSTALL_NAME_CMD)libpcre.$(SHLIB_EXT) $@
	touch -c $@

clean-pcre:
	-$(MAKE) -C pcre-$(PCRE_VER) clean
	-rm -f $(build_shlibdir)/libpcre*
distclean-pcre: clean-pcre
	-rm -rf pcre-$(PCRE_VER).tar.bz2 pcre-$(PCRE_VER)

get-pcre: pcre-$(PCRE_VER).tar.bz2
configure-pcre: pcre-$(PCRE_VER)/config.status
compile-pcre: $(PCRE_SRC_TARGET)
check-pcre: pcre-$(PCRE_VER)/checked
install-pcre: $(PCRE_OBJ_TARGET)


## openlibm ##

OPENLIBM_FLAGS = ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC)

OPENLIBM_OBJ_TARGET = $(build_shlibdir)/libopenlibm.$(SHLIB_EXT) $(build_libdir)/libopenlibm.a
OPENLIBM_OBJ_SOURCE = openlibm/libopenlibm.$(SHLIB_EXT)

openlibm/Makefile:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d openlibm/.git ] && echo exists ))
$(OPENLIBM_OBJ_SOURCE): openlibm/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/.git/modules/deps/openlibm ] && echo exists ))
$(OPENLIBM_OBJ_SOURCE): $(JULIAHOME)/.git/modules/deps/openlibm/HEAD
endif
$(OPENLIBM_OBJ_SOURCE): openlibm/Makefile
	$(MAKE) -C openlibm $(OPENLIBM_FLAGS) $(MAKE_COMMON)
	touch -c $@
$(OPENLIBM_OBJ_TARGET): $(OPENLIBM_OBJ_SOURCE)
	$(MAKE) -C openlibm install $(OPENLIBM_FLAGS) $(MAKE_COMMON)
	$(INSTALL_NAME_CMD)libopenlibm.$(SHLIB_EXT) $(build_shlibdir)/libopenlibm.$(SHLIB_EXT)
	touch -c $@

clean-openlibm:
	-$(MAKE) -C openlibm distclean $(OPENLIBM_FLAGS)
	-rm $(OPENLIBM_OBJ_TARGET)
	-rm $(build_libdir)/libopenlibm.a

distclean-openlibm: clean-openlibm
	-cd openlibm && git clean -fdx

get-openlibm: openlibm/Makefile
configure-openlibm: get-openlibm
compile-openlibm: $(OPENLIBM_OBJ_SOURCE)
check-openlibm: compile-openlibm
install-openlibm: $(OPENLIBM_OBJ_TARGET)

## openspecfun ##

OPENSPECFUN_FLAGS = ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) FFLAGS="$(JFFLAGS)"

ifeq ($(USE_SYSTEM_LIBM),0)
	OPENSPECFUN_FLAGS += USE_OPENLIBM=1
	OPENLIBM_FAKE_TARGET = $(OPENLIBM_OBJ_TARGET)
endif

OPENSPECFUN_OBJ_TARGET = $(build_shlibdir)/libopenspecfun.$(SHLIB_EXT)
OPENSPECFUN_OBJ_SOURCE = openspecfun/libopenspecfun.$(SHLIB_EXT)

openspecfun/Makefile openspecfun/Makefile.extras:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d openspecfun/.git ] && echo exists ))
$(OPENSPECFUN_OBJ_SOURCE): openspecfun/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/.git/modules/deps/openspecfun ] && echo exists ))
$(OPENSPECFUN_OBJ_SOURCE): $(JULIAHOME)/.git/modules/deps/openspecfun/HEAD
endif

$(OPENSPECFUN_OBJ_SOURCE): openspecfun/Makefile $(OPENLIBM_FAKE_TARGET)
	$(MAKE) -C openspecfun $(OPENSPECFUN_FLAGS) $(MAKE_COMMON)
	touch -c $@
$(OPENSPECFUN_OBJ_TARGET): $(OPENSPECFUN_OBJ_SOURCE)
	$(MAKE) -C openspecfun install $(OPENSPECFUN_FLAGS) $(MAKE_COMMON)
	$(INSTALL_NAME_CMD)libopenspecfun.$(SHLIB_EXT) $@
	touch -c $@

clean-openspecfun:
	-$(MAKE) -C openspecfun distclean $(OPENSPECFUN_FLAGS)
	-rm $(OPENSPECFUN_OBJ_TARGET)
	-rm $(build_libdir)/libopenspecfun.a
distclean-openspecfun: clean-openspecfun
	-cd openspecfun && git clean -fdx

get-openspecfun: openspecfun/Makefile
configure-openspecfun: get-openspecfun
compile-openspecfun: $(OPENSPECFUN_OBJ_SOURCE)
check-openspecfun: compile-openspecfun
install-openspecfun: $(OPENSPECFUN_OBJ_TARGET)

## DSFMT ##

DSFMT_OBJ_TARGET = $(build_shlibdir)/libdSFMT.$(SHLIB_EXT)
DSFMT_OBJ_SOURCE = dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT)

DSFMT_CFLAGS = $(CFLAGS) -DNDEBUG -DDSFMT_MEXP=19937 $(fPIC) -DDSFMT_DO_NOT_USE_OLD_NAMES
ifneq ($(USEMSVC), 1)
DSFMT_CFLAGS += -O3 -finline-functions -fomit-frame-pointer -fno-strict-aliasing \
		--param max-inline-insns-single=1800 -Wmissing-prototypes -Wall  -std=c99 -shared
else
DSFMT_CFLAGS += -Wl,-dll
endif
ifeq ($(ARCH), x86_64)
DSFMT_CFLAGS += -msse2 -DHAVE_SSE2
endif

dsfmt-$(DSFMT_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz
	touch -c $@
dsfmt-$(DSFMT_VER)/config.status: dsfmt-$(DSFMT_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p dsfmt-$(DSFMT_VER) && \
	$(TAR) -C dsfmt-$(DSFMT_VER) --strip-components 1 -xf dsfmt-$(DSFMT_VER).tar.gz && \
	cd dsfmt-$(DSFMT_VER) && patch < ../dSFMT.h.patch && patch < ../dSFMT.c.patch
	echo 1 > $@
$(DSFMT_OBJ_SOURCE): dsfmt-$(DSFMT_VER)/config.status
	cd dsfmt-$(DSFMT_VER) && \
	$(CC) $(CPPFLAGS) $(DSFMT_CFLAGS) $(LDFLAGS) dSFMT.c -o libdSFMT.$(SHLIB_EXT) && \
	cp dSFMT.h $(build_includedir)
$(DSFMT_OBJ_TARGET): $(DSFMT_OBJ_SOURCE)
	cp $< $@ && \
	$(INSTALL_NAME_CMD)libdSFMT.$(SHLIB_EXT) $(DSFMT_OBJ_TARGET)

clean-dsfmt:
	-rm -f dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT)
distclean-dsfmt: clean-dsfmt
	-rm -rf dsfmt*.tar.gz dsfmt-$(DSFMT_VER)

get-dsfmt: dsfmt-$(DSFMT_VER).tar.gz
configure-dsfmt: dsfmt-$(DSFMT_VER)/config.status
compile-dsfmt: $(DSFMT_OBJ_SOURCE)
check-dsfmt: compile-dsfmt
install-dsfmt: $(DSFMT_OBJ_TARGET)


## Rmath ##

ifeq ($(USE_SYSTEM_DSFMT),0)
DSFMT_FAKE_TARGET = $(DSFMT_OBJ_TARGET)
endif

RMATH_OBJ_TARGET = $(build_shlibdir)/libRmath-julia.$(SHLIB_EXT)
RMATH_OBJ_SOURCE = Rmath/src/libRmath-julia.$(SHLIB_EXT)

RMATH_FLAGS += CC="$(CC)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) \
			   OS="$(OS)" ARCH="$(ARCH)" \
			   USE_DSFMT=1 DSFMT_libdir="$(build_shlibdir)" \
			   DSFMT_includedir="$(build_includedir)"

Rmath/Make.inc:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d Rmath/.git ] && echo exists ))
$(RMATH_OBJ_SOURCE): Rmath/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/.git/modules/deps/Rmath ] && echo exists ))
$(RMATH_OBJ_SOURCE): $(JULIAHOME)/.git/modules/deps/Rmath/HEAD
endif
$(RMATH_OBJ_SOURCE): Rmath/Make.inc $(DSFMT_FAKE_TARGET)
	$(MAKE) -C Rmath/src $(RMATH_FLAGS) $(MAKE_COMMON)
	touch -c $@
$(RMATH_OBJ_TARGET): $(RMATH_OBJ_SOURCE) | $(build_shlibdir)
	cp $< $@
	$(INSTALL_NAME_CMD)libRmath-julia.$(SHLIB_EXT) $@

clean-Rmath:
	-$(MAKE) -C Rmath/src clean
	-rm $(RMATH_OBJ_TARGET)
distclean-Rmath: clean-Rmath
	-cd Rmath && git clean -fdx

get-Rmath: Rmath/Make.inc
configure-Rmath: get-Rmath
compile-Rmath: $(RMATH_OBJ_SOURCE)
check-Rmath: compile-Rmath
install-Rmath: $(RMATH_OBJ_TARGET)


## objconv ##

OBJCONV_SOURCE = objconv/objconv
OBJCONV_TARGET = $(build_bindir)/objconv

objconv.zip:
	$(JLDOWNLOAD) $@ http://www.agner.org/optimize/objconv.zip
objconv/config.status: objconv.zip
	unzip -d objconv $<
	cd objconv && unzip source.zip
	echo 1 > $@
$(OBJCONV_SOURCE): objconv/config.status
	cd objconv && $(CXX) -o objconv -O2 *.cpp
$(OBJCONV_TARGET): $(OBJCONV_SOURCE) | $(build_bindir)
	cp -f $< $@

clean-objconv:
	-rm -f $(OBJCONV_TARGET)
distclean-objconv:
	-rm -rf objconv.zip objconv

get-objconv: objconv.zip
configure-objconv: objconv/config.status
compile-objconv: $(OBJCONV_SOURCE)
check-objconv: compile-objconv
install-objconv: $(OBJCONV_TARGET)


## OpenBLAS ##
# LAPACK is built into OpenBLAS by default

OPENBLAS_OBJ_SOURCE = openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT)
OPENBLAS_OBJ_TARGET = $(build_shlibdir)/libopenblas.$(SHLIB_EXT)

OPENBLAS_BUILD_OPTS = CC="$(CC)" FC="$(FC)" RANLIB="$(RANLIB)" FFLAGS="$(FFLAGS) $(JFFLAGS)" LDFLAGS="$(LDFLAGS)" TARGET=$(OPENBLAS_TARGET_ARCH) BINARY=$(BINARY)

# Thread support
ifeq ($(OPENBLAS_USE_THREAD), 1)
OPENBLAS_BUILD_OPTS += USE_THREAD=1
OPENBLAS_BUILD_OPTS += GEMM_MULTITHREADING_THRESHOLD=50
# Maximum number of threads for parallelism
ifneq ($(ARCH),x86_64)
# Assume we can't address much memory to spawn many threads
# It is also unlikely that 32-bit architectures have too many cores
OPENBLAS_BUILD_OPTS += NUM_THREADS=8
else ifeq ($(OS),WINNT)
# Windows seems unable to handle very many
OPENBLAS_BUILD_OPTS += NUM_THREADS=16
else ifeq ($(OS),Darwin)
# This should suffice for the largest macs
OPENBLAS_BUILD_OPTS += NUM_THREADS=16
else
# On linux, try to provision for the largest possible machine currently
OPENBLAS_BUILD_OPTS += NUM_THREADS=16
endif
else
OPENBLAS_BUILD_OPTS += USE_THREAD=0
endif

# don't touch scheduler affinity since we manage this ourselves
OPENBLAS_BUILD_OPTS += NO_AFFINITY=1

# Build for all architectures - required for distribution
ifeq ($(OPENBLAS_DYNAMIC_ARCH), 1)
OPENBLAS_BUILD_OPTS += DYNAMIC_ARCH=1
endif

# 64-bit BLAS interface
ifeq ($(USE_BLAS64), 1)
OPENBLAS_BUILD_OPTS += INTERFACE64=1 SYMBOLSUFFIX="$(OPENBLAS_SYMBOLSUFFIX)"
ifeq ($(OS), Darwin)
OPENBLAS_BUILD_OPTS += OBJCONV=$(JULIAHOME)/deps/objconv/objconv
$(OPENBLAS_OBJ_SOURCE): $(OBJCONV_SOURCE)
endif
endif

# Decide whether to build for 32-bit or 64-bit arch
ifneq ($(BUILD_OS),$(OS))
OPENBLAS_BUILD_OPTS += OSNAME=$(OS) CROSS=1 HOSTCC=$(HOSTCC)
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

# Allow disabling AVX for older binutils
ifeq ($(OPENBLAS_NO_AVX), 1)
OPENBLAS_BUILD_OPTS += NO_AVX=1 NO_AVX2=1
else ifeq ($(OPENBLAS_NO_AVX2), 1)
OPENBLAS_BUILD_OPTS += NO_AVX2=1
endif

openblas-$(OPENBLAS_VER).tar.gz:
	$(JLDOWNLOAD) $@ https://github.com/xianyi/OpenBLAS/tarball/$(OPENBLAS_VER)
openblas-$(OPENBLAS_VER)/config.status: openblas-$(OPENBLAS_VER).tar.gz
ifneq ($(OPENBLAS_VER),develop)
	$(JLCHECKSUM) $<
endif
	mkdir -p openblas-$(OPENBLAS_VER) && \
	$(TAR) -C openblas-$(OPENBLAS_VER) --strip-components 1 -xf $<
	perl -i -ple 's/^\s*(EXTRALIB\s*\+=\s*-lSystemStubs)\s*$$/# $$1/g' openblas-$(OPENBLAS_VER)/Makefile.system
	echo 1 > $@
$(OPENBLAS_OBJ_SOURCE): openblas-$(OPENBLAS_VER)/config.status
	$(MAKE) -C openblas-$(OPENBLAS_VER) $(OPENBLAS_BUILD_OPTS) || (echo "*** Clean the OpenBLAS build with 'make -C deps clean-openblas'. Rebuild with 'make OPENBLAS_USE_THREAD=0 if OpenBLAS had trouble linking libpthread.so, and with 'make OPENBLAS_TARGET_ARCH=NEHALEM' if there were errors building SandyBridge support. Both these options can also be used simultaneously. ***" && false)
	touch -c $@
$(OPENBLAS_OBJ_TARGET): $(OPENBLAS_OBJ_SOURCE) | $(build_shlibdir)
	cp -f openblas-$(OPENBLAS_VER)/libopenblas.$(SHLIB_EXT) $(build_shlibdir)
ifeq ($(OS), Linux)
	cd $(build_shlibdir) && \
	ln -sf libopenblas.$(SHLIB_EXT) libopenblas.$(SHLIB_EXT).0
endif
	$(INSTALL_NAME_CMD)libopenblas.$(SHLIB_EXT) $(build_shlibdir)/libopenblas.$(SHLIB_EXT)

clean-openblas:
	-$(MAKE) -C openblas-$(OPENBLAS_VER) clean
distclean-openblas:
	-rm -rf openblas-$(OPENBLAS_VER).tar.gz openblas-$(OPENBLAS_VER)

get-openblas: openblas-$(OPENBLAS_VER).tar.gz
configure-openblas: openblas-$(OPENBLAS_VER)/config.status
compile-openblas: $(OPENBLAS_OBJ_SOURCE)
check-openblas: compile-openblas
install-openblas: $(OPENBLAS_OBJ_TARGET)


## ATLAS (currently 3.10.0) ##

# no threading, with full lapack, shared library
# should always be compiled with (a real) gcc, it's
# configure script will search for the best match
# (gcc 4.7, gcc, clang,ICC/microsoft/others)
ATLAS_OBJ_SOURCE = atlas/build/lib/libsatlas.$(SHLIB_EXT)
ATLAS_OBJ_TARGET = $(build_shlibdir)/libsatlas.$(SHLIB_EXT)
ATLAS_FLAGS = --shared --prefix=$(build_prefix) --cc=gcc -t 0 \
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

libgfortblas.$(SHLIB_EXT): gfortblas.c gfortblas.alias
	$(CC) -Wall -O3 $(CPPFLAGS) $(CFLAGS) $(fPIC) -shared $< -o $@ -pipe \
				-Wl,-reexport_framework,vecLib -Wl,-alias_list,gfortblas.alias
$(build_shlibdir)/libgfortblas.$(SHLIB_EXT): libgfortblas.$(SHLIB_EXT)
	cp -f $< $@
	$(INSTALL_NAME_CMD)libgfortblas.$(SHLIB_EXT) $@
endif

## LAPACK ##

ifeq ($(USE_SYSTEM_LAPACK), 0)
LAPACK_OBJ_TARGET = $(build_shlibdir)/liblapack.$(SHLIB_EXT)
LAPACK_OBJ_SOURCE = lapack-$(LAPACK_VER)/liblapack.$(SHLIB_EXT)
else
LAPACK_OBJ_TARGET =
LAPACK_OBJ_SOURCE =
endif

LAPACK_MFLAGS = NOOPT="$(FFLAGS) $(JFFLAGS) $(GFORTBLAS_FFLAGS) -O0" OPTS="$(FFLAGS) $(JFFLAGS) $(GFORTBLAS_FFLAGS)" FORTRAN="$(FC)" LOADER="$(FC)"
ifneq ($(OS),WINNT)
LAPACK_MFLAGS += BLASLIB="-Wl,-rpath,'$(build_libdir)' $(LIBBLAS)"
endif

lapack-$(LAPACK_VER).tgz:
	$(JLDOWNLOAD) $@ http://www.netlib.org/lapack/$@
lapack-$(LAPACK_VER)/Makefile: lapack-$(LAPACK_VER).tgz
	$(JLCHECKSUM) $<
	$(TAR) zxf $<
	touch -c $@
ifeq ($(USE_SYSTEM_BLAS), 0)
lapack-$(LAPACK_VER)/liblapack.a: | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(OS),Darwin)
lapack-$(LAPACK_VER)/liblapack.a: | $(build_shlibdir)/libgfortblas.$(SHLIB_EXT)
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
	$(INSTALL_NAME_CMD)liblapack.$(SHLIB_EXT) $@

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
ARPACK_CFLAGS =

ifeq ($(USE_BLAS64), 1)
ifeq ($(USEIFC),1)
ARPACK_FFLAGS += -i8
else
ARPACK_FFLAGS += -fdefault-integer-8
ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(OPENBLAS_SYMBOLSUFFIX), 64_)
ARPACK_FFLAGS += -cpp -ffixed-line-length-none
ARPACK_OPENBLASFCNS1 = axpy copy gemv geqr2 lacpy lahqr lanhs larnv lartg lascl laset scal trevc trmm trsen
ARPACK_OPENBLASFCNS2 = dot ger labad laev2 lamch lanst lanv2 lapy2 larf larfg lasr nrm2 orm2r rot steqr swap
ARPACK_OPENBLASFCNS3 = dotc geru unm2r
ARPACK_OPENBLASFCNS4 = COPY LABAD LAMCH LANHS LANV2 LARFG ROT
ARPACK_FFLAGS += $(foreach fcn, $(ARPACK_OPENBLASFCNS1) $(ARPACK_OPENBLASFCNS2), -Ds$(fcn)=s$(fcn)_64 -Dd$(fcn)=d$(fcn)_64)
ARPACK_FFLAGS += $(foreach fcn, $(ARPACK_OPENBLASFCNS1) $(ARPACK_OPENBLASFCNS3), -Dc$(fcn)=c$(fcn)_64 -Dz$(fcn)=z$(fcn)_64)
ARPACK_FFLAGS += $(foreach fcn, $(ARPACK_OPENBLASFCNS4), -DS$(fcn)=S$(fcn)_64 -DD$(fcn)=D$(fcn)_64)
ARPACK_FFLAGS += -Dscnrm2=scnrm2_64 -Ddznrm2=dznrm2_64 -Dcsscal=csscal_64 -Dzdscal=zdscal_64
# CFLAGS are for the configure checks
ARPACK_CFLAGS += -Dsgemm_=sgemm_64_ -Dcheev_=cheev_64_
endif
endif
endif
endif

ifeq ($(OS),WINNT)
ARPACK_OBJ_SOURCE = arpack-ng-$(ARPACK_VER)/.libs/libarpack-2.$(SHLIB_EXT)
else
ARPACK_OBJ_SOURCE = arpack-ng-$(ARPACK_VER)/.libs/libarpack.$(SHLIB_EXT)
endif
ARPACK_OBJ_TARGET = $(build_shlibdir)/libarpack.$(SHLIB_EXT)

ARPACK_MFLAGS = F77="$(FC)" MPIF77="$(FC)"
ARPACK_FFLAGS += $(FFLAGS) $(JFFLAGS)
ARPACK_FLAGS = --with-blas="$(LIBBLAS)" --with-lapack="$(LIBLAPACK)" --disable-mpi --enable-shared FFLAGS="$(ARPACK_FFLAGS)" CFLAGS="$(ARPACK_CFLAGS)"
ifneq ($(OS),WINNT)
ARPACK_FLAGS += LDFLAGS="$(LDFLAGS) -Wl,-rpath,'$(build_libdir)'"
endif

# ARPACK-NG upstream keeps changing their download filenames
arpack-ng-$(ARPACK_VER).tar.gz:
	$(JLDOWNLOAD) $@ https://github.com/opencollab/arpack-ng/archive/$(ARPACK_VER).tar.gz
	touch -c $@
arpack-ng-$(ARPACK_VER)/configure: arpack-ng-$(ARPACK_VER).tar.gz
	$(JLCHECKSUM) $<
	$(TAR) zxf $<
	touch -c $@

ifeq ($(USE_ATLAS), 1)
arpack-ng-$(ARPACK_VER)/config.status: | $(ATLAS_OBJ_TARGET)
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
arpack-ng-$(ARPACK_VER)/config.status: | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(USE_SYSTEM_LAPACK), 0)
arpack-ng-$(ARPACK_VER)/config.status: | $(LAPACK_OBJ_TARGET)
endif

arpack-ng-$(ARPACK_VER)/config.status: arpack-ng-$(ARPACK_VER)/configure
	cd arpack-ng-$(ARPACK_VER) && \
	./configure $(CONFIGURE_COMMON) $(ARPACK_FLAGS)
	touch -c $@
$(ARPACK_OBJ_SOURCE): arpack-ng-$(ARPACK_VER)/config.status
	cd arpack-ng-$(ARPACK_VER) && \
	$(MAKE) $(ARPACK_MFLAGS)
	touch -c $@
arpack-ng-$(ARPACK_VER)/checked: $(ARPACK_OBJ_SOURCE)
	cd arpack-ng-$(ARPACK_VER)
#	$(MAKE) check $(ARPACK_MFLAGS) && \
#	cd TESTS && $(call spawn,./dnsimp$(EXE))
	echo 1 > $@
$(ARPACK_OBJ_TARGET): $(ARPACK_OBJ_SOURCE) arpack-ng-$(ARPACK_VER)/checked | $(build_shlibdir)
	cd arpack-ng-$(ARPACK_VER) && \
	$(MAKE) install $(ARPACK_MFLAGS) $(MAKE_COMMON)
ifeq ($(OS), WINNT)
	mv $(build_shlibdir)/libarpack-2.dll $@
endif
	$(INSTALL_NAME_CMD)libarpack.$(SHLIB_EXT) $(build_shlibdir)/libarpack.$(SHLIB_EXT)
ifeq ($(OS), Linux)
	for filename in $(build_shlibdir)/libarpack.so* ; do \
		[ -L $$filename ] || $(PATCHELF) --set-rpath '$$ORIGIN' $$filename ;\
	done
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
FFTW_SINGLE_OBJ_TARGET = $(build_shlibdir)/libfftw3f.$(SHLIB_EXT)
FFTW_DOUBLE_OBJ_TARGET = $(build_shlibdir)/libfftw3.$(SHLIB_EXT)

FFTW_CONFIG = --enable-shared --disable-fortran --disable-mpi --enable-threads
ifeq ($(ARCH), arm)
  FFTW_CONFIG += --enable-neon
else ifeq ($(ARCH), ppc)
  FFTW_CONFIG += --enable-altivec
else ifeq ($(ARCH), x86_64)
  FFTW_CONFIG += --enable-sse2 --enable-fma
endif
ifeq ($(OS),WINNT)
FFTW_CONFIG += --with-our-malloc --with-combined-threads
ifneq ($(ARCH),x86_64)
FFTW_CONFIG += --with-incoming-stack-boundary=2
endif
endif

fftw-$(FFTW_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://www.fftw.org/$@

fftw-$(FFTW_VER)-single/configure: fftw-$(FFTW_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p fftw-$(FFTW_VER)-single && \
	$(TAR) -C fftw-$(FFTW_VER)-single --strip-components 1 -xf $<
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
	$(MAKE) -C fftw-$(FFTW_VER)-single install $(MAKE_COMMON)
	touch -c $@
ifeq ($(OS), Darwin)
	$(INSTALL_NAME_CMD)libfftw3f.$(SHLIB_EXT) $(build_shlibdir)/libfftw3f.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libfftw3f_threads.$(SHLIB_EXT) $(build_shlibdir)/libfftw3f_threads.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) $(build_shlibdir)/libfftw3f.3.$(SHLIB_EXT) $(INSTALL_NAME_ID_DIR)libfftw3f.$(SHLIB_EXT) $(build_shlibdir)/libfftw3f_threads.$(SHLIB_EXT)
else ifeq ($(OS), WINNT)
	mv -f $(build_shlibdir)/libfftw3f-3.dll $@
else ifeq ($(OS), Linux)
	for filename in $(build_shlibdir)/libfftw3f_threads.so* ; do \
		[ -L $$filename ] || $(PATCHELF) --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch -c $@

fftw-$(FFTW_VER)-double/configure: fftw-$(FFTW_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p fftw-$(FFTW_VER)-double && \
	$(TAR) -C fftw-$(FFTW_VER)-double --strip-components 1 -xf $<
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
	$(MAKE) -C fftw-$(FFTW_VER)-double install $(MAKE_COMMON)
ifeq ($(OS), Darwin)
	$(INSTALL_NAME_CMD)libfftw3.$(SHLIB_EXT) $(build_shlibdir)/libfftw3.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libfftw3_threads.$(SHLIB_EXT) $(build_shlibdir)/libfftw3_threads.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) $(build_shlibdir)/libfftw3.3.$(SHLIB_EXT) $(INSTALL_NAME_ID_DIR)libfftw3.$(SHLIB_EXT) $(build_shlibdir)/libfftw3_threads.$(SHLIB_EXT)
else ifeq ($(OS), WINNT)
	mv -f $(build_shlibdir)/libfftw3-3.dll $@
else ifeq ($(OS), Linux)
	for filename in $(build_shlibdir)/libfftw3_threads.so* ; do \
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
distclean-fftw: distclean-fftw-single distclean-fftw-double
distclean-fftw-single:
	-rm -rf fftw-$(FFTW_VER).tar.gz fftw-$(FFTW_VER)-single
distclean-fftw-double:
	-rm -rf fftw-$(FFTW_VER).tar.gz fftw-$(FFTW_VER)-double

get-fftw: get-fftw-single get-fftw-double
configure-fftw: configure-fftw-single configure-fftw-double
compile-fftw: compile-fftw-single compile-fftw-double
check-fftw: check-fftw-single check-fftw-double
install-fftw: check-fftw-single check-fftw-double
	@$(MAKE) -s install-fftw-single
	@$(MAKE) -s install-fftw-double

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

## MOJIBAKE ##

MOJIBAKE_SRC_TARGET = libmojibake/libmojibake.a
MOJIBAKE_OBJ_LIB    = $(build_libdir)/libmojibake.a
MOJIBAKE_OBJ_HEADER = $(build_includedir)/mojibake.h
MOJIBAKE_OBJ_TARGET = $(MOJIBAKE_OBJ_LIB) $(MOJIBAKE_OBJ_HEADER)


libmojibake/Makefile:
	(cd .. && git submodule init && git submodule update)
ifeq (exists, $(shell [ -d libmojibake/.git ] && echo exists ))
$(MOJIBAKE_SRC_TARGET): libmojibake/.git/HEAD
endif
ifeq (exists, $(shell [ -d $(JULIAHOME)/.git/modules/deps/libmojibake ] && echo exists ))
$(MOJIBAKE_SRC_TARGET): $(JULIAHOME)/.git/modules/deps/libmojibake/HEAD
endif
$(MOJIBAKE_SRC_TARGET): libmojibake/Makefile
	$(MAKE) -C libmojibake cc="$(CC) -O2 -std=c99 $(fPIC) -DMOJIBAKE_EXPORTS" AR="$(AR)" libmojibake.a
	touch -c $@
libmojibake/checked: $(MOJIBAKE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	-$(MAKE) -C libmojibake check
endif
	echo 1 > $@

$(MOJIBAKE_OBJ_LIB): $(MOJIBAKE_SRC_TARGET)
	cp -f $< $@

$(MOJIBAKE_OBJ_HEADER): libmojibake/Makefile
	cp -f libmojibake/mojibake.h $@

clean-mojibake:
	-$(MAKE) -C libmojibake clean
	-rm -rf $(build_libdir)/libmojibake.a $(build_includedir)/mojibake.h
distclean-mojibake: clean-mojibake

get-mojibake: libmojibake/Makefile
configure-mojibake: get-mojibake
compile-mojibake: $(MOJIBAKE_SRC_TARGET)
check-mojibake: libmojibake/checked
install-mojibake: $(MOJIBAKE_OBJ_TARGET)

## SUITESPARSE ##

SUITESPARSE_OBJ_SOURCE = SuiteSparse-$(SUITESPARSE_VER)/UMFPACK/Lib/libumfpack.a
SUITESPARSE_OBJ_TARGET = $(build_shlibdir)/libspqr.$(SHLIB_EXT)


ifeq ($(USE_BLAS64), 1)
UMFPACK_CONFIG = -DLONGBLAS='long long'
CHOLMOD_CONFIG = -DLONGBLAS='long long'
SPQR_CONFIG = -DLONGBLAS='long long'
ifeq ($(USE_SYSTEM_BLAS), 0)
ifeq ($(OPENBLAS_SYMBOLSUFFIX), 64_)
UMFPACK_CONFIG += -DSUN64
CHOLMOD_CONFIG += -DSUN64
SPQR_CONFIG += -DSUN64
endif
endif
endif

SUITE_SPARSE_LIB = -lm
ifneq ($(OS), Darwin)
ifneq ($(OS), WINNT)
SUITE_SPARSE_LIB += -lrt
endif
endif
ifneq ($(OS), WINNT)
SUITE_SPARSE_LIB += -Wl,-rpath,'$(build_libdir)'
endif
SUITESPARSE_MFLAGS = CC="$(CC)" CXX="$(CXX)" F77="$(FC)" AR="$(AR)" RANLIB="$(RANLIB)" BLAS="$(LIBBLAS)" LAPACK="$(LIBLAPACK)" \
	  INSTALL_LIB="$(build_libdir)" INSTALL_INCLUDE="$(build_includedir)" LIB="$(SUITE_SPARSE_LIB)" \
	  UMFPACK_CONFIG="$(UMFPACK_CONFIG)" CHOLMOD_CONFIG="$(CHOLMOD_CONFIG)" SPQR_CONFIG="$(SPQR_CONFIG)"

SuiteSparse-$(SUITESPARSE_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://faculty.cse.tamu.edu/davis/SuiteSparse/$@
SuiteSparse-$(SUITESPARSE_VER)/Makefile: SuiteSparse-$(SUITESPARSE_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p SuiteSparse-$(SUITESPARSE_VER)
	$(TAR) -C SuiteSparse-$(SUITESPARSE_VER) --strip-components 1 -zxf $<
	touch -c $@

ifeq ($(USE_ATLAS), 1)
$(SUITESPARSE_OBJ_SOURCE): | $(ATLAS_OBJ_TARGET)
endif

ifeq ($(USE_SYSTEM_BLAS), 0)
$(SUITESPARSE_OBJ_SOURCE): | $(OPENBLAS_OBJ_TARGET)
else ifeq ($(USE_SYSTEM_LAPACK), 0)
$(SUITESPARSE_OBJ_SOURCE): | $(LAPACK_OBJ_TARGET)
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
	$(CC) -shared $(WHOLE_ARCHIVE) libsuitesparseconfig.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libsuitesparseconfig.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libsuitesparseconfig.$(SHLIB_EXT) $(build_shlibdir)/libsuitesparseconfig.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libamd.$(SHLIB_EXT) $(build_shlibdir)/libamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcolamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libcolamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcolamd.$(SHLIB_EXT) $(build_shlibdir)/libcolamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libcamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libcamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcamd.$(SHLIB_EXT) $(build_shlibdir)/libcamd.$(SHLIB_EXT) && \
	$(CC) -shared $(WHOLE_ARCHIVE) libccolamd.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libccolamd.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lsuitesparseconfig $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libccolamd.$(SHLIB_EXT) $(build_shlibdir)/libccolamd.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libcholmod.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libcholmod.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lcolamd -lamd -lcamd -lccolamd -lsuitesparseconfig $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libcholmod.$(SHLIB_EXT) $(build_shlibdir)/libcholmod.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libumfpack.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libumfpack.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lcholmod -lcolamd -lamd -lsuitesparseconfig $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libumfpack.$(SHLIB_EXT) $(build_shlibdir)/libumfpack.$(SHLIB_EXT) && \
	$(CXX) -shared $(WHOLE_ARCHIVE) libspqr.a $(NO_WHOLE_ARCHIVE) -o $(build_shlibdir)/libspqr.$(SHLIB_EXT) $(LDFLAGS) -L$(build_shlibdir) -lcholmod -lcolamd -lamd -lsuitesparseconfig $(LIBLAPACK) $(LIBBLAS) $(RPATH_ORIGIN) && \
	$(INSTALL_NAME_CMD)libspqr.$(SHLIB_EXT) $(build_shlibdir)/libspqr.$(SHLIB_EXT)

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
SUITESPARSE_LIB = -L$(build_shlibdir) -lcholmod -lumfpack -lspqr $(RPATH_ORIGIN)
$(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT):  $(SUITESPARSE_OBJ_TARGET)
endif

$(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT): SuiteSparse_wrapper.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -O2 -shared $(fPIC) $(SUITESPARSE_INC) $< -o $@ $(SUITESPARSE_LIB)
	$(INSTALL_NAME_CMD)libsuitesparse_wrapper.$(SHLIB_EXT) $@
	touch -c $@

clean-suitesparse-wrapper:
	-rm -f $(SUITESPARSE_OBJ_TARGET) $(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT)
distclean-suitesparse-wrapper: clean-suitesparse-wrapper

get-suitesparse-wrapper:
configure-suitesparse-wrapper:
compile-suitesparse-wrapper:
check-suitesparse-wrapper:
install-suitesparse-wrapper: $(build_shlibdir)/libsuitesparse_wrapper.$(SHLIB_EXT)


## UNWIND ##

LIBUNWIND_TARGET_OBJ = $(build_libdir)/libunwind.a
LIBUNWIND_TARGET_SOURCE = libunwind-$(UNWIND_VER)/src/.libs/libunwind.a
LIBUNWIND_CFLAGS = $(CFLAGS) -U_FORTIFY_SOURCE $(fPIC)
LIBUNWIND_CPPFLAGS = $(CPPFLAGS)

libunwind-$(UNWIND_VER).tar.gz:
	$(JLDOWNLOAD) $@ http://download.savannah.gnu.org/releases/libunwind/$@
libunwind-$(UNWIND_VER)/configure: libunwind-$(UNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	$(TAR) xfz $<
	cd libunwind-$(UNWIND_VER) && patch -p1 < ../libunwind.patch
	touch -c $@
libunwind-$(UNWIND_VER)/config.status: libunwind-$(UNWIND_VER)/configure
	cd libunwind-$(UNWIND_VER) && \
	./configure $(CONFIGURE_COMMON) CPPFLAGS="$(LIBUNWIND_CPPFLAGS)" CFLAGS="$(LIBUNWIND_CFLAGS)" --disable-shared --disable-minidebuginfo
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
	$(MAKE) install -C libunwind-$(UNWIND_VER) $(MAKE_COMMON)
ifeq ($(ARCH), ppc64)
	# workaround for configure script bug
	mv $(build_prefix)/lib64/libunwind*.a $(build_libdir)
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

## OS X Unwind ##

OSXUNWIND_FLAGS = ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) CFLAGS="-ggdb3 -O0" CXXFLAGS="$(CXXFLAGS) -ggdb3 -O0" SFLAGS="-ggdb3" LDFLAGS="-Wl,-macosx_version_min,10.7"

OSXUNWIND_OBJ_TARGET = $(build_shlibdir)/libosxunwind.$(SHLIB_EXT)
OSXUNWIND_OBJ_SOURCE = libosxunwind-$(OSXUNWIND_VER)/libosxunwind.$(SHLIB_EXT)

libosxunwind-$(OSXUNWIND_VER).tar.gz:
	$(JLDOWNLOAD) $@ https://github.com/JuliaLang/libosxunwind/archive/v$(OSXUNWIND_VER).tar.gz

libosxunwind-$(OSXUNWIND_VER)/Makefile: libosxunwind-$(OSXUNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	$(TAR) xfz $<
	touch -c $@

$(OSXUNWIND_OBJ_SOURCE): libosxunwind-$(OSXUNWIND_VER)/Makefile
	$(MAKE) -C libosxunwind-$(OSXUNWIND_VER) $(OSXUNWIND_FLAGS)
	touch -c $@
$(OSXUNWIND_OBJ_TARGET): $(OSXUNWIND_OBJ_SOURCE) | $(build_shlibdir)
	cp libosxunwind-$(OSXUNWIND_VER)/libosxunwind.a $(build_libdir)/libosxunwind.a
	cp $< $@
	cp -R libosxunwind-$(OSXUNWIND_VER)/include/* $(build_includedir)
	$(INSTALL_NAME_CMD)libosxunwind.$(SHLIB_EXT) $@

clean-osxunwind:
	-$(MAKE) -C libosxunwind-$(OSXUNWIND_VER) distclean $(OSXUNWIND_FLAGS)
	-rm $(OSXUNWIND_OBJ_TARGET)
distclean-osxunwind: clean-osxunwind

get-osxunwind: libosxunwind-$(OSXUNWIND_VER)/Makefile
configure-osxunwind: get-osxunwind
compile-osxunwind: $(OSXUNWIND_OBJ_SOURCE)
check-osxunwind: compile-osxunwind
install-osxunwind: $(OSXUNWIND_OBJ_TARGET)

## GMP ##

GMP_SRC_TARGET = gmp-$(GMP_VER)/.libs/libgmp.$(SHLIB_EXT)
GMP_OBJ_TARGET = $(build_shlibdir)/libgmp.$(SHLIB_EXT)

gmp-$(GMP_VER).tar.bz2:
	$(JLDOWNLOAD) $@ https://gmplib.org/download/gmp/$@
gmp-$(GMP_VER)/configure: gmp-$(GMP_VER).tar.bz2
	$(JLCHECKSUM) $<
	$(TAR) jxf $<
ifeq ($(OS), Darwin)
	cd gmp-$(GMP_VER) && patch -p1 < ../gmp_6.0.0_osx.patch
endif
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
$(GMP_OBJ_TARGET): $(GMP_SRC_TARGET) gmp-$(GMP_VER)/checked | $(build_shlibdir) $(build_includedir)
	$(INSTALL_M) gmp-$(GMP_VER)/.libs/libgmp.*$(SHLIB_EXT)* $(build_shlibdir)
	$(INSTALL_F) gmp-$(GMP_VER)/gmp.h $(build_includedir)
	$(INSTALL_NAME_CMD)libgmp.$(SHLIB_EXT) $@

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
MPFR_OBJ_TARGET = $(build_shlibdir)/libmpfr.$(SHLIB_EXT)
ifeq ($(OS),Darwin)
MPFR_CHECK_MFLAGS = LDFLAGS="-Wl,-rpath,'$(build_libdir)'"
endif

mpfr-$(MPFR_VER).tar.bz2:
	$(JLDOWNLOAD) $@ http://www.mpfr.org/mpfr-current/$@
mpfr-$(MPFR_VER)/configure: mpfr-$(MPFR_VER).tar.bz2
	$(JLCHECKSUM) $<
	$(TAR) jxf $<
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
	$(MAKE) -C mpfr-$(MPFR_VER) $(LIBTOOL_CCLD) install $(MAKE_COMMON)
	$(INSTALL_NAME_CMD)libmpfr.$(SHLIB_EXT) $@
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

## patchelf ##

PATCHELF_SOURCE = patchelf-$(PATCHELF_VER)/src/patchelf
PATCHELF_TARGET = $(build_bindir)/patchelf

compile-patchelf: install-patchelf
install-patchelf: $(PATCHELF_TARGET)

patchelf-$(PATCHELF_VER).tar.gz:
	$(JLDOWNLOAD) $@ https://ia601003.us.archive.org/29/items/julialang/mirror/patchelf-$(PATCHELF_VER).tar.gz
patchelf-$(PATCHELF_VER)/configure: patchelf-$(PATCHELF_VER).tar.gz
	$(JLCHECKSUM) $<
	$(TAR) zxf $<
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
	$(MAKE) -C patchelf-$(PATCHELF_VER) install $(MAKE_COMMON)
	touch -c $@

clean-patchelf:
	-$(MAKE) -C patchelf-$(PATCHELF_VER) clean
	-rm -f $(PATCHELF_OBJ_TARGET)
distclean-patchelf:
	-rm -rf patchelf-$(PATCHELF_VER).tar.gz patchelf-$(PATCHELF_VER)

get-patchelf: patchelf-$(PATCHELF_VER).tar.gz
configure-patchelf: patchelf-$(PATCHELF_VER)/config.status
compile-patchelf: $(PATCHELF_SOURCE)
check-patchelf: patchelf-$(PATCHELF_VER)/checked
install-patchelf: $(PATCHELF_TARGET)

## Git
# only used for the mac binaries in contrib/mac/app/Makefile

GIT_SOURCE = git-$(GIT_VER)/src/git
GIT_TARGET = $(build_prefix)/git

git-$(GIT_VER).tar.gz:
	$(JLDOWNLOAD) $@ https://www.kernel.org/pub/software/scm/git/git-$(GIT_VER).tar.gz
git-$(GIT_VER)/configure: git-$(GIT_VER).tar.gz
	$(JLCHECKSUM) $<
	$(TAR) zxf $<
	touch -c $@
git-$(GIT_VER)/config.status: git-$(GIT_VER)/configure
	cd git-$(GIT_VER) && \
	./configure $(CONFIGURE_COMMON) --bindir="$(build_libexecdir)"
	touch -c $@
$(GIT_SOURCE): git-$(GIT_VER)/config.status
	$(MAKE) -C git-$(GIT_VER)
	touch -c $@
git-$(GIT_VER)/checked: $(GIT_SOURCE)
	echo 1 > $@
$(GIT_TARGET): $(GIT_SOURCE) git-$(GIT_VER)/checked
	$(MAKE) -C git-$(GIT_VER) install NO_INSTALL_HARDLINKS=1 $(MAKE_COMMON)
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

## virtualenv

VIRTUALENV_SOURCE = virtualenv-$(VIRTUALENV_VER)/virtualenv.py
VIRTUALENV_TARGET = julia-env

virtualenv-$(VIRTUALENV_VER).tar.gz:
	$(JLDOWNLOAD) $@ https://pypi.python.org/packages/source/v/virtualenv/$@
$(VIRTUALENV_SOURCE): virtualenv-$(VIRTUALENV_VER).tar.gz
	$(JLCHECKSUM) $<
	$(TAR) zxf $<
	touch -c $@
$(VIRTUALENV_TARGET): $(VIRTUALENV_SOURCE)
	"$(shell ./find_python2)" $< $@
	touch -c $@

clean-virtualenv:
	-$(MAKE) -C virtualenv-$(VIRTUALENV_VER) clean
	-rm -f $(VIRTUALENV_TARGET)
distclean-virtualenv:
	-rm -rf virtualenv-$(VIRTUALENV_VER).tar.gz virtualenv-$(VIRTUALENV_VER)

get-virtualenv: virtualenv-$(VIRTUALENV_VER).tar.gz
configure-virtualenv: get-virtualenv
compile-virtualenv: $(VIRTUALENV_SOURCE)
check-virtualenv: compile-virtualenv
install-virtualenv: $(VIRTUALENV_TARGET)

## libgit2

LIBGIT2_OBJ_SOURCE = libgit2-$(LIBGIT2_VER)/build/libgit2.$(SHLIB_EXT)
LIBGIT2_OBJ_TARGET = $(build_shlibdir)/libgit2.$(SHLIB_EXT)

LIBGIT2_OPTS = $(CMAKE_COMMON) -DTHREADSAFE=ON
ifeq ($(OS),WINNT)
LIBGIT2_OPTS += -DWIN32=ON -DMINGW=ON -DUSE_SSH=OFF -DCMAKE_SYSTEM_NAME=Windows
ifeq ($(BUILD_OS),WINNT)
LIBGIT2_OPTS += -G"MSYS Makefiles"
else
LIBGIT2_OPTS += -DBUILD_CLAR=OFF -DCMAKE_RC_COMPILER=`which $(CROSS_COMPILE)windres`
LIBGIT2_OPTS += -DCMAKE_FIND_ROOT_PATH=/usr/$(XC_HOST) -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY
endif
endif

libgit2-$(LIBGIT2_VER).tar.gz:
	$(JLDOWNLOAD) $@ https://github.com/libgit2/libgit2/archive/v$(LIBGIT2_VER).tar.gz
libgit2-$(LIBGIT2_VER)/CMakeLists.txt: libgit2-$(LIBGIT2_VER).tar.gz
	$(JLCHECKSUM) $<
ifeq ($(BUILD_OS),WINNT)
	$(TAR) --exclude=libgit2-$(LIBGIT2_VER)/tests/resources/template/hooks/link.sample -zxf $<
else
	$(TAR) zxf $<
endif
	cd libgit2-$(LIBGIT2_VER) && patch -p1 < ../libgit2-0.21.3-mingw.patch
	touch -c $@
libgit2-$(LIBGIT2_VER)/build/Makefile: libgit2-$(LIBGIT2_VER)/CMakeLists.txt
	mkdir -p libgit2-$(LIBGIT2_VER)/build
	cd libgit2-$(LIBGIT2_VER)/build/ && \
	$(CMAKE) .. $(LIBGIT2_OPTS)
	touch -c $@
$(LIBGIT2_OBJ_SOURCE): libgit2-$(LIBGIT2_VER)/build/Makefile
	$(MAKE) -C libgit2-$(LIBGIT2_VER)/build
	touch -c $@
libgit2-$(LIBGIT2_VER)/build/checked: $(LIBGIT2_OBJ_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C libgit2-$(LIBGIT2_VER)/build test
endif
	echo 1 > $@
$(LIBGIT2_OBJ_TARGET): $(LIBGIT2_OBJ_SOURCE) | $(build_shlibdir)
	cp $< $@

clean-libgit2:
	-rm -rf libgit2-$(LIBGIT2_VER)/build/
	-rm -f $(LIBGIT2_OBJ_TARGET)
distclean-libgit2:
	-rm -rf libgit2-$(LIBGIT2_VER).tar.gz libgit2-$(LIBGIT2_VER).zip libgit2-$(LIBGIT2_VER)

get-libgit2: libgit2-$(LIBGIT2_VER).tar.gz
configure-libgit2: libgit2-$(LIBGIT2_VER)/build/Makefile
compile-libgit2: $(LIBGIT2_OBJ_SOURCE)
check-libgit2: libgit2-$(LIBGIT2_VER)/build/checked
install-libgit2: $(LIBGIT2_OBJ_TARGET)

## phony targets ##

.PHONY: default compile install cleanall distcleanall \
	get-* configure-* compile-* check-* install-* \
	clean-* distclean-* reinstall-* update-llvm
