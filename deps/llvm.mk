## LLVM ##
include $(SRCDIR)/llvm-ver.make

ifneq ($(USE_BINARYBUILDER_LLVM), 1)
LLVM_GIT_URL ?= https://github.com/llvm/llvm-project.git

ifeq ($(BUILD_LLDB), 1)
BUILD_LLVM_CLANG := 1
# because it's a build requirement
endif

ifeq ($(USE_POLLY),1)
ifeq ($(USE_SYSTEM_LLVM),0)
ifneq ($(LLVM_VER),svn)
$(error USE_POLLY=1 requires LLVM_VER=svn)
endif
endif
endif

# for Monorepo
LLVM_ENABLE_PROJECTS :=
ifeq ($(BUILD_LLVM_CLANG), 1)
LLVM_ENABLE_PROJECTS := $(LLVM_ENABLE_PROJECTS);clang;compiler-rt
endif
ifeq ($(USE_POLLY), 1)
LLVM_ENABLE_PROJECTS := $(LLVM_ENABLE_PROJECTS);polly
endif
ifeq ($(BUILD_LLDB), 1)
LLVM_ENABLE_PROJECTS := $(LLVM_ENABLE_PROJECTS);lldb
endif

include $(SRCDIR)/llvm-options.mk
LLVM_LIB_FILE := libLLVMCodeGen.a

LLVM_TAR_EXT:=$(LLVM_VER).src.tar.xz

ifneq ($(LLVM_VER),svn)
LLVM_TAR:=$(SRCCACHE)/llvm-$(LLVM_TAR_EXT)

ifeq ($(BUILD_LLDB),1)
LLVM_LLDB_TAR:=$(SRCCACHE)/lldb-$(LLVM_TAR_EXT)
endif # BUILD_LLDB

ifeq ($(BUILD_LLVM_CLANG),1)
LLVM_CLANG_TAR:=$(SRCCACHE)/cfe-$(LLVM_TAR_EXT)
LLVM_COMPILER_RT_TAR:=$(SRCCACHE)/compiler-rt-$(LLVM_TAR_EXT)
else
LLVM_CLANG_TAR:=
LLVM_COMPILER_RT_TAR:=
LLVM_LIBCXX_TAR:=
endif # BUILD_LLVM_CLANG

ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LLVM_LIBCXX_TAR:=$(SRCCACHE)/libcxx-$(LLVM_TAR_EXT)
endif
endif # LLVM_VER != svn

# Figure out which targets to build
LLVM_TARGETS := host;NVPTX;AMDGPU;WebAssembly

LLVM_CFLAGS :=
LLVM_CXXFLAGS :=
LLVM_CPPFLAGS :=
LLVM_LDFLAGS :=
LLVM_CMAKE :=

# MONOREPO
ifeq ($(LLVM_VER),svn)
LLVM_CMAKE += -DLLVM_ENABLE_PROJECTS="$(LLVM_ENABLE_PROJECTS)"
endif

# Allow adding LLVM specific flags
LLVM_CFLAGS += $(CFLAGS)
LLVM_CXXFLAGS += $(CXXFLAGS)
LLVM_CPPFLAGS += $(CPPFLAGS)
LLVM_LDFLAGS += $(LDFLAGS)
LLVM_CMAKE += -DLLVM_TARGETS_TO_BUILD:STRING="$(LLVM_TARGETS)" -DCMAKE_BUILD_TYPE="$(LLVM_CMAKE_BUILDTYPE)"
LLVM_CMAKE += -DLLVM_ENABLE_ZLIB=OFF -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_HOST_TRIPLE="$(or $(XC_HOST),$(BUILD_MACHINE))"
ifeq ($(USE_POLLY_ACC),1)
LLVM_CMAKE += -DPOLLY_ENABLE_GPGPU_CODEGEN=ON
endif
LLVM_CMAKE += -DLLVM_TOOLS_INSTALL_DIR=$(shell $(JULIAHOME)/contrib/relative_path.sh $(build_prefix) $(build_depsbindir))
LLVM_CMAKE += -DLLVM_UTILS_INSTALL_DIR=$(shell $(JULIAHOME)/contrib/relative_path.sh $(build_prefix) $(build_depsbindir))
LLVM_CMAKE += -DLLVM_INCLUDE_UTILS=ON -DLLVM_INSTALL_UTILS=ON
LLVM_CMAKE += -DLLVM_BINDINGS_LIST="" -DLLVM_INCLUDE_DOCS=Off -DLLVM_ENABLE_TERMINFO=Off -DHAVE_HISTEDIT_H=Off -DHAVE_LIBEDIT=Off
ifeq ($(LLVM_ASSERTIONS), 1)
LLVM_CMAKE += -DLLVM_ENABLE_ASSERTIONS:BOOL=ON
endif # LLVM_ASSERTIONS
ifeq ($(LLVM_DEBUG), 1)
ifeq ($(OS), WINNT)
LLVM_CXXFLAGS += -Wa,-mbig-obj
endif # OS == WINNT
endif # LLVM_DEBUG
ifeq ($(OS), WINNT)
LLVM_CPPFLAGS += -D__USING_SJLJ_EXCEPTIONS__ -D__CRT__NO_INLINE
ifneq ($(BUILD_OS),WINNT)
LLVM_CMAKE += -DCROSS_TOOLCHAIN_FLAGS_NATIVE=-DCMAKE_TOOLCHAIN_FILE=$(SRCDIR)/NATIVE.cmake
endif # BUILD_OS != WINNT
endif # OS == WINNT
ifeq ($(OS), emscripten)
LLVM_CMAKE += -DCMAKE_TOOLCHAIN_FILE=$(EMSCRIPTEN)/cmake/Modules/Platform/Emscripten.cmake -DCROSS_TOOLCHAIN_FLAGS_NATIVE=-DCMAKE_TOOLCHAIN_FILE=$(SRCDIR)/NATIVE.cmake -DLLVM_INCLUDE_TOOLS=OFF -DLLVM_BUILD_TOOLS=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_ENABLE_THREADS=OFF -DLLVM_BUILD_UTILS=OFF
endif # OS == emscripten
ifeq ($(USE_LLVM_SHLIB),1)
# NOTE: we could also --disable-static here (on the condition we link tools
#       against libLLVM) but there doesn't seem to be a CMake counterpart option
LLVM_CMAKE += -DLLVM_BUILD_LLVM_DYLIB:BOOL=ON -DLLVM_LINK_LLVM_DYLIB:BOOL=ON
endif
ifeq ($(USE_INTEL_JITEVENTS), 1)
LLVM_CMAKE += -DLLVM_USE_INTEL_JITEVENTS:BOOL=ON
endif # USE_INTEL_JITEVENTS

ifeq ($(USE_OPROFILE_JITEVENTS), 1)
LLVM_CMAKE += -DLLVM_USE_OPROFILE:BOOL=ON
endif # USE_OPROFILE_JITEVENTS

ifeq ($(USE_PERF_JITEVENTS), 1)
	LLVM_CMAKE += -DLLVM_USE_PERF:BOOL=ON
endif # USE_PERF_JITEVENTS

ifeq ($(BUILD_LLDB),1)
ifeq ($(USECLANG),0)
LLVM_CXXFLAGS += -std=c++0x
endif # USECLANG
ifeq ($(LLDB_DISABLE_PYTHON),1)
LLVM_CXXFLAGS += -DLLDB_DISABLE_PYTHON
LLVM_CMAKE += -DLLDB_DISABLE_PYTHON=ON
endif # LLDB_DISABLE_PYTHON
endif # BUILD_LLDB

ifneq (,$(filter $(ARCH), powerpc64le ppc64le))
LLVM_CXXFLAGS += -mminimal-toc
endif

ifeq ($(LLVM_SANITIZE),1)
ifeq ($(SANITIZE_MEMORY),1)
LLVM_CFLAGS += -fsanitize=memory -fsanitize-memory-track-origins
LLVM_LDFLAGS += -fsanitize=memory -fsanitize-memory-track-origins
LLVM_CXXFLAGS += -fsanitize=memory -fsanitize-memory-track-origins
LLVM_CMAKE += -DLLVM_USE_SANITIZER="MemoryWithOrigins"
else
LLVM_CFLAGS += -fsanitize=address
LLVM_LDFLAGS += -fsanitize=address
LLVM_CXXFLAGS += -fsanitize=address
LLVM_CMAKE += -DLLVM_USE_SANITIZER="Address"
endif
endif # LLVM_SANITIZE

ifeq ($(LLVM_LTO),1)
LLVM_CPPFLAGS += -flto
LLVM_LDFLAGS += -flto
endif # LLVM_LTO

ifeq ($(fPIC),)
LLVM_CMAKE += -DLLVM_ENABLE_PIC=OFF
endif

ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LLVM_LDFLAGS += -Wl,-rpath,$(build_libdir)
LLVM_CPPFLAGS += -I$(build_includedir)
# We don't want to link to libc++ while trying to build it, so we define these
# flags separately so that we can still pass them to the main LLVM build
LLVM_LIBCXX_LDFLAGS := -lc++ -lc++abi
ifeq ($(USEICC),1)
LLVM_LDFLAGS += -no_cpprt
endif # USEICC
else
LLVM_LIBCXX_LDFLAGS :=
endif # BUILD_CUSTOM_LIBCXX

LLVM_CMAKE += -DCMAKE_C_FLAGS="$(LLVM_CPPFLAGS) $(LLVM_CFLAGS)" \
	-DCMAKE_CXX_FLAGS="$(LLVM_CPPFLAGS) $(LLVM_CXXFLAGS)"

ifeq ($(BUILD_LLVM_CLANG),0)
# block default building of Clang
LLVM_CMAKE += -DLLVM_TOOL_CLANG_BUILD=OFF
LLVM_CMAKE += -DLLVM_TOOL_COMPILER_RT_BUILD=OFF
endif
ifeq ($(BUILD_LLDB),0)
# block default building of lldb
LLVM_CMAKE += -DLLVM_TOOL_LLDB_BUILD=OFF
endif

ifneq ($(LLVM_VER),svn)
ifeq (,$(findstring rc,$(LLVM_VER)))
ifeq ($(shell [ x"$(LLVM_VER)" = x"8.0.1" ]; echo $$?),0)
LLVM_SRC_URL := https://github.com/llvm/llvm-project/releases/download/llvmorg-$(LLVM_VER)
else ifeq ($(shell [ x"$(LLVM_VER)" = x"9.0.1" ]; echo $$?),0)
LLVM_SRC_URL := https://github.com/llvm/llvm-project/releases/download/llvmorg-$(LLVM_VER)
else
LLVM_SRC_URL := http://releases.llvm.org/$(LLVM_VER)
endif
else
LLVM_VER_SPLIT := $(subst rc, ,$(LLVM_VER))
LLVM_SRC_URL := https://prereleases.llvm.org/$(word 1,$(LLVM_VER_SPLIT))/rc$(word 2,$(LLVM_VER_SPLIT))
endif

ifneq ($(LLVM_CLANG_TAR),)
$(LLVM_CLANG_TAR): | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(LLVM_SRC_URL)/$(notdir $@)
endif
ifneq ($(LLVM_COMPILER_RT_TAR),)
$(LLVM_COMPILER_RT_TAR): | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(LLVM_SRC_URL)/$(notdir $@)
endif

ifneq ($(LLVM_LIBCXX_TAR),)
$(LLVM_LIBCXX_TAR): | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(LLVM_SRC_URL)/$(notdir $@)
endif
ifneq ($(LLVM_VER),svn)
$(LLVM_TAR): | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(LLVM_SRC_URL)/$(notdir $@)
endif

ifneq ($(LLVM_LLDB_TAR),)
$(LLVM_LLDB_TAR): | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(LLVM_SRC_URL)/$(notdir $@)
endif
ifeq ($(BUILD_LLDB),1)
$(LLVM_SRC_DIR)/tools/lldb:
$(LLVM_SRC_DIR)/source-extracted: $(LLVM_SRC_DIR)/tools/lldb
endif
endif

# LLDB still relies on plenty of python 2.x infrastructure, without checking
llvm_python_location=$(shell /usr/bin/env python2 -c 'import sys; print(sys.executable)')
llvm_python_workaround=$(SRCCACHE)/python2_path
$(llvm_python_workaround):
	mkdir -p $@
	-python -c 'import sys; sys.exit(not sys.version_info > (3, 0))' && \
	/usr/bin/env python2 -c 'import sys; sys.exit(not sys.version_info < (3, 0))' && \
	ln -sf $(llvm_python_location) "$@/python" && \
	ln -sf $(llvm_python_location)-config "$@/python-config"

ifeq ($(BUILD_CUSTOM_LIBCXX),1)

# Take a snapshot of the CMake flags before linking to -lc++ and -lc++abi
# These are added to the LLVM CMake flags further down
LLVM_CMAKE_LIBCXX := $(LLVM_CMAKE) \
	-DCMAKE_EXE_LINKER_FLAGS="$(LLVM_LDFLAGS)" \
	-DCMAKE_SHARED_LINKER_FLAGS="$(LLVM_LDFLAGS)"

ifeq ($(USEICC),1)
LIBCXX_EXTRA_FLAGS := -Bstatic -lirc -Bdynamic
endif

# These libraries require unwind.h from the libunwind dependency
ifeq ($(USE_SYSTEM_LIBUNWIND),0)
ifeq ($(OS),Darwin)
BUILT_UNWIND := $(build_prefix)/manifest/osxunwind
else
BUILT_UNWIND := $(build_prefix)/manifest/unwind
endif # Darwin
else
BUILT_UNWIND :=
endif # Building libunwind

$(LIBCXX_ROOT_DIR)/libcxx: $(LLVM_LIBCXX_TAR) | $(LLVM_SRC_DIR)/source-extracted
$(LIBCXX_ROOT_DIR)/libcxxabi: $(LLVM_LIBCXXABI_TAR) | $(LLVM_SRC_DIR)/source-extracted
$(LLVM_BUILD_DIR)/libcxx-build/Makefile: | $(LIBCXX_ROOT_DIR)/libcxx $(LIBCXX_ROOT_DIR)/libcxxabi $(BUILT_UNWIND)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
		$(CMAKE) -G "Unix Makefiles" $(CMAKE_COMMON) $(LLVM_CMAKE_LIBCXX) -DLIBCXX_CXX_ABI=libcxxabi -DLIBCXX_CXX_ABI_INCLUDE_PATHS="$(LIBCXX_ROOT_DIR)/libcxxabi/include" $(LIBCXX_ROOT_DIR)/libcxx -DCMAKE_SHARED_LINKER_FLAGS="$(LDFLAGS) -L$(build_libdir) $(LIBCXX_EXTRA_FLAGS)"
$(LLVM_BUILD_DIR)/libcxxabi-build/Makefile: | $(LIBCXX_ROOT_DIR)/libcxxabi $(LIBCXX_ROOT_DIR)/libcxx $(BUILT_UNWIND)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
		$(CMAKE) -G "Unix Makefiles" $(CMAKE_COMMON) $(LLVM_CMAKE_LIBCXX) -DLLVM_ABI_BREAKING_CHECKS="WITH_ASSERTS" -DLLVM_PATH="$(LLVM_SRC_DIR)" $(LIBCXX_ROOT_DIR)/libcxxabi -DLIBCXXABI_CXX_ABI_LIBRARIES="$(LIBCXX_EXTRA_FLAGS)" -DCMAKE_CXX_FLAGS="$(LLVM_CPPFLAGS) $(LLVM_CXXFLAGS) -std=c++11"
$(LLVM_BUILD_DIR)/libcxxabi-build/lib/libc++abi.so.1.0: $(LLVM_BUILD_DIR)/libcxxabi-build/Makefile $(LIBCXX_ROOT_DIR)/libcxxabi/.git/HEAD
	$(MAKE) -C $(LLVM_BUILD_DIR)/libcxxabi-build
	touch -c $@
$(build_libdir)/libc++abi.so.1.0: $(LLVM_BUILD_DIR)/libcxxabi-build/lib/libc++abi.so.1.0
	$(MAKE) -C $(LLVM_BUILD_DIR)/libcxxabi-build install
	touch -c $@
	# Building this library installs these headers, which breaks other dependencies
	-rm -rf $(build_includedir)/c++
$(LLVM_BUILD_DIR)/libcxx-build/lib/libc++.so.1.0: $(build_libdir)/libc++abi.so.1.0 $(LLVM_BUILD_DIR)/libcxx-build/Makefile $(LIBCXX_ROOT_DIR)/libcxx/.git/HEAD
	$(MAKE) -C $(LLVM_BUILD_DIR)/libcxx-build
$(build_libdir)/libc++.so.1.0: $(LLVM_BUILD_DIR)/libcxx-build/lib/libc++.so.1.0
	$(MAKE) -C $(LLVM_BUILD_DIR)/libcxx-build install
	touch -c $@
	# Building this library installs these headers, which breaks other dependencies
	-rm -rf $(build_includedir)/c++
get-libcxx: $(LIBCXX_ROOT_DIR)/libcxx
get-libcxxabi: $(LIBCXX_ROOT_DIR)/libcxxabi
install-libcxxabi: $(build_libdir)/libc++abi.so.1.0
install-libcxx: $(build_libdir)/libc++.so.1.0
endif # BUILD_CUSTOM_LIBCXX

# We want to be able to clean without having to pass BUILD_CUSTOM_LIBCXX=1, so define these
# outside of the conditional above, can't use `LIBCXX_ROOT_DIR` since that might come from
# the monorepo.
clean-libcxx:
	-$(MAKE) -C $(LLVM_BUILD_DIR)/libcxx-build clean
clean-libcxxabi:
	-$(MAKE) -C $(LLVM_BUILD_DIR)/libcxxabi-build clean
distclean-libcxx:
	-rm -rf $(LLVM_LIBCXX_TAR) $(LLVM_SRC_DIR)/projects/libcxx $(LLVM_BUILD_DIR)/libcxx-build
distclean-libcxxabi:
	-rm -rf $(LLVM_LIBCXXABI_TAR) $(LLVM_SRC_DIR)/projects/libcxxabi $(LLVM_BUILD_DIR)/libcxxabi-build

# We want to ensure that the libcxx linking flags don't get passed to the libcxx build, since it will
# error on a fresh build
LLVM_CMAKE += -DCMAKE_EXE_LINKER_FLAGS="$(LLVM_LDFLAGS) $(LLVM_LIBCXX_LDFLAGS)" \
	-DCMAKE_SHARED_LINKER_FLAGS="$(LLVM_LDFLAGS) $(LLVM_LIBCXX_LDFLAGS)"

# change the SONAME of Julia's private LLVM
# i.e. libLLVM-6.0jl.so
# see #32462
LLVM_CMAKE += -DLLVM_VERSION_SUFFIX:STRING="jl"

ifeq ($(BUILD_CUSTOM_LIBCXX),1)
LIBCXX_DEPENDENCY := $(build_libdir)/libc++abi.so.1.0 $(build_libdir)/libc++.so.1.0
get-llvm: get-libcxx get-libcxxabi
endif

$(LLVM_SRC_DIR)/source-extracted: | $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR) $(LLVM_LIBCXX_TAR) $(LLVM_LLDB_TAR)
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
	-rm -rf $(LLVM_SRC_DIR)
ifneq ($(LLVM_VER),svn)
	mkdir -p $(LLVM_SRC_DIR)
	$(TAR) -C $(LLVM_SRC_DIR) --strip-components 1 -xf $(LLVM_TAR)
else
	([ ! -d $(LLVM_MONOSRC_DIR) ] && \
		git clone $(LLVM_GIT_URL) $(LLVM_MONOSRC_DIR) ) || \
		(cd $(LLVM_MONOSRC_DIR) && \
		git pull --ff-only)
ifneq ($(LLVM_GIT_VER),)
	(cd $(LLVM_MONOSRC_DIR) && \
		git checkout $(LLVM_GIT_VER))
endif # LLVM_GIT_VER
	# Debug output only. Disable pager and ignore error.
	(cd $(LLVM_SRC_DIR) && \
		git show HEAD --stat | cat) || true
endif # LLVM_VER
ifneq ($(LLVM_VER),svn)
ifneq ($(LLVM_CLANG_TAR),)
	mkdir -p $(LLVM_SRC_DIR)/tools/clang
	$(TAR) -C $(LLVM_SRC_DIR)/tools/clang --strip-components 1 -xf $(LLVM_CLANG_TAR)
endif # LLVM_CLANG_TAR
ifneq ($(LLVM_COMPILER_RT_TAR),)
	mkdir -p $(LLVM_SRC_DIR)/projects/compiler-rt
	$(TAR) -C $(LLVM_SRC_DIR)/projects/compiler-rt --strip-components 1 -xf $(LLVM_COMPILER_RT_TAR)
endif # LLVM_COMPILER_RT_TAR
ifneq ($(LLVM_LLDB_TAR),)
	mkdir -p $(LLVM_SRC_DIR)/tools/lldb
	$(TAR) -C $(LLVM_SRC_DIR)/tools/lldb --strip-components 1 -xf $(LLVM_LLDB_TAR)
endif # LLVM_LLDB_TAR
endif # LLVM_VER
	# touch some extra files to ensure bisect works pretty well
	touch -c $(LLVM_SRC_DIR).extracted
	touch -c $(LLVM_SRC_DIR)/configure
	touch -c $(LLVM_SRC_DIR)/CMakeLists.txt
	echo 1 > $@

# Apply version-specific LLVM patches sequentially
LLVM_PATCH_PREV :=
define LLVM_PATCH
$$(LLVM_SRC_DIR)/$1.patch-applied: $$(LLVM_SRC_DIR)/source-extracted | $$(SRCDIR)/patches/$1.patch $$(LLVM_PATCH_PREV)
	cd $$(LLVM_SRC_DIR) && patch -p1 < $$(SRCDIR)/patches/$1.patch
	echo 1 > $$@
# declare that applying any patch must re-run the compile step
$$(LLVM_BUILDDIR_withtype)/build-compiled: $$(LLVM_SRC_DIR)/$1.patch-applied
LLVM_PATCH_PREV := $$(LLVM_SRC_DIR)/$1.patch-applied
endef

ifeq ($(LLVM_VER_SHORT),8.0)
$(eval $(call LLVM_PATCH,llvm-D27629-AArch64-large_model_6.0.1))
$(eval $(call LLVM_PATCH,llvm8-D34078-vectorize-fdiv))
$(eval $(call LLVM_PATCH,llvm-6.0-NVPTX-addrspaces)) # NVPTX -- warning: this fails check-llvm-codegen-nvptx
$(eval $(call LLVM_PATCH,llvm-7.0-D44650)) # mingw32 build fix
$(eval $(call LLVM_PATCH,llvm-6.0-DISABLE_ABI_CHECKS))
$(eval $(call LLVM_PATCH,llvm7-D50010-VNCoercion-ni))
$(eval $(call LLVM_PATCH,llvm-8.0-D50167-scev-umin))
$(eval $(call LLVM_PATCH,llvm7-windows-race))
$(eval $(call LLVM_PATCH,llvm-D57118-powerpc)) # remove for 9.0
$(eval $(call LLVM_PATCH,llvm8-WASM-addrspaces)) # WebAssembly
$(eval $(call LLVM_PATCH,llvm-exegesis-mingw)) # mingw build
$(eval $(call LLVM_PATCH,llvm-test-plugin-mingw)) # mingw build
$(eval $(call LLVM_PATCH,llvm-8.0-D66401-mingw-reloc)) # remove for 9.0
$(eval $(call LLVM_PATCH,llvm7-revert-D44485))
$(eval $(call LLVM_PATCH,llvm-8.0-D63688-wasm-isLocal)) # remove for 9.0
$(eval $(call LLVM_PATCH,llvm-8.0-D55758-tablegen-cond)) # remove for 9.0
$(eval $(call LLVM_PATCH,llvm-8.0-D59389-refactor-wmma)) # remove for 9.0
$(eval $(call LLVM_PATCH,llvm-8.0-D59393-mma-ptx63-fix)) # remove for 9.0
$(eval $(call LLVM_PATCH,llvm-8.0-D66657-codegen-degenerate)) # remove for 10.0
$(eval $(call LLVM_PATCH,llvm-8.0-D71495-vectorize-freduce)) # remove for 10.0
$(eval $(call LLVM_PATCH,llvm-8.0-D65174-limit-merge-stores)) # remove for 10.0
endif # LLVM_VER 8.0

ifeq ($(LLVM_VER_SHORT),9.0)
$(eval $(call LLVM_PATCH,llvm-D27629-AArch64-large_model_6.0.1))
$(eval $(call LLVM_PATCH,llvm8-D34078-vectorize-fdiv))
$(eval $(call LLVM_PATCH,llvm-6.0-NVPTX-addrspaces)) # NVPTX -- warning: this fails check-llvm-codegen-nvptx
$(eval $(call LLVM_PATCH,llvm-7.0-D44650)) # mingw32 build fix
$(eval $(call LLVM_PATCH,llvm9-D50010-VNCoercion-ni))
$(eval $(call LLVM_PATCH,llvm8-WASM-addrspaces)) # WebAssembly
$(eval $(call LLVM_PATCH,llvm-exegesis-mingw)) # mingw build
$(eval $(call LLVM_PATCH,llvm-test-plugin-mingw)) # mingw build
$(eval $(call LLVM_PATCH,llvm7-revert-D44485))
$(eval $(call LLVM_PATCH,llvm-8.0-D66657-codegen-degenerate)) # remove for 10.0
$(eval $(call LLVM_PATCH,llvm-8.0-D71495-vectorize-freduce)) # remove for 10.0
$(eval $(call LLVM_PATCH,llvm-D75072-SCEV-add-type))
$(eval $(call LLVM_PATCH,llvm-8.0-D65174-limit-merge-stores)) # remove for 10.0
endif # LLVM_VER 9.0


# Add a JL prefix to the version map. DO NOT REMOVE
ifneq ($(LLVM_VER), svn)
$(eval $(call LLVM_PATCH,llvm7-symver-jlprefix))
endif

# declare that all patches must be applied before running ./configure
$(LLVM_BUILDDIR_withtype)/build-configured: | $(LLVM_PATCH_PREV)

$(LLVM_BUILDDIR_withtype)/build-configured: $(LLVM_SRC_DIR)/source-extracted | $(llvm_python_workaround) $(LIBCXX_DEPENDENCY)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
		export PATH=$(llvm_python_workaround):"$$PATH" && \
		$(CMAKE) $(LLVM_SRC_DIR) $(CMAKE_GENERATOR_COMMAND) $(CMAKE_COMMON) $(LLVM_CMAKE) \
		|| { echo '*** To install a newer version of cmake, run contrib/download_cmake.sh ***' && false; }
	echo 1 > $@

$(LLVM_BUILDDIR_withtype)/build-compiled: $(LLVM_BUILDDIR_withtype)/build-configured | $(llvm_python_workaround)
	cd $(LLVM_BUILDDIR_withtype) && \
		export PATH=$(llvm_python_workaround):"$$PATH" && \
		$(if $(filter $(CMAKE_GENERATOR),make), \
		  $(MAKE), \
		  $(CMAKE) --build .)
	echo 1 > $@

$(LLVM_BUILDDIR_withtype)/build-checked: $(LLVM_BUILDDIR_withtype)/build-compiled | $(llvm_python_workaround)
ifeq ($(OS),$(BUILD_OS))
	cd $(LLVM_BUILDDIR_withtype) && \
		export PATH=$(llvm_python_workaround):"$$PATH" && \
		  $(CMAKE) --build . --target check
endif
	echo 1 > $@

$(build_prefix)/manifest/llvm: | $(llvm_python_workaround)

LLVM_INSTALL = \
	cd $1 && mkdir -p $2$$(build_depsbindir) && \
    cp -r $$(LLVM_SRC_DIR)/utils/lit $2$$(build_depsbindir)/ && \
    $$(CMAKE) -DCMAKE_INSTALL_PREFIX="$2$$(build_prefix)" -P cmake_install.cmake
ifeq ($(OS), WINNT)
LLVM_INSTALL += && cp $2$$(build_shlibdir)/LLVM.dll $2$$(build_depsbindir)
endif
ifeq ($(OS),Darwin)
# https://github.com/JuliaLang/julia/issues/29981
LLVM_INSTALL += && ln -s libLLVM.dylib $2$$(build_shlibdir)/libLLVM-$$(LLVM_VER_SHORT).dylib
endif

$(eval $(call staged-install,llvm,llvm-$$(LLVM_VER)/build_$$(LLVM_BUILDTYPE), \
	LLVM_INSTALL,,,))

clean-llvm: clean-libcxx clean-libcxxabi
	-rm $(LLVM_BUILDDIR_withtype)/build-configured $(LLVM_BUILDDIR_withtype)/build-compiled
	-$(MAKE) -C $(LLVM_BUILDDIR_withtype) clean

distclean-llvm: distclean-libcxx distclean-libcxxabi
	-rm -rf $(LLVM_TAR) $(LLVM_CLANG_TAR) \
		$(LLVM_COMPILER_RT_TAR) $(LLVM_LIBCXX_TAR) $(LLVM_LLDB_TAR) \
		$(LLVM_SRC_DIR) $(LLVM_BUILDDIR_withtype)


ifneq ($(LLVM_VER),svn)
get-llvm: $(LLVM_TAR) $(LLVM_CLANG_TAR) $(LLVM_COMPILER_RT_TAR) $(LLVM_LIBCXX_TAR) $(LLVM_LLDB_TAR)
else
get-llvm: $(LLVM_SRC_DIR)/source-extracted
endif
extract-llvm: $(LLVM_SRC_DIR)/source-extracted
configure-llvm: $(LLVM_BUILDDIR_withtype)/build-configured
compile-llvm: $(LLVM_BUILDDIR_withtype)/build-compiled
fastcheck-llvm: #none
check-llvm: $(LLVM_BUILDDIR_withtype)/build-checked
#todo: LLVM make check target is broken on julia.mit.edu (and really slow elsewhere)


ifeq ($(LLVM_VER),svn)
update-llvm:
	cd $(LLVM_MONOSRC_DIR) && \
		git pull --ff-only
endif
else # USE_BINARYBUILDER_LLVM
LLVM_BB_URL_BASE := https://github.com/JuliaBinaryWrappers/LLVM_jll.jl/releases/download/LLVM-v$(LLVM_VER)+$(LLVM_BB_REL)
ifneq ($(BINARYBUILDER_LLVM_ASSERTS), 1)
LLVM_BB_NAME := LLVM.v$(LLVM_VER)
else
LLVM_BB_NAME := LLVM.asserts.v$(LLVM_VER)
endif

$(eval $(call bb-install,llvm,LLVM,false,true))

endif # USE_BINARYBUILDER_LLVM
