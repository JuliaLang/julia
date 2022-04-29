## LLVM ##
include $(SRCDIR)/llvm-ver.make
include $(SRCDIR)/llvm-options.mk

ifneq ($(USE_BINARYBUILDER_LLVM), 1)
LLVM_GIT_URL:=https://github.com/JuliaLang/llvm-project.git
LLVM_TAR_URL=https://api.github.com/repos/JuliaLang/llvm-project/tarball/$1
$(eval $(call git-external,llvm,LLVM,CMakeLists.txt,,$(SRCCACHE)))

LLVM_BUILDDIR := $(BUILDDIR)/$(LLVM_SRC_DIR)
LLVM_BUILDDIR_withtype := $(LLVM_BUILDDIR)/build_$(LLVM_BUILDTYPE)

ifeq ($(BUILD_LLDB), 1)
BUILD_LLVM_CLANG := 1
# because it's a build requirement
endif

ifeq ($(BUILD_LIBCXX), 1)
BUILD_LLVM_CLANG := 1
# because it's a build requirement
endif

ifeq ($(USE_RV),1)
BUILD_LLVM_CLANG := 1
# because it's a build requirement
endif

# TODO: Add RV support back in
# ifneq ($(USE_RV),)
# LLVM_RV_GIT_URL ?= https://github.com/cdl-saarland/rv
# LLVM_RV_GIT_VER ?= release_90
# endif


# for Monorepo
LLVM_ENABLE_PROJECTS :=
LLVM_EXTERNAL_PROJECTS :=
LLVM_ENABLE_RUNTIMES :=
ifeq ($(BUILD_LLVM_CLANG), 1)
LLVM_ENABLE_PROJECTS := $(LLVM_ENABLE_PROJECTS);clang
LLVM_ENABLE_RUNTIMES := $(LLVM_ENABLE_RUNTIMES);compiler-rt
endif
ifeq ($(USE_POLLY), 1)
LLVM_ENABLE_PROJECTS := $(LLVM_ENABLE_PROJECTS);polly
endif
ifeq ($(BUILD_LLDB), 1)
LLVM_ENABLE_PROJECTS := $(LLVM_ENABLE_PROJECTS);lldb
endif
ifeq ($(USE_MLIR), 1)
LLVM_ENABLE_PROJECTS := $(LLVM_ENABLE_PROJECTS);mlir
endif
ifeq ($(USE_RV), 1)
LLVM_EXTERNAL_PROJECTS := $(LLVM_EXTERNAL_PROJECTS);rv
endif
ifeq ($(BUILD_LIBCXX), 1)
LLVM_ENABLE_RUNTIMES := $(LLVM_ENABLE_RUNTIMES);libcxx;libcxxabi
endif


LLVM_LIB_FILE := libLLVMCodeGen.a

# Figure out which targets to build
LLVM_TARGETS := host;NVPTX;AMDGPU;WebAssembly;BPF
LLVM_EXPERIMENTAL_TARGETS :=

LLVM_CFLAGS :=
LLVM_CXXFLAGS :=
LLVM_CPPFLAGS :=
LLVM_LDFLAGS :=
LLVM_CMAKE :=

LLVM_CMAKE += -DLLVM_ENABLE_PROJECTS="$(LLVM_ENABLE_PROJECTS)"
LLVM_CMAKE += -DLLVM_EXTERNAL_PROJECTS="$(LLVM_EXTERNAL_PROJECTS)"
LLVM_CMAKE += -DLLVM_ENABLE_RUNTIMES="$(LLVM_ENABLE_RUNTIMES)"

ifeq ($(USE_RV),1)
LLVM_CMAKE += -DLLVM_EXTERNAL_RV_SOURCE_DIR=$(LLVM_MONOSRC_DIR)/rv
LLVM_CMAKE += -DLLVM_CXX_STD=c++14
endif

# Allow adding LLVM specific flags
LLVM_CFLAGS += $(CFLAGS)
LLVM_CXXFLAGS += $(CXXFLAGS)
LLVM_CPPFLAGS += $(CPPFLAGS)
LLVM_LDFLAGS += $(LDFLAGS)
LLVM_CMAKE += -DLLVM_TARGETS_TO_BUILD:STRING="$(LLVM_TARGETS)" -DCMAKE_BUILD_TYPE="$(LLVM_CMAKE_BUILDTYPE)"
LLVM_CMAKE += -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD:STRING="$(LLVM_EXPERIMENTAL_TARGETS)"
LLVM_CMAKE += -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_HOST_TRIPLE="$(or $(XC_HOST),$(BUILD_MACHINE))"
LLVM_CMAKE += -DLLVM_ENABLE_ZLIB=ON -DZLIB_LIBRARY="$(build_prefix)/lib"
LLVM_CMAKE += -DCOMPILER_RT_ENABLE_IOS=OFF -DCOMPILER_RT_ENABLE_WATCHOS=OFF -DCOMPILER_RT_ENABLE_TVOS=OFF
ifeq ($(USE_POLLY_ACC),1)
LLVM_CMAKE += -DPOLLY_ENABLE_GPGPU_CODEGEN=ON
endif
LLVM_CMAKE += -DLLVM_TOOLS_INSTALL_DIR=$(call rel_path,$(build_prefix),$(build_depsbindir))
LLVM_CMAKE += -DLLVM_UTILS_INSTALL_DIR=$(call rel_path,$(build_prefix),$(build_depsbindir))
LLVM_CMAKE += -DLLVM_INCLUDE_UTILS=ON -DLLVM_INSTALL_UTILS=ON
LLVM_CMAKE += -DLLVM_BINDINGS_LIST="" -DLLVM_INCLUDE_DOCS=Off -DLLVM_ENABLE_TERMINFO=Off -DHAVE_HISTEDIT_H=Off -DHAVE_LIBEDIT=Off
ifeq ($(LLVM_ASSERTIONS), 1)
LLVM_CMAKE += -DLLVM_ENABLE_ASSERTIONS:BOOL=ON
endif # LLVM_ASSERTIONS
ifeq ($(OS), WINNT)
LLVM_CPPFLAGS += -D__USING_SJLJ_EXCEPTIONS__ -D__CRT__NO_INLINE
endif # OS == WINNT
ifneq ($(HOSTCC),$(CC))
LLVM_CMAKE += -DCROSS_TOOLCHAIN_FLAGS_NATIVE="-DCMAKE_C_COMPILER=$$(which $(HOSTCC));-DCMAKE_CXX_COMPILER=$$(which $(HOSTCXX))"
endif
ifeq ($(OS), emscripten)
LLVM_CMAKE += -DCMAKE_TOOLCHAIN_FILE=$(EMSCRIPTEN)/cmake/Modules/Platform/Emscripten.cmake -DLLVM_INCLUDE_TOOLS=OFF -DLLVM_BUILD_TOOLS=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_ENABLE_THREADS=OFF -DLLVM_BUILD_UTILS=OFF
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
ifeq (${USECLANG},0)
LLVM_CXXFLAGS += -mminimal-toc
endif
endif

ifeq ($(LLVM_SANITIZE),1)
ifeq ($(SANITIZE_MEMORY),1)
LLVM_CFLAGS += -fsanitize=memory -fsanitize-memory-track-origins
LLVM_LDFLAGS += -fsanitize=memory -fsanitize-memory-track-origins
LLVM_CXXFLAGS += -fsanitize=memory -fsanitize-memory-track-origins
LLVM_CMAKE += -DLLVM_USE_SANITIZER="MemoryWithOrigins"
endif
ifeq ($(SANITIZE_ADDRESS),1)
LLVM_CFLAGS += -fsanitize=address
LLVM_LDFLAGS += -fsanitize=address
LLVM_CXXFLAGS += -fsanitize=address
LLVM_CMAKE += -DLLVM_USE_SANITIZER="Address"
endif
ifeq ($(SANITIZE_THREAD),1)
LLVM_CFLAGS += -fsanitize=thread
LLVM_LDFLAGS += -fsanitize=thread
LLVM_CXXFLAGS += -fsanitize=thread
LLVM_CMAKE += -DLLVM_USE_SANITIZER="Thread"
endif
endif # LLVM_SANITIZE

ifeq ($(LLVM_LTO),1)
LLVM_CPPFLAGS += -flto
LLVM_LDFLAGS += -flto
endif # LLVM_LTO

ifeq ($(USE_LLVM_SHLIB),1)
ifeq ($(USECLANG),0)
# https://bugs.llvm.org/show_bug.cgi?id=48221
LLVM_CXXFLAGS += -fno-gnu-unique
endif
endif

ifeq ($(fPIC),)
LLVM_CMAKE += -DLLVM_ENABLE_PIC=OFF
endif

LLVM_CMAKE += -DCMAKE_C_FLAGS="$(LLVM_CPPFLAGS) $(LLVM_CFLAGS)" \
	-DCMAKE_CXX_FLAGS="$(LLVM_CPPFLAGS) $(LLVM_CXXFLAGS)"
ifeq ($(OS),Darwin)
# Explicitly use the default for -mmacosx-version-min=10.9 and later
LLVM_CMAKE += -DLLVM_ENABLE_LIBCXX=ON
endif

ifeq ($(BUILD_LLVM_CLANG),0)
# block default building of Clang
LLVM_CMAKE += -DLLVM_TOOL_CLANG_BUILD=OFF
LLVM_CMAKE += -DLLVM_TOOL_COMPILER_RT_BUILD=OFF
endif
ifeq ($(BUILD_LLDB),0)
# block default building of lldb
LLVM_CMAKE += -DLLVM_TOOL_LLDB_BUILD=OFF
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

LLVM_CMAKE += -DCMAKE_EXE_LINKER_FLAGS="$(LLVM_LDFLAGS)" \
	-DCMAKE_SHARED_LINKER_FLAGS="$(LLVM_LDFLAGS)"

# change the SONAME of Julia's private LLVM
# i.e. libLLVM-6.0jl.so
# see #32462
LLVM_CMAKE += -DLLVM_VERSION_SUFFIX:STRING="jl"
LLVM_CMAKE += -DLLVM_SHLIB_SYMBOL_VERSION:STRING="JL_LLVM_$(LLVM_VER_SHORT)"

# Apply version-specific LLVM patches sequentially
LLVM_PATCH_PREV :=
define LLVM_PATCH
$$(SRCCACHE)/$$(LLVM_SRC_DIR)/$1.patch-applied: $$(SRCCACHE)/$$(LLVM_SRC_DIR)/source-extracted | $$(SRCDIR)/patches/$1.patch $$(LLVM_PATCH_PREV)
	cd $$(SRCCACHE)/$$(LLVM_SRC_DIR)/llvm && patch -p1 < $$(SRCDIR)/patches/$1.patch
	echo 1 > $$@
# declare that applying any patch must re-run the compile step
$$(LLVM_BUILDDIR_withtype)/build-compiled: $$(SRCCACHE)/$$(LLVM_SRC_DIR)/$1.patch-applied
LLVM_PATCH_PREV := $$(SRCCACHE)/$$(LLVM_SRC_DIR)/$1.patch-applied
endef

define LLVM_PROJ_PATCH
$$(SRCCACHE)/$$(LLVM_SRC_DIR)/$1.patch-applied: $$(SRCCACHE)/$$(LLVM_SRC_DIR)/source-extracted | $$(SRCDIR)/patches/$1.patch $$(LLVM_PATCH_PREV)
	cd $$(SRCCACHE)/$$(LLVM_SRC_DIR) && patch -p1 < $$(SRCDIR)/patches/$1.patch
	echo 1 > $$@
# declare that applying any patch must re-run the compile step
$$(LLVM_BUILDDIR_withtype)/build-compiled: $$(SRCCACHE)/$$(LLVM_SRC_DIR)/$1.patch-applied
LLVM_PATCH_PREV := $$(SRCCACHE)/$$(LLVM_SRC_DIR)/$1.patch-applied
endef

ifeq ($(USE_SYSTEM_ZLIB), 0)
$(LLVM_BUILDDIR_withtype)/build-configured: | $(build_prefix)/manifest/zlib
endif

# NOTE: LLVM 12 and 13 have their patches applied to JuliaLang/llvm-project

# declare that all patches must be applied before running ./configure
$(LLVM_BUILDDIR_withtype)/build-configured: | $(LLVM_PATCH_PREV)

$(LLVM_BUILDDIR_withtype)/build-configured: $(SRCCACHE)/$(LLVM_SRC_DIR)/source-extracted | $(llvm_python_workaround)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
		export PATH=$(llvm_python_workaround):"$$PATH" && \
		$(CMAKE) $(SRCCACHE)/$(LLVM_SRC_DIR)/llvm $(CMAKE_GENERATOR_COMMAND) $(CMAKE_COMMON) $(LLVM_CMAKE) \
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
    cp -r $$(SRCCACHE)/$$(LLVM_SRC_DIR)/llvm/utils/lit $2$$(build_depsbindir)/ && \
    $$(CMAKE) -DCMAKE_INSTALL_PREFIX="$2$$(build_prefix)" -P cmake_install.cmake
ifeq ($(OS), WINNT)
LLVM_INSTALL += && cp $2$$(build_shlibdir)/libLLVM.dll $2$$(build_depsbindir)
endif
ifeq ($(OS),Darwin)
# https://github.com/JuliaLang/julia/issues/29981
LLVM_INSTALL += && ln -s libLLVM.dylib $2$$(build_shlibdir)/libLLVM-$$(LLVM_VER_SHORT).dylib
endif

$(eval $(call staged-install, \
	llvm,$$(LLVM_SRC_DIR)/build_$$(LLVM_BUILDTYPE), \
	LLVM_INSTALL,,,))

clean-llvm:
	-rm -f $(LLVM_BUILDDIR_withtype)/build-configured $(LLVM_BUILDDIR_withtype)/build-compiled
	-$(MAKE) -C $(LLVM_BUILDDIR_withtype) clean

get-llvm: $(LLVM_SRC_FILE)
extract-llvm: $(SRCCACHE)/$(LLVM_SRC_DIR)/source-extracted
configure-llvm: $(LLVM_BUILDDIR_withtype)/build-configured
compile-llvm: $(LLVM_BUILDDIR_withtype)/build-compiled
fastcheck-llvm: #none
check-llvm: $(LLVM_BUILDDIR_withtype)/build-checked
#todo: LLVM make check target is broken on julia.mit.edu (and really slow elsewhere)

else # USE_BINARYBUILDER_LLVM

# We provide a way to subversively swap out which LLVM JLL we pull artifacts from
ifeq ($(LLVM_ASSERTIONS), 1)
LLVM_JLL_DOWNLOAD_NAME := libLLVM_assert
LLVM_JLL_VER := $(LLVM_ASSERT_JLL_VER)
LLVM_TOOLS_JLL_DOWNLOAD_NAME := LLVM_assert
LLVM_TOOLS_JLL_VER := $(LLVM_TOOLS_ASSERT_JLL_VER)
endif

$(eval $(call bb-install,llvm,LLVM,false,true))
$(eval $(call bb-install,clang,CLANG,false,true))
$(eval $(call bb-install,llvm-tools,LLVM_TOOLS,false,true))

install-clang install-llvm-tools: install-llvm

endif # USE_BINARYBUILDER_LLVM
