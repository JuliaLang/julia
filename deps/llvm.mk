## LLVM ##
include $(SRCDIR)/llvm-ver.make
include $(SRCDIR)/llvm-options.mk

ifneq ($(USE_BINARYBUILDER_LLVM), 1)
LLVM_GIT_URL:=git://github.com/JuliaLang/llvm-project.git
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
LLVM_CMAKE += -DLLVM_ENABLE_ZLIB=ON -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_HOST_TRIPLE="$(or $(XC_HOST),$(BUILD_MACHINE))"
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

ifeq ($(USE_LLVM_SHLIB),1)
ifeq ($(USECLANG),0)
# https://bugs.llvm.org/show_bug.cgi?id=48221
LLVM_CXXFLAGS += -fno-gnu-unique
endif
endif

ifeq ($(fPIC),)
LLVM_CMAKE += -DLLVM_ENABLE_PIC=OFF
endif

# disable ABI breaking checks: by default only enabled for asserts build, in which case
# it is then impossible to call non-asserts LLVM libraries (like out-of-tree backends)
LLVM_CMAKE += -DLLVM_ABI_BREAKING_CHECKS=FORCE_OFF

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

ifeq ($(LLVM_VER_SHORT),11.0)
ifeq ($(LLVM_VER_PATCH), 0)
$(eval $(call LLVM_PATCH,llvm-D27629-AArch64-large_model_6.0.1)) # remove for LLVM 12
endif # LLVM_VER 11.0.0
$(eval $(call LLVM_PATCH,llvm8-D34078-vectorize-fdiv)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-7.0-D44650)) # replaced by D90969 for LLVM 12
$(eval $(call LLVM_PATCH,llvm-6.0-DISABLE_ABI_CHECKS)) # Needs upstreaming
$(eval $(call LLVM_PATCH,llvm9-D50010-VNCoercion-ni)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm7-revert-D44485)) # Needs upstreaming
$(eval $(call LLVM_PATCH,llvm-11-D75072-SCEV-add-type))
$(eval $(call LLVM_PATCH,llvm-julia-tsan-custom-as))
$(eval $(call LLVM_PATCH,llvm-D80101)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-D84031)) # remove for LLVM 12
ifeq ($(LLVM_VER_PATCH), 0)
$(eval $(call LLVM_PATCH,llvm-10-D85553)) # remove for LLVM 12
endif # LLVM_VER 11.0.0
$(eval $(call LLVM_PATCH,llvm-10-unique_function_clang-sa)) # Needs upstreaming
ifeq ($(BUILD_LLVM_CLANG),1)
$(eval $(call LLVM_PATCH,llvm-D88630-clang-cmake))
endif
ifeq ($(LLVM_VER_PATCH), 0)
$(eval $(call LLVM_PATCH,llvm-11-D85313-debuginfo-empty-arange)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-11-D90722-rtdyld-absolute-relocs)) # remove for LLVM 12
endif # LLVM_VER 11.0.0
$(eval $(call LLVM_PATCH,llvm-invalid-addrspacecast-sink)) # Still being upstreamed as D92210
$(eval $(call LLVM_PATCH,llvm-11-D92906-ppc-setjmp)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-11-PR48458-X86ISelDAGToDAG)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-11-D93092-ppc-knownbits)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-11-D93154-globalisel-as))
$(eval $(call LLVM_PATCH,llvm-11-ppc-half-ctr)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-11-ppc-sp-from-bp)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-rGb498303066a6-gcc11-header-fix)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-11-D94813-mergeicmps))
$(eval $(call LLVM_PATCH,llvm-11-D94980-CTR-half)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-11-D94058-sext-atomic-ops)) # remove for LLVM 12
$(eval $(call LLVM_PATCH,llvm-11-D96283-dagcombine-half)) # remove for LLVM 12
$(eval $(call LLVM_PROJ_PATCH,llvm-11-AArch64-FastIsel-bug))
$(eval $(call LLVM_PROJ_PATCH,llvm-11-D97435-AArch64-movaddrreg))
$(eval $(call LLVM_PROJ_PATCH,llvm-11-D97571-AArch64-loh)) # remove for LLVM 13
$(eval $(call LLVM_PROJ_PATCH,llvm-11-aarch64-addrspace)) # remove for LLVM 13
endif # LLVM_VER 11.0

# NOTE: LLVM 12 has the patches applied to JuliaLang/llvm-project

# Add a JL prefix to the version map. DO NOT REMOVE
ifneq ($(LLVM_VER), svn)
$(eval $(call LLVM_PATCH,llvm7-symver-jlprefix))
endif

# declare that all patches must be applied before running ./configure
$(LLVM_BUILDDIR_withtype)/build-configured: | $(LLVM_PATCH_PREV) $(build_prefix)/manifest/zlib

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
	-rm $(LLVM_BUILDDIR_withtype)/build-configured $(LLVM_BUILDDIR_withtype)/build-compiled
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
