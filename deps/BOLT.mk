## BOLT ##
include $(SRCDIR)/BOLT.version

ifneq ($(USE_BINARYBUILDER_BOLT), 1)
BOLT_GIT_URL:=https://github.com/llvm/llvm-project.git
BOLT_TAR_URL=https://api.github.com/repos/llvm/llvm-project/tarball/$1
$(eval $(call git-external,BOLT,BOLT,CMakeLists.txt,,$(SRCCACHE)))

BOLT_BUILDDIR := $(BUILDDIR)/$(BOLT_SRC_DIR)/build

BOLT_BUILD_PROJECTS := bolt

BOLT_BUILD_CFLAGS := $(CFLAGS)
BOLT_BUILD_CXXFLAGS := $(CXXFLAGS)
BOLT_BUILD_CPPFLAGS := $(CPPFLAGS)
BOLT_BUILD_LDFLAGS := $(LDFLAGS)
BOLT_BUILD_CMAKE :=

BOLT_BUILD_CMAKE += -DLLVM_ENABLE_PROJECTS="$(BOLT_BUILD_PROJECTS)"

# Otherwise LLVM will translate \\ to / on mingw
BOLT_BUILD_CMAKE += -DLLVM_WINDOWS_PREFER_FORWARD_SLASH=False

BOLT_BUILD_CMAKE += -DLLVM_TARGETS_TO_BUILD:STRING=host -DCMAKE_BUILD_TYPE=Release
BOLT_BUILD_CMAKE += -DLLVM_ENABLE_LIBXML2=OFF -DLLVM_HOST_TRIPLE="$(or $(XC_HOST),$(BUILD_MACHINE))"
BOLT_BUILD_CMAKE += -DLLVM_ENABLE_ZLIB=FORCE_ON -DZLIB_ROOT="$(build_prefix)"

BOLT_BUILD_CMAKE += -DLLVM_BINDINGS_LIST="" -DLLVM_ENABLE_BINDINGS=OFF -DLLVM_INCLUDE_DOCS=Off -DLLVM_ENABLE_TERMINFO=Off -DHAVE_LIBEDIT=Off

ifeq ($(OS), WINNT)
BOLT_BUILD_CPPFLAGS += -D__USING_SJLJ_EXCEPTIONS__ -D__CRT__NO_INLINE
endif # OS == WINNT
ifneq ($(HOSTCC),$(CC))
BOLT_BUILD_CMAKE += -DCROSS_TOOLCHAIN_FLAGS_NATIVE="-DCMAKE_C_COMPILER=$$(which $(HOSTCC));-DCMAKE_CXX_COMPILER=$$(which $(HOSTCXX))"

# Defaults to off when crosscompiling, starting from LLVM 18
BOLT_BUILD_CMAKE += -DBOLT_ENABLE_RUNTIME=ON
endif
ifeq ($(OS), emscripten)
BOLT_BUILD_CMAKE += -DCMAKE_TOOLCHAIN_FILE=$(EMSCRIPTEN)/cmake/Modules/Platform/Emscripten.cmake -DLLVM_INCLUDE_TOOLS=OFF -DLLVM_BUILD_TOOLS=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_ENABLE_THREADS=OFF -DLLVM_BUILD_UTILS=OFF
endif # OS == emscripten

ifneq (,$(filter $(ARCH), powerpc64le ppc64le))
ifeq (${USECLANG},0)
BOLT_BUILD_CXXFLAGS += -mminimal-toc
endif
endif

ifeq ($(fPIC),)
BOLT_BUILD_CMAKE += -DLLVM_ENABLE_PIC=OFF
endif

BOLT_BUILD_CMAKE += -DCMAKE_C_FLAGS="$(BOLT_BUILD_CPPFLAGS) $(BOLT_BUILD_CFLAGS)" \
	-DCMAKE_CXX_FLAGS="$(BOLT_BUILD_CPPFLAGS) $(BOLT_BUILD_CXXFLAGS)"
ifeq ($(OS),Darwin)
# Explicitly use the default for -mmacosx-version-min=10.9 and later
BOLT_BUILD_CMAKE += -DLLVM_ENABLE_LIBCXX=ON
endif

BOLT_BUILD_CMAKE += -DCMAKE_EXE_LINKER_FLAGS="$(BOLT_BUILD_LDFLAGS)" \
	-DCMAKE_SHARED_LINKER_FLAGS="$(BOLT_BUILD_LDFLAGS)"

ifeq ($(USE_SYSTEM_ZLIB), 0)
$(BOLT_BUILDDIR)/build-configured: | $(build_prefix)/manifest/zlib
endif

# Backport of llvm/llvm-project#215415, which the BOLT_jll build also carries:
# without it, BOLT cannot rewrite a ThinLTO-built libLLVM on AArch64.
$(SRCCACHE)/$(BOLT_SRC_DIR)/BOLT-aarch64-adr-relaxation-non-simple.patch-applied: $(SRCCACHE)/$(BOLT_SRC_DIR)/source-extracted
	cd $(dir $@) && \
		patch -p1 -f < $(SRCDIR)/patches/BOLT-aarch64-adr-relaxation-non-simple.patch
	echo 1 > $@

# Backport of llvm/llvm-project#226076, which the BOLT_jll build also carries:
# without it, rewriting debug info corrupts units with forward DW_FORM_ref_udata
# references, such as those GNU as generates for libgcc's AArch64 lse.S.
$(SRCCACHE)/$(BOLT_SRC_DIR)/BOLT-dwarf-ref-udata-forward-refs.patch-applied: $(SRCCACHE)/$(BOLT_SRC_DIR)/BOLT-aarch64-adr-relaxation-non-simple.patch-applied
	cd $(dir $@) && \
		patch -p1 -f < $(SRCDIR)/patches/BOLT-dwarf-ref-udata-forward-refs.patch
	echo 1 > $@

$(BOLT_BUILDDIR)/build-configured: $(SRCCACHE)/$(BOLT_SRC_DIR)/BOLT-dwarf-ref-udata-forward-refs.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
		$(CMAKE) $(SRCCACHE)/$(BOLT_SRC_DIR)/llvm $(CMAKE_GENERATOR_COMMAND) $(CMAKE_COMMON) $(BOLT_BUILD_CMAKE) \
		|| { echo '*** To install a newer version of cmake, run contrib/download_cmake.sh ***' && false; }
	echo 1 > $@

$(BOLT_BUILDDIR)/build-compiled: $(BOLT_BUILDDIR)/build-configured
	cd $(BOLT_BUILDDIR) && \
		$(CMAKE) --build . --target bolt
	echo 1 > $@

$(BOLT_BUILDDIR)/build-checked: $(BOLT_BUILDDIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	cd $(BOLT_BUILDDIR) && \
		  $(CMAKE) --build . --target check-bolt
endif
	echo 1 > $@

BOLT_INSTALL = \
	cd $1 && mkdir -p $2$$(build_depsbindir) && \
	$$(CMAKE) -DCMAKE_INSTALL_PREFIX="$2$$(build_prefix)" -P tools/bolt/cmake_install.cmake

# Use the same target names as the BinaryBuilder install below (and the name of
# BOLT.version), so that `make install-BOLT` works for either kind of build.
$(eval $(call staged-install, \
	BOLT,$$(BOLT_SRC_DIR)/build, \
	BOLT_INSTALL,,,))

clean-BOLT:
	-rm -f $(BOLT_BUILDDIR)/build-configured $(BOLT_BUILDDIR)/build-compiled
	-if [ -d $(BOLT_BUILDDIR) ]; then $(MAKE) -C $(BOLT_BUILDDIR) clean; fi

get-BOLT: $(BOLT_SRC_FILE)
extract-BOLT: $(SRCCACHE)/$(BOLT_SRC_DIR)/source-extracted
configure-BOLT: $(BOLT_BUILDDIR)/build-configured
compile-BOLT: $(BOLT_BUILDDIR)/build-compiled
fastcheck-BOLT: #none
check-BOLT: $(BOLT_BUILDDIR)/build-checked

else # USE_BINARYBUILDER_BOLT

$(eval $(call bb-install,BOLT,BOLT,false,true))

endif # USE_BINARYBUILDER_BOLT
