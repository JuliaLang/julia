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

$(BOLT_BUILDDIR)/build-configured: $(SRCCACHE)/$(BOLT_SRC_DIR)/source-extracted
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

$(eval $(call staged-install, \
	bolt,$$(BOLT_SRC_DIR)/build, \
	BOLT_INSTALL,,,))

clean-bolt:
	-rm -f $(BOLT_BUILDDIR)/build-configured $(BOLT_BUILDDIR)/build-compiled
	-if [ -d $(BOLT_BUILDDIR) ]; then $(MAKE) -C $(BOLT_BUILDDIR) clean; fi

get-bolt: $(BOLT_SRC_FILE)
extract-bolt: $(SRCCACHE)/$(BOLT_SRC_DIR)/source-extracted
configure-bolt: $(BOLT_BUILDDIR)/build-configured
compile-bolt: $(BOLT_BUILDDIR)/build-compiled
fastcheck-bolt: #none
check-bolt: $(BOLT_BUILDDIR)/build-checked

else # USE_BINARYBUILDER_BOLT

$(eval $(call bb-install,BOLT,BOLT,false,true))

endif # USE_BINARYBUILDER_BOLT
