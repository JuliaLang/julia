## LLVMDialects
include $(SRCDIR)/llvm-options.mk

LLVMDIALECTS_GIT_URL := https://github.com/JuliaLang/llvm-dialects.git
LLVMDIALECTS_TAR_URL = https://api.github.com/repos/JuliaLang/llvm-dialects/tarball/$1
$(eval $(call git-external,llvmdialects,LLVMDIALECTS,CMakeLists.txt,,$(SRCCACHE)))

LLVMDIALECTS_BUILDDIR_withtype := $(BUILDDIR)/$(LLVMDIALECTS_SRC_DIR)/build_$(LLVM_BUILDTYPE)

ifeq ($(USE_SYSTEM_LLVM), 0)
$(LLVMDIALECTS_BUILDDIR_withtype)/build-configured: | $(build_prefix)/manifest/llvm
endif

# Build against the in-tree LLVM's cmake exports, or ask the system
# llvm-config for them when a system LLVM is used.
ifeq ($(USE_SYSTEM_LLVM), 1)
LLVMDIALECTS_LLVM_ROOT := $$($(LLVM_CONFIG) --cmakedir)
else
LLVMDIALECTS_LLVM_ROOT := $(build_libdir)/cmake/llvm
endif

LLVMDIALECTS_OPTS := $(CMAKE_COMMON) -DLLVM_ROOT="$(LLVMDIALECTS_LLVM_ROOT)" \
		-DCMAKE_POSITION_INDEPENDENT_CODE=ON -DCMAKE_BUILD_TYPE="$(LLVM_CMAKE_BUILDTYPE)"
ifeq ($(LLVM_ASSERTIONS), 1)
LLVMDIALECTS_OPTS += -DLLVM_ENABLE_ASSERTIONS:BOOL=ON
endif # LLVM_ASSERTIONS

LLVMDIALECTS_SRC_PATH := $(SRCCACHE)/$(LLVMDIALECTS_SRC_DIR)

$(LLVMDIALECTS_BUILDDIR_withtype)/build-configured: $(LLVMDIALECTS_SRC_PATH)/source-extracted
	mkdir -p $(dir $@)
ifeq ($(USE_SYSTEM_LLVM), 0)
# The LLVM tarballs do not ship every artifact that LLVMExports.cmake
# references (libLTO, libRemarks, some tools), which fails any
# find_package(LLVM) against the installed tree. llvm-dialects only needs
# the core libraries, so satisfy the export checks with placeholder
# symlinks for whatever is missing.
# TODO: the placeholders (symlinks to llvm-config) stay behind in
# $(build_prefix) and are not removed by uninstall-llvm.
	grep -oh '{_IMPORT_PREFIX}/[^"]*' $(build_libdir)/cmake/llvm/LLVMExports*.cmake | sed 's|^{_IMPORT_PREFIX}||' | sort -u | while read -r f; do \
		[ -e "$(build_prefix)$$f" ] || [ -h "$(build_prefix)$$f" ] || { \
			mkdir -p "$$(dirname "$(build_prefix)$$f")" && \
			ln -s $(build_depsbindir)/llvm-config "$(build_prefix)$$f"; }; \
	done
endif
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(CMAKE_GENERATOR_COMMAND) $(LLVMDIALECTS_OPTS)
	echo 1 > $@

$(LLVMDIALECTS_BUILDDIR_withtype)/build-compiled: $(LLVMDIALECTS_BUILDDIR_withtype)/build-configured
	cd $(dir $<) && \
		$(if $(filter $(CMAKE_GENERATOR),make), \
		  $(MAKE), \
		  $(CMAKE) --build .)
	echo 1 > $@

# TODO: llvm-dialects-tblgen is built for the target, but runs on the build
# host (src/Makefile invokes it); cross-compilation needs a host build of it,
# similar to llvm-tools.
define LLVMDIALECTS_INSTALL
	mkdir -p $2/$$(build_includedir)
	mkdir -p $2/$$(build_libdir)
	mkdir -p $2/$$(build_depsbindir)
	cp $1/llvm-dialects-tblgen$$(EXE) $2/$$(build_depsbindir)
	cp $1/libllvm_dialects.a $2/$$(build_libdir)
	cp -r $(LLVMDIALECTS_SRC_PATH)/include/llvm-dialects $2/$$(build_includedir)/
endef

$(eval $(call staged-install, \
	llvmdialects,$$(LLVMDIALECTS_SRC_DIR)/build_$$(LLVM_BUILDTYPE), \
	LLVMDIALECTS_INSTALL,,,))

clean-llvmdialects:
	-rm -f $(LLVMDIALECTS_BUILDDIR_withtype)/build-configured $(LLVMDIALECTS_BUILDDIR_withtype)/build-compiled
	-if [ -d $(LLVMDIALECTS_BUILDDIR_withtype) ]; then $(MAKE) -C $(LLVMDIALECTS_BUILDDIR_withtype) clean; fi

get-llvmdialects: $(LLVMDIALECTS_SRC_FILE)
extract-llvmdialects: $(SRCCACHE)/$(LLVMDIALECTS_SRC_DIR)/source-extracted
configure-llvmdialects: $(LLVMDIALECTS_BUILDDIR_withtype)/build-configured
compile-llvmdialects: $(LLVMDIALECTS_BUILDDIR_withtype)/build-compiled
fastcheck-llvmdialects: #none
check-llvmdialects: #none
