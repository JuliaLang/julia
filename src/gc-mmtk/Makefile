## MMTK ##

MMTK_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME ?= $(abspath $(MMTK_DIR)/../..)
include $(JULIAHOME)/Make.inc

# Use a non-moving MMTk build by default.
MMTK_MOVING ?= 0
# Match Julia build mode by default
MMTK_BUILD_MODE ?= $(JULIA_BUILD_MODE)

MMTK_JULIA_DIR := $(MMTK_DIR)/mmtk_julia
MMTK_JULIA_LIB_PATH := $(MMTK_JULIA_DIR)/target/$(MMTK_BUILD_MODE)
MMTK_JULIA_LIB := $(MMTK_JULIA_LIB_PATH)/$(MMTK_LIB_NAME)
MMTK_INSTALLED_LIB := $(build_shlibdir)/$(MMTK_LIB_NAME)
MMTK_JULIA_BUILD_CONFIG := $(MMTK_JULIA_LIB_PATH)/.build_config
MMTK_JULIA_DEPS_LIBDIR := $(if $(build_libdir),$(build_libdir),$(JULIAHOME)/usr/lib)
MMTK_JULIA_BINDGEN_INCLUDE_DIRS := $$($(LLVM_CONFIG_HOST) --includedir)$(PATHSEP)$(LIBUV_INC)$(PATHSEP)$(build_includedir)
# Pin the libclang that bindgen loads.
#
# clang-sys searches LD_LIBRARY_PATH (set below so cargo can find the in-tree deps)
# among other places, and prefers the highest version it finds, which is Julia's own
# LLVM. bindgen separately derives its system include paths by running the `clang`
# executable from PATH. Left alone, those resolve to different LLVM installs, and a
# libclang given another toolchain's resource headers segfaults part-way through
# parsing Julia's headers. Pinning short-circuits the search, so libclang matches the
# clang whose headers it is handed.
MMTK_HOST_LLVM_CONFIG := $(shell command -v llvm-config 2>/dev/null)
ifneq ($(MMTK_HOST_LLVM_CONFIG),)
MMTK_LIBCLANG_PATH ?= $(shell $(MMTK_HOST_LLVM_CONFIG) --libdir 2>/dev/null)
endif
MMTK_JULIA_CARGO_ENV := LD_LIBRARY_PATH="$(MMTK_JULIA_DEPS_LIBDIR):$$LD_LIBRARY_PATH" MMTK_JULIA_BINDGEN_INCLUDE_DIRS="$(MMTK_JULIA_BINDGEN_INCLUDE_DIRS)"
ifneq ($(MMTK_LIBCLANG_PATH),)
MMTK_JULIA_CARGO_ENV += LIBCLANG_PATH="$(MMTK_LIBCLANG_PATH)"
endif
MMTK_JULIA_CARGO_BUILD := cd $(MMTK_JULIA_DIR) && $(MMTK_JULIA_CARGO_ENV) cargo build

ifeq (${MMTK_PLAN},Immix)
MMTK_JULIA_CARGO_FEATURES := immix
else ifeq (${MMTK_PLAN},StickyImmix)
MMTK_JULIA_CARGO_FEATURES := stickyimmix
else ifeq (${MMTK_PLAN},ConcurrentImmix)
MMTK_JULIA_CARGO_FEATURES := concurrentimmix
else
$(error "Unsupported MMTk plan: $(MMTK_PLAN)")
endif

ifeq ($(MMTK_MOVING), 0)
MMTK_JULIA_CARGO_FEATURES := $(MMTK_JULIA_CARGO_FEATURES),non_moving
endif

ifeq ($(MMTK_BUILD_MODE),release)
MMTK_JULIA_CARGO_PROFILE := --release
else ifeq ($(MMTK_BUILD_MODE),debug)
MMTK_JULIA_CARGO_PROFILE :=
else
$(error "Unsupported MMTk build: $(MMTK_BUILD_MODE)")
endif
MMTK_JULIA_CARGO_ARGS := --features $(MMTK_JULIA_CARGO_FEATURES) $(MMTK_JULIA_CARGO_PROFILE)

# These are rebuilt during every Julia build, so must be ignored
# to avoid re-triggering bindgen.
MMTK_JULIA_BINDGEN_IGNORE_DEPS := \
	$(JULIAHOME)/src/julia_version.h \
	$(JULIAHOME)/src/jl_internal_funcs.inc \
	$(JULIAHOME)/src/jl_data_globals_defs.inc \
	$(JULIAHOME)/src/julia_flisp.boot.inc
MMTK_JULIA_BINDGEN_DEPS := \
	$(filter-out $(MMTK_JULIA_BINDGEN_IGNORE_DEPS), \
		$(wildcard $(JULIAHOME)/src/*.h) \
		$(wildcard $(JULIAHOME)/src/*.inc) \
		$(wildcard $(JULIAHOME)/src/support/*.h) \
		$(wildcard $(JULIAHOME)/src/support/*.inc)) \
	$(wildcard $(MMTK_DIR)/*.h) \
	$(JULIAHOME)/VERSION \
	$(MMTK_JULIA_DIR)/api/mmtk.h \
	$(MMTK_JULIA_DIR)/api/mmtkMutator.h
MMTK_JULIA_RUST_DEPS := \
	$(MMTK_JULIA_DIR)/Cargo.lock \
	$(MMTK_JULIA_DIR)/Cargo.toml \
	$(MMTK_JULIA_DIR)/build.rs \
	$(MMTK_DIR)/Makefile \
	$(MMTK_JULIA_DIR)/rust-toolchain \
	$(filter-out $(MMTK_JULIA_DIR)/src/julia_types.rs,$(wildcard $(MMTK_JULIA_DIR)/src/*.rs))

MMTK_JULIA_DEPS :=
ifneq ($(USE_SYSTEM_LLVM), 1)
MMTK_JULIA_DEPS += install-llvm
endif
ifneq ($(USE_SYSTEM_LIBUV), 1)
MMTK_JULIA_DEPS += install-libuv
endif
ifeq ($(DISABLE_LIBUNWIND), 0)
ifneq ($(USE_SYSTEM_LIBUNWIND), 1)
ifneq (,$(filter $(OS),Linux FreeBSD))
MMTK_JULIA_DEPS += install-unwind
else ifneq (,$(filter $(OS),OpenBSD Darwin))
MMTK_JULIA_DEPS += install-llvmunwind
endif
endif
endif

# Public C API headers for the binding
MMTK_API_HEADERS := mmtk.h mmtkMutator.h
MMTK_INSTALLED_HEADERS := $(addprefix $(build_includedir)/,$(MMTK_API_HEADERS))

.PHONY: compile-mmtk_julia
compile-mmtk_julia: $(MMTK_INSTALLED_LIB) $(MMTK_INSTALLED_HEADERS)

.PHONY: clean-mmtk_julia
clean-mmtk_julia:
	-rm -f $(MMTK_INSTALLED_LIB) $(MMTK_INSTALLED_HEADERS)
	-cd $(MMTK_JULIA_DIR) && cargo clean

$(MMTK_INSTALLED_LIB): $(MMTK_JULIA_LIB) mmtk_julia-force
	@mkdir -p $(dir $@)
	@rm -f $@
	@cp -f $< $@

$(MMTK_INSTALLED_HEADERS): $(build_includedir)/%: $(MMTK_JULIA_DIR)/api/%
	@mkdir -p $(dir $@)
	$(INSTALL_F) $< $@

$(MMTK_JULIA_DIR)/src/julia_types.rs: $(MMTK_JULIA_BINDGEN_DEPS) | mmtk_julia-deps
	@echo "Regenerating the Rust FFI bindings for Julia";
	@rm -f $@
	@$(MMTK_JULIA_CARGO_BUILD) $(MMTK_JULIA_CARGO_ARGS)

.PHONY: mmtk_julia-deps
ifneq ($(strip $(MMTK_JULIA_DEPS)),)
mmtk_julia-deps:
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/deps $(MMTK_JULIA_DEPS)
else
mmtk_julia-deps:
endif

.PHONY: mmtk_julia-force
mmtk_julia-force:

$(MMTK_JULIA_BUILD_CONFIG): mmtk_julia-force
	@mkdir -p $(dir $@)
	@{ \
		echo "MMTK_PLAN=$(MMTK_PLAN)"; \
		echo "MMTK_MOVING=$(MMTK_MOVING)"; \
		echo "MMTK_BUILD_MODE=$(MMTK_BUILD_MODE)"; \
	} > $@.tmp
	@if test -f $@; then \
		old_config=$$(grep -v '^MMTK_BUILD_MODE=' $@); \
		new_config=$$(grep -v '^MMTK_BUILD_MODE=' $@.tmp); \
		if test "$$old_config" != "$$new_config"; then \
			echo "ERROR: MMTk was previously built with a different plan or moving configuration."; \
			echo "Existing configuration:"; \
			sed 's/^/  /' $@; \
			echo "Requested configuration:"; \
			sed 's/^/  /' $@.tmp; \
			echo "Run 'make clean' before rebuilding with a different MMTk configuration."; \
			rm -f $@.tmp; \
			exit 1; \
		fi; \
	fi; \
	if ! test -f $@; then \
		mv $@.tmp $@; \
	elif cmp -s $@ $@.tmp; then \
		rm -f $@.tmp; \
	else \
		mv $@.tmp $@; \
	fi

$(MMTK_JULIA_LIB): $(MMTK_JULIA_BUILD_CONFIG) $(MMTK_JULIA_DIR)/src/julia_types.rs $(MMTK_JULIA_RUST_DEPS) | mmtk_julia-deps
	@echo "Building the Rust project in $(MMTK_JULIA_DIR)/";
	@$(MMTK_JULIA_CARGO_BUILD) $(MMTK_JULIA_CARGO_ARGS)
