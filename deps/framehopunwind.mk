## FRAMEHOPUNWIND ##
# framehop-backed, libunwind-compatible unwinder (built from source with cargo, like
# mmtk_julia; fetched from git like cpufeatures). Replaces libunwind's *backtrace* path.
# A future LibFramehopUnwind_jll would ship this prebuilt and skip the cargo dependency.

FRAMEHOPUNWIND_GIT_URL := https://github.com/gbaraldi/framehopunwind.git
FRAMEHOPUNWIND_TAR_URL = https://api.github.com/repos/gbaraldi/framehopunwind/tarball/$1
$(eval $(call git-external,framehopunwind,FRAMEHOPUNWIND,,,$(BUILDDIR)))

FRAMEHOPUNWIND_BUILDDIR := $(BUILDDIR)/$(FRAMEHOPUNWIND_SRC_DIR)

# Rust toolchain (override CARGO in Make.user if it is not on PATH).
CARGO ?= cargo

ifeq ($(USE_FRAMEHOP), 1)
ifneq ($(XC_HOST),)
$(error USE_FRAMEHOP=1 does not support cross-compilation yet (cargo builds for the host triple); build natively or wait for LibFramehopUnwind_jll)
endif
endif

# NB: the first build fetches the crates.io deps (pinned via --locked); a fully-offline
# build needs vendored crates or a future LibFramehopUnwind_jll. The `+` hands make's
# jobserver to cargo.
$(FRAMEHOPUNWIND_BUILDDIR)/build-compiled: $(FRAMEHOPUNWIND_BUILDDIR)/source-extracted
	@command -v $(CARGO) >/dev/null 2>&1 || { \
		echo "ERROR: USE_FRAMEHOP=1 requires a Rust toolchain (cargo >= 1.78)." >&2; \
		echo "       Install one via https://rustup.rs, or set CARGO in Make.user." >&2; \
		exit 1; }
	+cd $(dir $<) && \
		$(CARGO) build --release --locked --offline 2>/dev/null || \
		$(CARGO) build --release --locked
	echo 1 > $@

define FRAMEHOPUNWIND_INSTALL
	mkdir -p $2/$$(build_includedir)
	mkdir -p $2/$$(build_shlibdir)
	cp $1/include/framehopunwind.h $2/$$(build_includedir)/
	cp $1/target/release/libframehopunwind.$$(SHLIB_EXT) $2/$$(build_shlibdir)/
endef
$(eval $(call staged-install, \
	framehopunwind,$(FRAMEHOPUNWIND_SRC_DIR), \
	FRAMEHOPUNWIND_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libframehopunwind.$$(SHLIB_EXT) $$(build_shlibdir)/libframehopunwind.$$(SHLIB_EXT)))

clean-framehopunwind:
	-rm -f $(FRAMEHOPUNWIND_BUILDDIR)/build-compiled
	-rm -rf $(FRAMEHOPUNWIND_BUILDDIR)/target

get-framehopunwind: $(FRAMEHOPUNWIND_SRC_FILE)
extract-framehopunwind: $(FRAMEHOPUNWIND_BUILDDIR)/source-extracted
configure-framehopunwind: extract-framehopunwind
compile-framehopunwind: $(FRAMEHOPUNWIND_BUILDDIR)/build-compiled
fastcheck-framehopunwind: check-framehopunwind
check-framehopunwind: compile-framehopunwind
