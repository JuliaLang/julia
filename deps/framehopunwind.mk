## FRAMEHOPUNWIND ##
# framehop-backed, libunwind-compatible unwinder. Replaces libunwind's *backtrace* path.
# Default: the prebuilt LibFramehopUnwind_jll artifact (bb-install). Source build
# (USE_BINARYBUILDER_FRAMEHOPUNWIND=0) needs a Rust toolchain, like mmtk_julia.

ifneq ($(USE_BINARYBUILDER_FRAMEHOPUNWIND),1)

FRAMEHOPUNWIND_GIT_URL := https://github.com/gbaraldi/framehopunwind.git
FRAMEHOPUNWIND_TAR_URL = https://api.github.com/repos/gbaraldi/framehopunwind/tarball/$1
$(eval $(call git-external,framehopunwind,FRAMEHOPUNWIND,,,$(BUILDDIR)))

FRAMEHOPUNWIND_BUILDDIR := $(BUILDDIR)/$(FRAMEHOPUNWIND_SRC_DIR)

# Rust toolchain (override CARGO in Make.user if it is not on PATH).
CARGO ?= cargo

ifeq ($(USE_FRAMEHOP), 1)
ifneq ($(XC_HOST),)
$(error the framehopunwind source build does not support cross-compilation (cargo builds for the host triple); use USE_BINARYBUILDER_FRAMEHOPUNWIND=1)
endif
endif

# The `+` hands make's jobserver to cargo.
$(FRAMEHOPUNWIND_BUILDDIR)/build-compiled: $(FRAMEHOPUNWIND_BUILDDIR)/source-extracted
	@command -v $(CARGO) >/dev/null 2>&1 || { \
		echo "ERROR: building framehopunwind from source requires a Rust toolchain (cargo >= 1.88)." >&2; \
		echo "       Install one via https://rustup.rs, set CARGO in Make.user, or use" >&2; \
		echo "       USE_BINARYBUILDER_FRAMEHOPUNWIND=1 for the prebuilt artifact." >&2; \
		exit 1; }
	+cd $(dir $<) && $(CARGO) build --release --locked $(FRAMEHOPUNWIND_CARGO_FLAGS)
	echo 1 > $@

ifeq (,$(filter framehopunwind 1,$(DEPS_GIT)))
# The crates in Cargo.lock, downloaded with the source so that cargo needs no network access.
FRAMEHOPUNWIND_CRATES := $(SRCCACHE)/$(FRAMEHOPUNWIND_SRC_DIR)-crates.tar.gz
FRAMEHOPUNWIND_CARGO_FLAGS := --offline --config 'source.crates-io.replace-with="crates"' \
	--config 'source.crates.directory="crates"'

$(FRAMEHOPUNWIND_CRATES): | $(FRAMEHOPUNWIND_SRC_FILE)
	$(JLCHECKSUM) $(FRAMEHOPUNWIND_SRC_FILE)
	rm -rf $@.tmp
	mkdir -p $@.tmp/src $@.tmp/crates
	$(TAR) -C $@.tmp/src --strip-components 1 -xf $(FRAMEHOPUNWIND_SRC_FILE)
	awk -F'"' '/^name = /{n=$$2} /^version = /{v=$$2} /^checksum = /{print n, n "-" v, $$2}' \
		$@.tmp/src/Cargo.lock > $@.tmp/list
	sha256=$$(command -v sha256sum || echo shasum -a 256); \
	while read name crate sum; do \
		$(JLDOWNLOAD) $@.tmp/$$crate.crate https://static.crates.io/crates/$$name/$$crate.crate && \
		[ "$$($$sha256 $@.tmp/$$crate.crate | cut -d' ' -f1)" = "$$sum" ] && \
		$(TAR) -C $@.tmp/crates -xzf $@.tmp/$$crate.crate && \
		printf '{"files":{},"package":"%s"}' $$sum > $@.tmp/crates/$$crate/.cargo-checksum.json || \
		{ echo "ERROR: could not fetch $$crate" >&2; exit 1; }; \
	done < $@.tmp/list
	$(TAR) -C $@.tmp -czf $@.tmp/crates.tar.gz crates
	mv $@.tmp/crates.tar.gz $@
	rm -rf $@.tmp

$(FRAMEHOPUNWIND_BUILDDIR)/crates-extracted: $(FRAMEHOPUNWIND_BUILDDIR)/source-extracted | $(FRAMEHOPUNWIND_CRATES)
	rm -rf $(dir $@)crates
	$(TAR) -C $(dir $@) -xzf $(FRAMEHOPUNWIND_CRATES)
	echo 1 > $@
$(FRAMEHOPUNWIND_BUILDDIR)/build-compiled: $(FRAMEHOPUNWIND_BUILDDIR)/crates-extracted
endif

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

get-framehopunwind: $(FRAMEHOPUNWIND_SRC_FILE) $(FRAMEHOPUNWIND_CRATES)
extract-framehopunwind: $(FRAMEHOPUNWIND_BUILDDIR)/source-extracted
configure-framehopunwind: extract-framehopunwind
compile-framehopunwind: $(FRAMEHOPUNWIND_BUILDDIR)/build-compiled
fastcheck-framehopunwind: check-framehopunwind
check-framehopunwind: compile-framehopunwind

else

$(eval $(call bb-install,framehopunwind,FRAMEHOPUNWIND,false))

endif # USE_BINARYBUILDER_FRAMEHOPUNWIND

ifneq (,$(findstring $(OS),Linux FreeBSD))
# The library links to libgcc_s; have it find the copy Julia ships next to it. Patch only
# once, since rewriting the file can crash processes that have it loaded.
FRAMEHOPUNWIND_LIB := $(build_shlibdir)/libframehopunwind.$(SHLIB_EXT)
install-framehopunwind: post-install-framehopunwind
post-install-framehopunwind: $(build_prefix)/manifest/framehopunwind $(PATCHELF_MANIFEST)
	[ "$$($(PATCHELF) --print-rpath $(FRAMEHOPUNWIND_LIB))" = '$$ORIGIN' ] || \
		$(PATCHELF) $(PATCHELF_SET_RPATH_ARG) '$$ORIGIN' $(FRAMEHOPUNWIND_LIB)
endif
