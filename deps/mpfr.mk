## MPFR ##
include $(SRCDIR)/mpfr.version

ifeq ($(USE_SYSTEM_GMP), 0)
$(BUILDDIR)/mpfr-$(MPFR_VER)/build-configured: | $(build_prefix)/manifest/gmp
endif

ifneq ($(USE_BINARYBUILDER_MPFR),1)

MPFR_CONFIGURE_OPTS := $(CONFIGURE_COMMON)
MPFR_CONFIGURE_OPTS += --enable-thread-safe --enable-shared-cache --disable-float128 --disable-decimal-float
MPFR_CONFIGURE_OPTS += --enable-shared --disable-static

ifeq ($(USE_SYSTEM_GMP), 0)
MPFR_CONFIGURE_OPTS += --with-gmp=$(abspath $(build_prefix))
endif

ifeq ($(SANITIZE),1)
# Force generic C build
MPFR_CONFIGURE_OPTS += --host=none-unknown-linux
endif

ifeq ($(OS),emscripten)
MPFR_CONFIGURE_OPTS += CFLAGS="-fPIC"
endif

$(SRCCACHE)/mpfr-$(MPFR_VER).tar.bz2: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://www.mpfr.org/mpfr-$(MPFR_VER)/$(notdir $@)

$(SRCCACHE)/mpfr-$(MPFR_VER)/source-extracted: $(SRCCACHE)/mpfr-$(MPFR_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) -jxf $<
	touch -c $(SRCCACHE)/mpfr-$(MPFR_VER)/configure # old target
	echo 1 > $@

checksum-mpfr: $(SRCCACHE)/mpfr-$(MPFR_VER).tar.bz2
	$(JLCHECKSUM) $<

$(BUILDDIR)/mpfr-$(MPFR_VER)/build-configured: $(SRCCACHE)/mpfr-$(MPFR_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(MPFR_CONFIGURE_OPTS)
	echo 1 > $@

$(BUILDDIR)/mpfr-$(MPFR_VER)/build-compiled: $(BUILDDIR)/mpfr-$(MPFR_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/mpfr-$(MPFR_VER)/build-checked: $(BUILDDIR)/mpfr-$(MPFR_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	mpfr,mpfr-$(MPFR_VER), \
	MAKE_INSTALL,,, \
	$$(WIN_MAKE_HARD_LINK) $(build_bindir)/libmpfr-*.dll $(build_bindir)/libmpfr.dll && \
		$$(INSTALL_NAME_CMD)libmpfr.$$(SHLIB_EXT) $$(build_shlibdir)/libmpfr.$$(SHLIB_EXT)))

clean-mpfr:
	-rm -f $(BUILDDIR)/mpfr-$(MPFR_VER)/build-configured $(BUILDDIR)/mpfr-$(MPFR_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/mpfr-$(MPFR_VER) clean

distclean-mpfr:
	rm -rf $(SRCCACHE)/mpfr-$(MPFR_VER).tar.bz2 \
		$(SRCCACHE)/mpfr-$(MPFR_VER) \
		$(BUILDDIR)/mpfr-$(MPFR_VER)

get-mpfr: $(SRCCACHE)/mpfr-$(MPFR_VER).tar.bz2
extract-mpfr: $(SRCCACHE)/mpfr-$(MPFR_VER)/source-extracted
configure-mpfr: $(BUILDDIR)/mpfr-$(MPFR_VER)/build-configured
compile-mpfr: $(BUILDDIR)/mpfr-$(MPFR_VER)/build-compiled
fastcheck-mpfr: check-mpfr
check-mpfr: $(BUILDDIR)/mpfr-$(MPFR_VER)/build-checked

else # USE_BINARYBUILDER_MPFR

$(eval $(call bb-install,mpfr,MPFR,false))

endif
