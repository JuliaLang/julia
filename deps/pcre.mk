## PCRE ##

# Force optimization for PCRE flags (Issue #11668)
PCRE_CFLAGS := -O3
PCRE_LDFLAGS := $(RPATH_ESCAPED_ORIGIN)

$(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre2-$(PCRE_VER).tar.bz2

$(SRCDIR)/srccache/pcre2-$(PCRE_VER)/source-extracted: $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $(notdir $<)
	echo $1 > $@

$(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured: $(SRCDIR)/srccache/pcre2-$(PCRE_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) --enable-jit --includedir=$(build_includedir) CFLAGS="$(CFLAGS) $(PCRE_CFLAGS)" LDFLAGS="$(LDFLAGS) $(PCRE_LDFLAGS)"
	echo 1 > $@

$(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	echo 1 > $@

$(BUILDDIR)/pcre2-$(PCRE_VER)/checked: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
ifneq ($(OS),WINNT)
	$(MAKE) -C $(dir $@) check -j1
endif
endif
	echo 1 > $@

$(build_prefix)/manifest/pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled | $(build_prefix)/manifest
	$(call make-install,pcre2-$(PCRE_VER),$(LIBTOOL_CCLD))
	$(INSTALL_NAME_CMD)libpcre2-8.$(SHLIB_EXT) $(build_shlibdir)/libpcre2-8.$(SHLIB_EXT)
	echo $(PCRE_VER) > $@

clean-pcre:
	-rm -f $(build_shlibdir)/libpcre* $(build_prefix)/manifest/pcre \
		$(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured $(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/pcre2-$(PCRE_VER) clean

distclean-pcre:
	-rm -rf $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2 $(SRCDIR)/srccache/pcre2-$(PCRE_VER) $(BUILDDIR)/pcre2-$(PCRE_VER)

get-pcre: $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2
extract-pcre: $(SRCDIR)/srccache/pcre2-$(PCRE_VER)/source-extracted
configure-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured
compile-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled
check-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-checked
install-pcre: $(build_prefix)/manifest/pcre
