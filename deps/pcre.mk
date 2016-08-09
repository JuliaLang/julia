## PCRE ##

# Force optimization for PCRE flags (Issue #11668)
PCRE_CFLAGS := -O3
PCRE_LDFLAGS := $(RPATH_ESCAPED_ORIGIN)

$(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre2-$(PCRE_VER).tar.bz2

$(SRCDIR)/srccache/pcre2-$(PCRE_VER)/source-extracted: $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $(notdir $<)
	touch -c $(SRCDIR)/srccache/pcre2-$(PCRE_VER)/configure # old target
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

$(eval $(call staged-install, \
	pcre,pcre2-$$(PCRE_VER), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD),, \
	$$(INSTALL_NAME_CMD)libpcre2-8.$$(SHLIB_EXT) $$(build_shlibdir)/libpcre2-8.$$(SHLIB_EXT)))

clean-pcre:
	-rm $(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured $(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/pcre2-$(PCRE_VER) clean

distclean-pcre:
	-rm -rf $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2 $(SRCDIR)/srccache/pcre2-$(PCRE_VER) $(BUILDDIR)/pcre2-$(PCRE_VER)


get-pcre: $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2
extract-pcre: $(SRCDIR)/srccache/pcre2-$(PCRE_VER)/source-extracted
configure-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured
compile-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled
fastcheck-pcre: check-pcre
check-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-checked
