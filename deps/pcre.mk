## PCRE ##

ifneq ($(USE_BINARYBUILDER_PCRE),1)
# Force optimization for PCRE flags (Issue #11668)
PCRE_CFLAGS := -O3
PCRE_LDFLAGS := $(RPATH_ESCAPED_ORIGIN)

$(SRCCACHE)/pcre2-$(PCRE_VER).tar.bz2: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://ftp.pcre.org/pub/pcre/pcre2-$(PCRE_VER).tar.bz2

$(SRCCACHE)/pcre2-$(PCRE_VER)/source-extracted: $(SRCCACHE)/pcre2-$(PCRE_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $(notdir $<)
	cp $(SRCDIR)/patches/config.sub $(SRCCACHE)/pcre2-$(PCRE_VER)/config.sub
	touch -c $(SRCCACHE)/pcre2-$(PCRE_VER)/configure # old target
	echo $1 > $@

$(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured: $(SRCCACHE)/pcre2-$(PCRE_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) --enable-jit --includedir=$(build_includedir) CFLAGS="$(CFLAGS) $(PCRE_CFLAGS)" LDFLAGS="$(LDFLAGS) $(PCRE_LDFLAGS)"
	echo 1 > $@

$(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	echo 1 > $@

$(BUILDDIR)/pcre2-$(PCRE_VER)/build-checked: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
ifneq ($(OS),WINNT)
	$(MAKE) -C $(dir $@) check -j1
endif
endif
	echo 1 > $@

$(eval $(call staged-install, \
	pcre,pcre2-$$(PCRE_VER), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD),, \
	rm $$(build_shlibdir)/libpcre2-posix.* && \
	$$(INSTALL_NAME_CMD)libpcre2-8.$$(SHLIB_EXT) $$(build_shlibdir)/libpcre2-8.$$(SHLIB_EXT)))

clean-pcre:
	-rm $(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured $(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/pcre2-$(PCRE_VER) clean

distclean-pcre:
	-rm -rf $(SRCCACHE)/pcre2-$(PCRE_VER).tar.bz2 $(SRCCACHE)/pcre2-$(PCRE_VER) $(BUILDDIR)/pcre2-$(PCRE_VER)


get-pcre: $(SRCCACHE)/pcre2-$(PCRE_VER).tar.bz2
extract-pcre: $(SRCCACHE)/pcre2-$(PCRE_VER)/source-extracted
configure-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-configured
compile-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-compiled
fastcheck-pcre: check-pcre
check-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/build-checked

else # USE_BINARYBUILDER_PCRE
PCRE_BB_URL_BASE := https://github.com/JuliaPackaging/Yggdrasil/releases/download/PCRE2-v$(PCRE_VER)-$(PCRE_BB_REL)
PCRE_BB_NAME := PCRE2.v$(PCRE_VER).0

$(eval $(call bb-install,pcre,PCRE,false))

endif
