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

# If we built our own libcurl, we need to generate a fake LibCURL_jll package to load it in:
$(eval $(call jll-generate,PCRE2_jll,libpcre=\"libpcre2-8\",,efcefdf7-47ab-520b-bdef-62a2eaa19f15,))

else # USE_BINARYBUILDER_PCRE

# Install PCRE2_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,PCRE2_jll))

# Fix naming mismatch (pcre2 vs. pcre)
$(build_prefix)/manifest/pcre: $(build_prefix)/manifest/pcre2
	cp "$<" "$@"
clean-pcre: clean-pcre2
distclean-pcre: distclean-pcre2
get-pcre: get-pcre2
extract-pcre: extract-pcre2
compile-pcre: compile-pcre2
install-pcre: install-pcre2 install-PCRE2_jll $(build_prefix)/manifest/pcre
UNINSTALL_pcre = $(UNINSTALL_pcre2)
uninstall-pcre: uninstall-pcre2
endif
