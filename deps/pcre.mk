## PCRE ##

PCRE_SRC_TARGET := $(BUILDDIR)/pcre2-$(PCRE_VER)/.libs/libpcre2-8.$(SHLIB_EXT)
PCRE_OBJ_TARGET := $(build_shlibdir)/libpcre2-8.$(SHLIB_EXT)

# Force optimization for PCRE flags (Issue #11668)
PCRE_CFLAGS := -O3
PCRE_LDFLAGS := $(RPATH_ESCAPED_ORIGIN)

$(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre2-$(PCRE_VER).tar.bz2
$(SRCDIR)/srccache/pcre2-$(PCRE_VER)/configure: $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $(notdir $<)
	touch -c $@
$(BUILDDIR)/pcre2-$(PCRE_VER)/config.status: $(SRCDIR)/srccache/pcre2-$(PCRE_VER)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) --enable-jit --includedir=$(build_includedir) CFLAGS="$(CFLAGS) $(PCRE_CFLAGS)" LDFLAGS="$(LDFLAGS) $(PCRE_LDFLAGS)"
	touch -c $@
$(PCRE_SRC_TARGET): $(BUILDDIR)/pcre2-$(PCRE_VER)/config.status
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	touch -c $@
$(BUILDDIR)/pcre2-$(PCRE_VER)/checked: $(PCRE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
ifneq ($(OS),WINNT)
	$(MAKE) -C $(dir $@) check -j1
endif
endif
	echo 1 > $@
$(PCRE_OBJ_TARGET): $(PCRE_SRC_TARGET)
	$(call make-install,pcre2-$(PCRE_VER),$(LIBTOOL_CCLD))
	$(INSTALL_NAME_CMD)libpcre2-8.$(SHLIB_EXT) $@
	touch -c $@

clean-pcre:
	-$(MAKE) -C $(BUILDDIR)/pcre2-$(PCRE_VER) clean
	-rm -f $(build_shlibdir)/libpcre*
distclean-pcre:
	-rm -rf $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2 $(SRCDIR)/srccache/pcre2-$(PCRE_VER) $(BUILDDIR)/pcre2-$(PCRE_VER)

get-pcre: $(SRCDIR)/srccache/pcre2-$(PCRE_VER).tar.bz2
configure-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/config.status
compile-pcre: $(PCRE_SRC_TARGET)
check-pcre: $(BUILDDIR)/pcre2-$(PCRE_VER)/checked
install-pcre: $(PCRE_OBJ_TARGET)
