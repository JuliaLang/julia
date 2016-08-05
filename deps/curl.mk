## CURL ##

ifeq ($(USE_SYSTEM_LIBSSH2), 0)
$(BUILDDIR)/curl-$(CURL_VER)/build-configured: $(build_prefix)/manifest/libssh2
endif

ifeq ($(USE_SYSTEM_MBEDTLS), 0)
$(BUILDDIR)/curl-$(CURL_VER)/build-configured: $(build_prefix)/manifest/mbedtls
endif

CURL_LDFLAGS := $(RPATH_ESCAPED_ORIGIN)

$(SRCDIR)/srccache/curl-$(CURL_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://curl.haxx.se/download/curl-$(CURL_VER).tar.bz2

$(SRCDIR)/srccache/curl-$(CURL_VER)/source-extracted: $(SRCDIR)/srccache/curl-$(CURL_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $(notdir $<)
	echo 1 > $@

$(BUILDDIR)/curl-$(CURL_VER)/build-configured: $(SRCDIR)/srccache/curl-$(CURL_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) --includedir=$(build_includedir) \
		--without-ssl --without-gnutls --without-gssapi --without-zlib \
		--without-libidn --without-libmetalink --without-librtmp \
		--without-nghttp2 --without-nss --without-polarssl \
		--without-spnego --without-libpsl --disable-ares \
		--disable-ldap --disable-ldaps --without-zsh-functions-dir \
		--with-libssh2=$(build_prefix) --with-mbedtls=$(build_prefix) \
		CFLAGS="$(CFLAGS) $(CURL_CFLAGS)" LDFLAGS="$(LDFLAGS) $(CURL_LDFLAGS)"
	echo 1 > $@

$(BUILDDIR)/curl-$(CURL_VER)/build-compiled: $(BUILDDIR)/curl-$(CURL_VER)/build-configured
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	echo 1 > $@

$(BUILDDIR)/curl-$(CURL_VER)/build-checked: $(BUILDDIR)/curl-$(CURL_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(build_prefix)/manifest/curl: $(BUILDDIR)/curl-$(CURL_VER)/build-compiled | $(build_prefix)/manifest
	$(call make-install,curl-$(CURL_VER),$(LIBTOOL_CCLD))
	$(INSTALL_NAME_CMD)libcurl.$(SHLIB_EXT) $(build_shlibdir)/libcurl.$(SHLIB_EXT)
	echo $(CURL_VER) > $@

clean-curl:
	-rm -rf $(build_prefix)/manifest/curl \
		$(BUILDDIR)/curl-$(CURL_VER)/build-configured $(BUILDDIR)/curl-$(CURL_VER)/build-compiled
	-rm -f $(build_shlibdir)/libcurl*
	-$(MAKE) -C $(BUILDDIR)/curl-$(CURL_VER) clean

distclean-curl:
	-rm -rf $(SRCDIR)/srccache/curl-$(CURL_VER).tar.bz2 $(SRCDIR)/srccache/curl-$(CURL_VER) $(BUILDDIR)/curl-$(CURL_VER)

get-curl: $(SRCDIR)/srccache/curl-$(CURL_VER).tar.bz2
extract-curl: $(SRCDIR)/srccache/curl-$(CURL_VER)/source-extracted
configure-curl: $(BUILDDIR)/curl-$(CURL_VER)/build-configured
compile-curl: $(BUILDDIR)/curl-$(CURL_VER)/build-compiled
check-curl: $(BUILDDIR)/curl-$(CURL_VER)/build-checked
install-curl: $(build_prefix)/manifest/curl
