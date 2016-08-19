## CURL ##

CURL_SRC_TARGET := $(BUILDDIR)/curl-$(CURL_VER)/lib/.libs/libcurl.$(SHLIB_EXT)
CURL_OBJ_TARGET := $(build_shlibdir)/libcurl.$(SHLIB_EXT)

CURL_LDFLAGS := $(RPATH_ESCAPED_ORIGIN)

$(SRCDIR)/srccache/curl-$(CURL_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://curl.haxx.se/download/curl-$(CURL_VER).tar.bz2

$(SRCDIR)/srccache/curl-$(CURL_VER)/configure: $(SRCDIR)/srccache/curl-$(CURL_VER).tar.bz2 $(MBEDTLS_OBJ_TARGET) $(LIBSSH2_OBJ_TARGET)
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $(notdir $<)
	touch -c $@

$(BUILDDIR)/curl-$(CURL_VER)/config.status: $(SRCDIR)/srccache/curl-$(CURL_VER)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) --includedir=$(build_includedir) \
		--without-ssl --without-gnutls --without-gssapi --without-zlib \
		--without-libidn --without-libmetalink --without-librtmp \
		--without-nghttp2 --without-nss --without-polarssl \
		--without-spnego --without-libpsl --disable-ares \
		--disable-ldap --disable-ldaps --without-zsh-functions-dir \
		--with-libssh2=$(build_prefix) --with-mbedtls=$(build_prefix) \
		CFLAGS="$(CFLAGS) $(CURL_CFLAGS)" LDFLAGS="$(LDFLAGS) $(CURL_LDFLAGS)"
	touch -c $@

$(CURL_SRC_TARGET): $(BUILDDIR)/curl-$(CURL_VER)/config.status
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	touch -c $@

$(BUILDDIR)/curl-$(CURL_VER)/checked: $(CURL_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
ifneq ($(OS),WINNT)
	$(MAKE) -C $(dir $@) check -j1
endif
endif
	echo 1 > $@

$(CURL_OBJ_TARGET): $(CURL_SRC_TARGET)
	$(call make-install,curl-$(CURL_VER),$(LIBTOOL_CCLD))
	$(INSTALL_NAME_CMD)libcurl.$(SHLIB_EXT) $@
	touch -c $@

clean-curl:
	-$(MAKE) -C $(BUILDDIR)/curl-$(CURL_VER) clean
	-rm -f $(build_shlibdir)/libcurl*
distclean-curl:
	-rm -rf $(SRCDIR)/srccache/curl-$(CURL_VER).tar.bz2 $(SRCDIR)/srccache/curl-$(CURL_VER) $(BUILDDIR)/curl-$(CURL_VER)

get-curl: $(SRCDIR)/srccache/curl-$(CURL_VER).tar.bz2
configure-curl: $(BUILDDIR)/curl-$(CURL_VER)/config.status
compile-curl: $(CURL_SRC_TARGET)
check-curl: $(BUILDDIR)/curl-$(CURL_VER)/checked
install-curl: $(CURL_OBJ_TARGET)
