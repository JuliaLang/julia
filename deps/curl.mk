## CURL ##
include $(SRCDIR)/curl.version

ifeq ($(USE_SYSTEM_LIBSSH2), 0)
$(BUILDDIR)/curl-$(CURL_VER)/build-configured: | $(build_prefix)/manifest/libssh2
endif

ifeq ($(USE_SYSTEM_ZLIB), 0)
$(BUILDDIR)/curl-$(CURL_VER)/build-configured: | $(build_prefix)/manifest/zlib
endif

ifeq ($(USE_SYSTEM_NGHTTP2), 0)
$(BUILDDIR)/curl-$(CURL_VER)/build-configured: | $(build_prefix)/manifest/nghttp2
endif

ifneq ($(USE_BINARYBUILDER_CURL),1)
CURL_LDFLAGS := $(RPATH_ESCAPED_ORIGIN)

# On older Linuces (those that use OpenSSL < 1.1) we include `libpthread` explicitly.
# It doesn't hurt to include it explicitly elsewhere, so we do so.
ifeq ($(OS),Linux)
CURL_LDFLAGS += -lpthread
endif

$(SRCCACHE)/curl-$(CURL_VER).tar.bz2: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://curl.se/download/curl-$(CURL_VER).tar.bz2

$(SRCCACHE)/curl-$(CURL_VER)/source-extracted: $(SRCCACHE)/curl-$(CURL_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $(notdir $<)
	echo 1 > $@

checksum-curl: $(SRCCACHE)/curl-$(CURL_VER).tar.bz2
	$(JLCHECKSUM) $<

## xref: https://github.com/JuliaPackaging/Yggdrasil/blob/master/L/LibCURL/common.jl
# Disable....almost everything
CURL_CONFIGURE_FLAGS := $(CONFIGURE_COMMON) \
	--without-ssl --without-gnutls --without-libidn2 --without-librtmp \
	--without-nss --without-libpsl --without-libgsasl --without-fish-functions-dir \
	--disable-ares --disable-manual --disable-ldap --disable-ldaps --disable-static
# A few things we actually enable
CURL_CONFIGURE_FLAGS += --enable-versioned-symbols \
	--with-libssh2=${build_prefix} --with-zlib=${build_prefix} --with-nghttp2=${build_prefix}
CURL_CONFIGURE_FLAGS += --without-gssapi

# We use different TLS libraries on different platforms.
#   On Windows, we use schannel
#   On MacOS, we use SecureTransport
#   On Linux, we use mbedTLS
ifeq ($(OS), WINNT)
CURL_TLS_CONFIGURE_FLAGS := --with-schannel
else ifeq ($(OS), Darwin)
CURL_TLS_CONFIGURE_FLAGS := --with-secure-transport
else
CURL_TLS_CONFIGURE_FLAGS := --with-mbedtls=$(build_prefix)
endif
CURL_CONFIGURE_FLAGS += $(CURL_TLS_CONFIGURE_FLAGS)

$(BUILDDIR)/curl-$(CURL_VER)/build-configured: $(SRCCACHE)/curl-$(CURL_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CURL_CONFIGURE_FLAGS) \
		CFLAGS="$(CFLAGS) $(CURL_CFLAGS)" LDFLAGS="$(LDFLAGS) $(CURL_LDFLAGS)"
	echo 1 > $@

$(BUILDDIR)/curl-$(CURL_VER)/build-compiled: $(BUILDDIR)/curl-$(CURL_VER)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON)
	echo 1 > $@

$(BUILDDIR)/curl-$(CURL_VER)/build-checked: $(BUILDDIR)/curl-$(CURL_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	curl,curl-$$(CURL_VER), \
	MAKE_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libcurl.$$(SHLIB_EXT) $$(build_shlibdir)/libcurl.$$(SHLIB_EXT)))

clean-curl:
	-rm -f $(BUILDDIR)/curl-$(CURL_VER)/build-configured $(BUILDDIR)/curl-$(CURL_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/curl-$(CURL_VER) clean

distclean-curl:
	rm -rf $(SRCCACHE)/curl-$(CURL_VER).tar.bz2 $(SRCCACHE)/curl-$(CURL_VER) $(BUILDDIR)/curl-$(CURL_VER)

get-curl: $(SRCCACHE)/curl-$(CURL_VER).tar.bz2
extract-curl: $(SRCCACHE)/curl-$(CURL_VER)/source-extracted
configure-curl: $(BUILDDIR)/curl-$(CURL_VER)/build-configured
compile-curl: $(BUILDDIR)/curl-$(CURL_VER)/build-compiled
fastcheck-curl: #none
check-curl: $(BUILDDIR)/curl-$(CURL_VER)/build-checked

else # USE_BINARYBUILDER_CURL
$(eval $(call bb-install,curl,CURL,false))
endif
