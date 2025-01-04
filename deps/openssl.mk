## OpenSSL ##
include $(SRCDIR)/openssl.version

ifneq ($(USE_BINARYBUILDER_OPENSSL),1)

ifeq ($(OS),Darwin)
ifeq ($(APPLE_ARCH),arm64)
OPENSSL_TARGET := darwin64-arm64-cc
else
OPENSSL_TARGET := darwin64-x86_64-cc
endif
else ifeq ($(OS),WINNT)
ifeq ($(ARCH),x86_64)
OPENSSL_TARGET := mingw64
else
OPENSSL_TARGET := mingw
endif
else ifeq ($(OS),FreeBSD)
ifeq ($(ARCH),aarch64)
OPENSSL_TARGET := BSD-aarch64
else
OPENSSL_TARGET := BSD-x86_64
endif
else ifeq ($(OS),Linux)
ifeq ($(ARCH),x86_64)
OPENSSL_TARGET := linux-x86_64
else ifeq ($(ARCH),i686)
OPENSSL_TARGET := linux-x86
else ifeq ($(ARCH),arm)
OPENSSL_TARGET := linux-armv4
else ifeq ($(ARCH),aarch64)
OPENSSL_TARGET := linux-aarch64
else ifeq ($(ARCH),ppc64le)
OPENSSL_TARGET := linux-ppc64le
else ifeq ($(ARCH),powerpc64le)
OPENSSL_TARGET := linux-ppc64le
endif
else
OPENSSL_TARGET := unknown
endif

$(SRCCACHE)/openssl-$(OPENSSL_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://www.openssl.org/source/$(notdir $@)

$(SRCCACHE)/openssl-$(OPENSSL_VER)/source-extracted: $(SRCCACHE)/openssl-$(OPENSSL_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) -zxf $<
	touch -c $(SRCCACHE)/openssl-$(OPENSSL_VER)/configure # old target
	echo 1 > $@

checksum-openssl: $(SRCCACHE)/openssl-$(OPENSSL_VER).tar.gz
	$(JLCHECKSUM) $<

# We cannot use $(CONFIGURE_COMMON) in this step, because openssl's Configure scripts is picky
# and does not like that we pass make variables as arguments, it wants them in the environment
$(BUILDDIR)/openssl-$(OPENSSL_VER)/build-configured: $(SRCCACHE)/openssl-$(OPENSSL_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
        CC="$(CC) $(SANITIZE_OPTS)" CXX="$(CXX) $(SANITIZE_OPTS)" LDFLAGS="$(LDFLAGS) $(RPATH_ESCAPED_ORIGIN) $(SANITIZE_LDFLAGS)" \
	$(dir $<)/Configure shared --prefix=$(abspath $(build_prefix)) $(OPENSSL_TARGET)
	echo 1 > $@

$(BUILDDIR)/openssl-$(OPENSSL_VER)/build-compiled: $(BUILDDIR)/openssl-$(OPENSSL_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/openssl-$(OPENSSL_VER)/build-checked: $(BUILDDIR)/openssl-$(OPENSSL_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

$(eval $(call staged-install, \
	openssl,openssl-$(OPENSSL_VER), \
	MAKE_INSTALL,,, \
	$$(WIN_MAKE_HARD_LINK) $(build_bindir)/libcrypto-*.dll $(build_bindir)/libcrypto.dll && \
	$$(WIN_MAKE_HARD_LINK) $(build_bindir)/libssl-*.dll $(build_bindir)/libssl.dll && \
	$$(INSTALL_NAME_CMD)libcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libcrypto.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libssl.$$(SHLIB_EXT) $$(build_shlibdir)/libssl.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) $$(build_shlibdir)/libcrypto.3.dylib @rpath/libcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libssl.$$(SHLIB_EXT)))

clean-openssl:
	-rm -f $(BUILDDIR)/-openssl-$(OPENSSL_VER)/build-configured $(BUILDDIR)/-openssl-$(OPENSSL_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/-openssl-$(OPENSSL_VER) clean

distclean-openssl:
	rm -rf $(SRCCACHE)/-openssl-$(OPENSSL_VER).tar.gz \
		$(SRCCACHE)/-openssl-$(OPENSSL_VER) \
		$(BUILDDIR)/-openssl-$(OPENSSL_VER)

get-openssl: $(SRCCACHE)/openssl-$(OPENSSL_VER).tar.gz
extract-openssl: $(SRCCACHE)/openssl-$(OPENSSL_VER)/source-extracted
configure-openssl: $(BUILDDIR)/openssl-$(OPENSSL_VER)/build-configured
compile-openssl: $(BUILDDIR)/openssl-$(OPENSSL_VER)/build-compiled
fastcheck-openssl: check-openssl
check-openssl: $(BUILDDIR)/openssl-$(OPENSSL_VER)/build-checked

else # USE_BINARYBUILDER_OPENSSL

$(eval $(call bb-install,openssl,OPENSSL,false))

endif
