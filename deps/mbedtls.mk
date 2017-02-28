## mbedtls

ifeq ($(USE_GPL_LIBS), 1)
MBEDTLS_SRC = mbedtls-$(MBEDTLS_VER)-gpl
else
MBEDTLS_SRC = mbedtls-$(MBEDTLS_VER)-apache
endif
MBEDTLS_URL = https://tls.mbed.org/download/$(MBEDTLS_SRC).tgz

MBEDTLS_OPTS := $(CMAKE_COMMON) -DUSE_SHARED_MBEDTLS_LIBRARY=ON \
    -DUSE_STATIC_MBEDTLS_LIBRARY=OFF -DENABLE_PROGRAMS=OFF -DCMAKE_BUILD_TYPE=Release

MBEDTLS_OPTS += -DENABLE_ZLIB_SUPPORT=OFF
ifeq ($(BUILD_OS),WINNT)
MBEDTLS_OPTS += -G"MSYS Makefiles"
endif

ifeq ($(OS),Linux)
MBEDTLS_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

ifeq ($(OS),FreeBSD)
MBEDTLS_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

$(SRCDIR)/srccache/$(MBEDTLS_SRC).tgz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ $(MBEDTLS_URL)

$(SRCDIR)/srccache/$(MBEDTLS_SRC)/source-extracted: $(SRCDIR)/srccache/$(MBEDTLS_SRC).tgz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	touch -c $(SRCDIR)/srccache/$(MBEDTLS_SRC)/CMakeLists.txt # old target
	echo 1 > $@

$(SRCDIR)/srccache/$(MBEDTLS_SRC)/mbedtls-ssl.h.patch-applied: $(SRCDIR)/srccache/$(MBEDTLS_SRC)/source-extracted
	cd $(SRCDIR)/srccache/$(MBEDTLS_SRC)/include/mbedtls && patch -p0 -f < $(SRCDIR)/patches/mbedtls-ssl.h.patch
	echo 1 > $@
$(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-configured: $(SRCDIR)/srccache/$(MBEDTLS_SRC)/mbedtls-ssl.h.patch-applied

$(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-configured: $(SRCDIR)/srccache/$(MBEDTLS_SRC)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(MBEDTLS_OPTS)
	echo 1 > $@

$(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-compiled: $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-checked: $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

ifeq ($(OS),WINNT)
define MBEDTLS_INSTALL
	mkdir -p $2/$$(build_shlibdir)
	cp $1/library/libmbedcrypto.$$(SHLIB_EXT) $2/$$(build_shlibdir)
	cp $1/library/libmbedx509.$$(SHLIB_EXT) $2/$$(build_shlibdir)
	cp $1/library/libmbedtls.$$(SHLIB_EXT) $2/$$(build_shlibdir)
endef
else
define MBEDTLS_INSTALL
	$(call MAKE_INSTALL,$1,$2,)
endef
endif
$(eval $(call staged-install, \
	mbedtls,mbedtls-$(MBEDTLS_VER), \
	MBEDTLS_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libmbedx509.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedx509.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libmbedtls.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedx509.0.dylib @rpath/libmbedx509.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.0.dylib @rpath/libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.0.dylib @rpath/libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedx509.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedcrypto.$$(SHLIB_EXT)))


clean-mbedtls:
	-rm $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-configured \
		$(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/mbedtls-$(MBEDTLS_VER) clean

distclean-mbedtls:
	-rm -rf $(SRCDIR)/srccache/$(MBEDTLS_SRC).tgz \
		$(SRCDIR)/srccache/$(MBEDTLS_SRC) \
		$(BUILDDIR)/mbedtls-$(MBEDTLS_VER)


get-mbedtls: $(SRCDIR)/srccache/$(MBEDTLS_SRC).tgz
extract-mbedtls: $(SRCDIR)/srccache/$(MBEDTLS_SRC)/source-extracted
configure-mbedtls: $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-configured
compile-mbedtls: $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-compiled
# tests disabled since they are known to fail
fastcheck-mbedtls: #check-mbedtls
check-mbedtls: $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/build-checked
