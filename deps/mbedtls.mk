## mbedtls

ifneq ($(USE_BINARYBUILDER_MBEDTLS), 1)
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

ifneq (,$(findstring $(OS),Linux FreeBSD))
MBEDTLS_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

$(SRCCACHE)/$(MBEDTLS_SRC).tgz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(MBEDTLS_URL)

$(SRCCACHE)/$(MBEDTLS_SRC)/source-extracted: $(SRCCACHE)/$(MBEDTLS_SRC).tgz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	touch -c $(SRCCACHE)/$(MBEDTLS_SRC)/CMakeLists.txt # old target
	echo 1 > $@

$(BUILDDIR)/$(MBEDTLS_SRC)/build-configured: $(SRCCACHE)/$(MBEDTLS_SRC)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(MBEDTLS_OPTS)
	echo 1 > $@

$(BUILDDIR)/$(MBEDTLS_SRC)/build-compiled: $(BUILDDIR)/$(MBEDTLS_SRC)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/$(MBEDTLS_SRC)/build-checked: $(BUILDDIR)/$(MBEDTLS_SRC)/build-compiled
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
	mbedtls,$(MBEDTLS_SRC), \
	MBEDTLS_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libmbedx509.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedx509.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libmbedtls.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedx509.0.dylib @rpath/libmbedx509.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.3.dylib @rpath/libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedtls.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.3.dylib @rpath/libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedx509.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libmbedcrypto.$$(SHLIB_EXT) $$(build_shlibdir)/libmbedcrypto.$$(SHLIB_EXT)))


clean-mbedtls:
	-rm $(BUILDDIR)/$(MBEDTLS_SRC)/build-configured \
		$(BUILDDIR)/$(MBEDTLS_SRC)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(MBEDTLS_SRC) clean

distclean-mbedtls:
	-rm -rf $(SRCCACHE)/$(MBEDTLS_SRC).tgz \
		$(SRCCACHE)/$(MBEDTLS_SRC) \
		$(BUILDDIR)/$(MBEDTLS_SRC)


get-mbedtls: $(SRCCACHE)/$(MBEDTLS_SRC).tgz
extract-mbedtls: $(SRCCACHE)/$(MBEDTLS_SRC)/source-extracted
configure-mbedtls: $(BUILDDIR)/$(MBEDTLS_SRC)/build-configured
compile-mbedtls: $(BUILDDIR)/$(MBEDTLS_SRC)/build-compiled
# tests disabled since they are known to fail
fastcheck-mbedtls: #check-mbedtls
check-mbedtls: $(BUILDDIR)/$(MBEDTLS_SRC)/build-checked

# If we built our own libmbedtls, we need to generate a fake MbedTLS_jll package to load it in:
$(eval $(call jll-generate,MbedTLS_jll,libbmedtls=\"libmbedtls\",,c8ffd9c3-330d-5841-b78e-0817d7145fa1,))

else # USE_BINARYBUILDER_MBEDTLS

# Install MbedTLS_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,MbedTLS_jll))

endif
