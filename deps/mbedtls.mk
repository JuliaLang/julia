## mbedtls

ifeq ($(USE_GPL_LIBS), 1)
MBEDTLS_SRC = mbedtls-$(MBEDTLS_VER)-gpl
else
MBEDTLS_SRC = mbedtls-$(MBEDTLS_VER)-apache
endif
MBEDTLS_URL = https://tls.mbed.org/download/$(MBEDTLS_SRC).tgz

MBEDTLS_OBJ_SOURCE := $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/library/libmbedcrypto.$(SHLIB_EXT)
MBEDTLS_OBJ_TARGET := $(build_shlibdir)/libmbedcrypto.$(SHLIB_EXT)

MBEDTLS_OPTS := $(CMAKE_COMMON) -DUSE_SHARED_MBEDTLS_LIBRARY=ON \
		-DENABLE_PROGRAMS=OFF -DCMAKE_BUILD_TYPE=Release

ifeq ($(OS),WINNT)
MBEDTLS_OPTS += -DENABLE_ZLIB_SUPPORT=OFF
ifeq ($(BUILD_OS),WINNT)
MBEDTLS_OPTS += -G"MSYS Makefiles"
endif
else
MBEDTLS_OPTS += -DENABLE_ZLIB_SUPPORT=ON
endif

ifeq ($(OS),Linux)
MBEDTLS_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

$(SRCDIR)/srccache/$(MBEDTLS_SRC).tgz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ $(MBEDTLS_URL)

$(SRCDIR)/srccache/$(MBEDTLS_SRC)/CMakeLists.txt: $(SRCDIR)/srccache/$(MBEDTLS_SRC).tgz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	touch -c $@

$(SRCDIR)/srccache/$(MBEDTLS_SRC)/mbedtls-ssl.h.patch-applied: | $(SRCDIR)/srccache/$(MBEDTLS_SRC)/CMakeLists.txt
	cd $(SRCDIR)/srccache/$(MBEDTLS_SRC)/include/mbedtls && patch -p0 -f < $(SRCDIR)/patches/mbedtls-ssl.h.patch
	echo 1 > $@

$(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/Makefile: $(SRCDIR)/srccache/$(MBEDTLS_SRC)/CMakeLists.txt $(SRCDIR)/srccache/$(MBEDTLS_SRC)/mbedtls-ssl.h.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(MBEDTLS_OPTS)
	touch -c $@

$(MBEDTLS_OBJ_SOURCE): $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/Makefile
	$(MAKE) -C $(dir $<)
	touch -c $@

$(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/checked: $(MBEDTLS_OBJ_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

$(MBEDTLS_OBJ_TARGET): $(MBEDTLS_OBJ_SOURCE) | $(build_shlibdir)
ifeq ($(BUILD_OS), WINNT)
	cp $^ $(build_shlibdir)
	cp $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/library/libmbedx509.$(SHLIB_EXT) $(build_shlibdir)
	cp $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/library/libmbedtls.$(SHLIB_EXT) $(build_shlibdir)
else
	$(call make-install,mbedtls-$(MBEDTLS_VER),)
endif
	$(INSTALL_NAME_CMD)libmbedx509.$(SHLIB_EXT) $(build_shlibdir)/libmbedx509.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libmbedtls.$(SHLIB_EXT) $(build_shlibdir)/libmbedtls.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) libmbedx509.0.dylib @rpath/libmbedx509.$(SHLIB_EXT) $(build_shlibdir)/libmbedtls.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.0.dylib @rpath/libmbedcrypto.$(SHLIB_EXT) $(build_shlibdir)/libmbedtls.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) libmbedcrypto.0.dylib @rpath/libmbedcrypto.$(SHLIB_EXT) $(build_shlibdir)/libmbedx509.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libmbedcrypto.$(SHLIB_EXT) $@
	touch -c $(MBEDTLS_OBJ_TARGET)

clean-mbedtls:
	-$(MAKE) -C $(BUILDDIR)/mbedtls-$(MBEDTLS_VER) clean
	-rm -f $(MBEDTLS_OBJ_TARGET) $(build_shlibdir)/libmbed*

distclean-mbedtls:
	-rm -rf $(SRCDIR)/srccache/$(MBEDTLS_SRC).tgz \
		$(SRCDIR)/srccache/$(MBEDTLS_SRC) \
		$(BUILDDIR)/mbedtls-$(MBEDTLS_VER)

get-mbedtls: $(SRCDIR)/srccache/$(MBEDTLS_SRC).tgz
configure-mbedtls: $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/Makefile
compile-mbedtls: $(MBEDTLS_OBJ_SOURCE)
check-mbedtls: $(BUILDDIR)/mbedtls-$(MBEDTLS_VER)/checked
install-mbedtls: $(MBEDTLS_OBJ_TARGET)
