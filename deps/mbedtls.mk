## mbedtls

MBEDTLS_GIT_URL := git://github.com/ARMmbed/mbedtls.git
MBEDTLS_TAR_URL = https://api.github.com/repos/ARMmbed/mbedtls/tarball/$1
$(eval $(call git-external,mbedtls,MBEDTLS,CMakeLists.txt,build/libmbedtls.$(SHLIB_EXT),$(SRCDIR)/srccache))

MBEDTLS_OBJ_SOURCE := $(BUILDDIR)/$(MBEDTLS_SRC_DIR)/library/libmbedtls.$(SHLIB_EXT) $(BUILDDIR)/$(MBEDTLS_SRC_DIR)/library/libmbedx509.$(SHLIB_EXT) $(BUILDDIR)/$(MBEDTLS_SRC_DIR)/library/libmbedcrypto.$(SHLIB_EXT)
MBEDTLS_OBJ_TARGET := $(build_shlibdir)/libmbedtls.$(SHLIB_EXT) $(build_shlibdir)/libmbedx509.$(SHLIB_EXT) $(build_shlibdir)/libmbedcrypto.$(SHLIB_EXT)

#-DCMAKE_PREFIX_PATH=$(build_prefix)
MBEDTLS_OPTS := $(CMAKE_COMMON) -DUSE_SHARED_MBEDTLS_LIBRARY=ON -DENABLE_PROGRAMS=OFF -DENABLE_ZLIB_SUPPORT=ON -DENABLE_TESTING=OFF
ifeq ($(OS),WINNT)
MBEDTLS_OPTS += -DCMAKE_BUILD_TYPE=RelWithDebInfo
else
MBEDTLS_OPTS += -DCMAKE_BUILD_TYPE=Release
endif

$(BUILDDIR)/$(MBEDTLS_SRC_DIR)/Makefile: $(SRCDIR)/srccache/$(MBEDTLS_SRC_DIR)/CMakeLists.txt
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(MBEDTLS_OPTS)
	touch -c $@

$(MBEDTLS_OBJ_SOURCE): $(BUILDDIR)/$(MBEDTLS_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<)
	touch -c $@

$(BUILDDIR)/$(MBEDTLS_SRC_DIR)/checked: $(MBEDTLS_OBJ_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

$(MBEDTLS_OBJ_TARGET): $(MBEDTLS_OBJ_SOURCE) | $(build_shlibdir)
	$(call make-install,$(MBEDTLS_SRC_DIR),)
	touch -c $(MBEDTLS_OBJ_TARGET)

clean-mbedtls:
	-rm -rf $(BUILDDIR)/$(MBEDTLS_SRC_DIR)
	-rm -f $(MBEDTLS_OBJ_TARGET)
distclean-mbedtls:
	-rm -rf $(SRCDIR)/srccache/mbedtls-$(MBEDTLS_SHA1).tar.gz \
		$(SRCDIR)/srccache/mbedtls-$(MBEDTLS_SHA1) \
		$(BUILDDIR)/mbedtls-$(MBEDTLS_SHA1)

get-mbedtls: $(MBEDTLS_SRC_FILE)
configure-mbedtls: $(BUILDDIR)/$(MBEDTLS_SRC_DIR)/Makefile
compile-mbedtls: $(MBEDTLS_OBJ_SOURCE)
check-mbedtls: $(BUILDDIR)/$(MBEDTLS_SRC_DIR)/checked
install-mbedtls: $(MBEDTLS_OBJ_TARGET)