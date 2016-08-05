## libssh2

LIBSSH2_GIT_URL := git://github.com/libssh2/libssh2.git
LIBSSH2_TAR_URL = https://api.github.com/repos/libssh2/libssh2/tarball/$1
$(eval $(call git-external,libssh2,LIBSSH2,,,$(SRCDIR)/srccache))

ifeq ($(USE_SYSTEM_MBEDTLS), 0)
$(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-configured: | $(build_prefix)/manifest/mbedtls
endif

LIBSSH2_OPTS := $(CMAKE_COMMON) -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=OFF \
		-DCMAKE_BUILD_TYPE=Release

ifeq ($(OS),WINNT)
LIBSSH2_OPTS += -DCRYPTO_BACKEND=WinCNG -DENABLE_ZLIB_COMPRESSION=OFF
ifeq ($(BUILD_OS),WINNT)
LIBSSH2_OPTS += -G"MSYS Makefiles"
endif
else
LIBSSH2_OPTS += -DCRYPTO_BACKEND=mbedTLS -DENABLE_ZLIB_COMPRESSION=OFF
endif

ifeq ($(OS),Linux)
LIBSSH2_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

$(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/libssh2-mbedtls.patch-applied: $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/source-extracted
	cd $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libssh2-mbedtls.patch
	echo 1 > $@

$(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/libssh2-encryptedpem.patch-applied: $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/source-extracted
	cd $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libssh2-encryptedpem.patch
	echo 1 > $@

$(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-configured: $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/source-extracted $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/libssh2-mbedtls.patch-applied $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/libssh2-encryptedpem.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(LIBSSH2_OPTS)
	echo 1 > $@

$(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<) libssh2
	echo 1 > $@

$(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-checked: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

$(build_prefix)/manifest/libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-compiled | $(build_shlibdir) $(build_prefix)/manifest
ifeq ($(BUILD_OS),WINNT)
	$(MAKE) -C $(BUILDDIR)/$(LIBSSH2_SRC_DIR) install
else
	$(call make-install,$(LIBSSH2_SRC_DIR),)
endif
	$(INSTALL_NAME_CMD)libssh2.$(SHLIB_EXT) $(build_shlibdir)/libssh2.$(SHLIB_EXT)
	echo $(LIBSSH2_SHA1) > $@

clean-libssh2:
	-rm -f $(build_prefix)/manifest/libssh2 $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBSSH2_SRC_DIR) clean

get-libssh2: $(LIBSSH2_SRC_FILE)
extract-libssh2: $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/source-extracted
configure-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-configured
compile-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-compiled
check-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-checked
install-libssh2: $(build_prefix)/manifest/libssh2
