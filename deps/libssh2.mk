## libssh2

LIBSSH2_GIT_URL := git://github.com/libssh2/libssh2.git
LIBSSH2_TAR_URL = https://api.github.com/repos/libssh2/libssh2/tarball/$1
$(eval $(call git-external,libssh2,LIBSSH2,CMakeLists.txt,,$(SRCCACHE)))

$(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-configured: | $(build_prefix)/manifest/mbedtls

ifneq ($(USE_BINARYBUILDER_LIBSSH2), 1)
LIBSSH2_OPTS = $(CMAKE_COMMON) -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=OFF \
		-DCMAKE_BUILD_TYPE=Release

ifeq ($(OS),WINNT)
LIBSSH2_OPTS += -DCRYPTO_BACKEND=WinCNG -DENABLE_ZLIB_COMPRESSION=OFF
ifeq ($(BUILD_OS),WINNT)
LIBSSH2_OPTS += -G"MSYS Makefiles"
endif
else
LIBSSH2_OPTS += -DCRYPTO_BACKEND=mbedTLS -DENABLE_ZLIB_COMPRESSION=OFF
LIBSSH2_OPTS += "-DMBEDTLS_INCLUDE_DIR=$(MBEDTLS_INCDIR)"
LIBSSH2_OPTS += "-DCMAKE_LIBRARY_PATH=$(MBEDTLS_LIBDIR)"
endif

ifneq (,$(findstring $(OS),Linux FreeBSD))
LIBSSH2_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

ifeq ($(LIBSSH2_ENABLE_TESTS), 0)
LIBSSH2_OPTS += -DBUILD_TESTING=OFF
endif

$(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-configured: $(SRCCACHE)/$(LIBSSH2_SRC_DIR)/source-extracted
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

$(eval $(call staged-install, \
	libssh2,$(LIBSSH2_SRC_DIR), \
	MAKE_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libssh2.$$(SHLIB_EXT) $$(build_shlibdir)/libssh2.$$(SHLIB_EXT)))

clean-libssh2:
	-rm $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBSSH2_SRC_DIR) clean


get-libssh2: $(LIBSSH2_SRC_FILE)
extract-libssh2: $(SRCCACHE)/$(LIBSSH2_SRC_DIR)/source-extracted
configure-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-configured
compile-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-compiled
fastcheck-libssh2: check-libssh2
check-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/build-checked

# If we built our own libssh2, we need to generate a fake LibSSH2_jll package to load it in:
$(eval $(call jll-generate,LibSSH2_jll,libssh2=\"libssh2\",,29816b5a-b9ab-546f-933c-edad1886dfa8, \
		                   MbedTLS_jll=c8ffd9c3-330d-5841-b78e-0817d7145fa1))

else # USE_BINARYBUILDER_LIBSSH2

# Install LibSSH2_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,LibSSH2_jll))

endif
