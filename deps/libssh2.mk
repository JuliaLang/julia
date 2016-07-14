## libssh2

LIBSSH2_GIT_URL := git://github.com/wildart/libssh2.git
LIBSSH2_TAR_URL = https://api.github.com/repos/wildart/libssh2/tarball/$1
$(eval $(call git-external,libssh2,LIBSSH2,CMakeLists.txt,build/libssh2.$(SHLIB_EXT),$(SRCDIR)/srccache))

LIBSSH2_OBJ_SOURCE := $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/src/libssh2.$(SHLIB_EXT)
LIBSSH2_OBJ_TARGET := $(build_shlibdir)/libssh2.$(SHLIB_EXT)

LIBSSH2_OPTS := $(CMAKE_COMMON) -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=OFF \
				-DCMAKE_PREFIX_PATH=$(build_prefix) \
				-DCMAKE_INSTALL_RPATH=$(build_prefix) -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE \
				-DCMAKE_BUILD_TYPE=Release

ifeq ($(OS),WINNT)
LIBSSH2_OPTS += -DCRYPTO_BACKEND=WinCNG -DENABLE_ZLIB_COMPRESSION=OFF
ifeq ($(BUILD_OS),WINNT)
LIBSSH2_OPTS += -G"MSYS Makefiles"
endif
else
LIBSSH2_OPTS += -DCRYPTO_BACKEND=mbedTLS -DENABLE_ZLIB_COMPRESSION=ON
endif

$(BUILDDIR)/$(LIBSSH2_SRC_DIR)/Makefile: $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/CMakeLists.txt $(MBEDTLS_OBJ_TARGET)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(LIBSSH2_OPTS)
	touch -c $@

$(LIBSSH2_OBJ_SOURCE): $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<) libssh2
	touch -c $@

$(BUILDDIR)/$(LIBSSH2_SRC_DIR)/checked: $(LIBSSH2_OBJ_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

$(LIBSSH2_OBJ_TARGET): $(LIBSSH2_OBJ_SOURCE) | $(build_shlibdir)
ifeq ($(OS),WINNT)
	$(MAKE) -C $(BUILDDIR)/$(LIBSSH2_SRC_DIR) install
else
	$(call make-install,$(LIBSSH2_SRC_DIR),)
endif
	touch -c $(LIBSSH2_OBJ_TARGET)

clean-libssh2:
	-rm -rf $(BUILDDIR)/$(LIBSSH2_SRC_DIR)
	-rm -f $(LIBSSH2_OBJ_TARGET)

get-libssh2: $(LIBSSH2_SRC_FILE)
configure-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/Makefile
compile-libssh2: $(LIBSSH2_OBJ_SOURCE)
check-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/checked
install-libssh2: $(LIBSSH2_OBJ_TARGET)
