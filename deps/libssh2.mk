## libssh2

LIBSSH2_GIT_URL := git://github.com/libssh2/libssh2.git
LIBSSH2_TAR_URL = https://api.github.com/repos/libssh2/libssh2/tarball/$1
$(eval $(call git-external,libssh2,LIBSSH2,CMakeLists.txt,build/libssh2.$(SHLIB_EXT),$(SRCDIR)/srccache))

LIBSSH2_OBJ_SOURCE := $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/src/libssh2.$(SHLIB_EXT)
LIBSSH2_OBJ_TARGET := $(build_shlibdir)/libssh2.$(SHLIB_EXT)

LIBSSH2_OPTS := $(CMAKE_COMMON) -DBUILD_SHARED_LIBS=ON -DBUILD_EXAMPLES=OFF \
				-DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_LIBDIR=lib

ifeq ($(OS),WINNT)
LIBSSH2_OPTS += -DCRYPTO_BACKEND=WinCNG -DENABLE_ZLIB_COMPRESSION=OFF
ifeq ($(BUILD_OS),WINNT)
LIBSSH2_OPTS += -G"MSYS Makefiles"
endif
else
LIBSSH2_OPTS += -DCRYPTO_BACKEND=mbedTLS -DENABLE_ZLIB_COMPRESSION=ON
endif

ifeq ($(OS),Linux)
LIBSSH2_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

$(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/libssh2-mbedtls.patch-applied: | $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/CMakeLists.txt
	cd $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libssh2-mbedtls.patch
	echo 1 > $@
$(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/libssh2-encryptedpem.patch-applied: | $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/CMakeLists.txt
	cd $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libssh2-encryptedpem.patch
	echo 1 > $@

$(BUILDDIR)/$(LIBSSH2_SRC_DIR)/Makefile: $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/CMakeLists.txt $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/libssh2-mbedtls.patch-applied $(SRCDIR)/srccache/$(LIBSSH2_SRC_DIR)/libssh2-encryptedpem.patch-applied $(MBEDTLS_OBJ_TARGET)
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
ifeq ($(BUILD_OS),WINNT)
	$(MAKE) -C $(BUILDDIR)/$(LIBSSH2_SRC_DIR) install
else
	$(call make-install,$(LIBSSH2_SRC_DIR),)
endif
	$(INSTALL_NAME_CMD)libssh2.$(SHLIB_EXT) $@
	touch -c $(LIBSSH2_OBJ_TARGET)

clean-libssh2:
	-$(MAKE) -C $(BUILDDIR)/$(LIBSSH2_SRC_DIR) clean
	-rm -f $(LIBSSH2_OBJ_TARGET)

get-libssh2: $(LIBSSH2_SRC_FILE)
configure-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/Makefile
compile-libssh2: $(LIBSSH2_OBJ_SOURCE)
check-libssh2: $(BUILDDIR)/$(LIBSSH2_SRC_DIR)/checked
install-libssh2: $(LIBSSH2_OBJ_TARGET)
