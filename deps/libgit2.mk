## libgit2

LIBGIT2_GIT_URL := git://github.com/libgit2/libgit2.git
LIBGIT2_TAR_URL = https://api.github.com/repos/libgit2/libgit2/tarball/$1
$(eval $(call git-external,libgit2,LIBGIT2,,,$(SRCDIR)/srccache))

ifeq ($(USE_SYSTEM_LIBSSH2), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: $(build_prefix)/manifest/libssh2
endif

ifneq ($(OS),WINNT)
ifeq ($(USE_SYSTEM_CURL), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: $(build_prefix)/manifest/curl
endif
endif

LIBGIT2_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release -DTHREADSAFE=ON
ifeq ($(OS),WINNT)
LIBGIT2_OPTS += -DWIN32=ON -DMINGW=ON
ifneq ($(ARCH),x86_64)
ifneq ($(USECLANG),1)
LIBGIT2_OPTS += -DCMAKE_C_FLAGS="-mincoming-stack-boundary=2"
endif
endif
ifeq ($(BUILD_OS),WINNT)
LIBGIT2_OPTS += -G"MSYS Makefiles"
else
LIBGIT2_OPTS += -DBUILD_CLAR=OFF -DDLLTOOL=`which $(CROSS_COMPILE)dlltool`
LIBGIT2_OPTS += -DCMAKE_FIND_ROOT_PATH=/usr/$(XC_HOST) -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY
endif
else
LIBGIT2_OPTS += -DCURL_INCLUDE_DIRS=$(build_includedir) -DCURL_LIBRARIES="-L$(build_shlibdir) -lcurl"
endif

ifeq ($(OS),Linux)
LIBGIT2_OPTS += -DCMAKE_INSTALL_RPATH="\$$ORIGIN"
endif

$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-ssh.patch-applied: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/source-extracted
	cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p0 -f < $(SRCDIR)/patches/libgit2-ssh.patch
	echo 1 > $@

$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-require-openssl.patch-applied: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/source-extracted | $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-ssh.patch-applied
	cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libgit2-require-openssl.patch
	echo 1 > $@

$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-agent-nonfatal.patch-applied: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/source-extracted | $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-require-openssl.patch-applied
	cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libgit2-agent-nonfatal.patch
	echo 1 > $@

$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-openssl-hang.patch-applied: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/source-extracted
	cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libgit2-openssl-hang.patch
	echo 1 > $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: \
	$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-require-openssl.patch-applied \
	$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-openssl-hang.patch-applied \
	$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-ssh.patch-applied \
	$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-agent-nonfatal.patch-applied

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(LIBGIT2_OPTS)
	echo 1 > $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-checked: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

$(build_prefix)/manifest/libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled | $(build_shlibdir) $(build_prefix)/manifest
ifeq ($(OS),WINNT)
	cp $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/libgit2.$(SHLIB_EXT) $(build_shlibdir)/libgit2.$(SHLIB_EXT)
else
	$(call make-install,$(LIBGIT2_SRC_DIR),)
endif
	$(INSTALL_NAME_CMD)libgit2.$(SHLIB_EXT) $(build_shlibdir)/libgit2.$(SHLIB_EXT)
ifeq ($(OS),Linux)
	@# If we're on linux, copy over libssl and libcrypto for libgit2
	-LIBGIT_LIBS=$$(ldd "$@" | tail -n +2 | awk '{print $$(NF-1)}'); \
	for LIB in libssl libcrypto; do \
		LIB_PATH=$$(echo "$$LIBGIT_LIBS" | grep "$$LIB"); \
		echo "LIB_PATH for $$LIB: $$LIB_PATH"; \
		[ ! -z "$$LIB_PATH" ] && cp -v -f "$$LIB_PATH" $(build_shlibdir); \
	done
endif
	echo $(LIBGIT2_SHA1) > $@

clean-libgit2:
	-rm -f $(build_prefix)/manifest/libgit2 $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBGIT2_SRC_DIR) clean

get-libgit2: $(LIBGIT2_SRC_FILE)
extract-libgit2: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/source-extracted
configure-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-configured
compile-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-compiled
check-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/build-checked
install-libgit2: $(build_prefix)/manifest/libgit2
