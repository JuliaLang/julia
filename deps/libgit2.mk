## libgit2

LIBGIT2_GIT_URL := git://github.com/libgit2/libgit2.git
LIBGIT2_TAR_URL = https://api.github.com/repos/libgit2/libgit2/tarball/$1
$(eval $(call git-external,libgit2,LIBGIT2,CMakeLists.txt,build/libgit2.$(SHLIB_EXT),$(SRCDIR)/srccache))

LIBGIT2_OBJ_SOURCE := $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/libgit2.$(SHLIB_EXT)
LIBGIT2_OBJ_TARGET := $(build_shlibdir)/libgit2.$(SHLIB_EXT)

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

$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-ssh.patch-applied: | $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/CMakeLists.txt
	cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p0 -f < $(SRCDIR)/patches/libgit2-ssh.patch
	echo 1 > $@

$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-require-openssl.patch-applied: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-ssh.patch-applied | $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/CMakeLists.txt
	cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libgit2-require-openssl.patch
	echo 1 > $@

$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-agent-nonfatal.patch-applied: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-ssh.patch-applied | $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/CMakeLists.txt
	cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libgit2-agent-nonfatal.patch
	echo 1 > $@

$(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-openssl-hang.patch-applied: | $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/CMakeLists.txt
	cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libgit2-openssl-hang.patch
	echo 1 > $@

ifeq ($(OS),Linux)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-require-openssl.patch-applied $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-openssl-hang.patch-applied
endif
ifneq ($(OS),WINNT)
ifeq ($(USE_SYSTEM_CURL), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile: $(CURL_OBJ_TARGET)
endif
endif
ifeq ($(USE_SYSTEM_LIBSSH2), 0)
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile: $(LIBSSH2_OBJ_TARGET)
endif
$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/CMakeLists.txt $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-ssh.patch-applied $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/libgit2-agent-nonfatal.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(CMAKE) $(dir $<) $(LIBGIT2_OPTS)
	touch -c $@

$(LIBGIT2_OBJ_SOURCE): $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<)
	touch -c $@

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/checked: $(LIBGIT2_OBJ_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) test
endif
	echo 1 > $@

$(LIBGIT2_OBJ_TARGET): $(LIBGIT2_OBJ_SOURCE) | $(build_shlibdir)
ifeq ($(BUILD_OS),WINNT)
	cp $< $@
else
	$(call make-install,$(LIBGIT2_SRC_DIR),)
endif
	$(INSTALL_NAME_CMD)libgit2.$(SHLIB_EXT) $@
ifeq ($(OS),Linux)
	@# If we're on linux, copy over libssl and libcrypto for libgit2
	-LIBGIT_LIBS=$$(ldd "$@" | tail -n +2 | awk '{print $$(NF-1)}'); \
	for LIB in libssl libcrypto; do \
		LIB_PATH=$$(echo "$$LIBGIT_LIBS" | grep "$$LIB"); \
		echo "LIB_PATH for $$LIB: $$LIB_PATH"; \
		[ ! -z "$$LIB_PATH" ] && cp -v -f "$$LIB_PATH" $(build_shlibdir); \
	done
endif
	touch -c $@

clean-libgit2:
	-$(MAKE) -C $(BUILDDIR)/$(LIBGIT2_SRC_DIR) clean
	-rm -f $(build_shlibdir)/libgit2* $(build_shlibdir)/libssl* $(build_shlibdir)/libcrypto*

get-libgit2: $(LIBGIT2_SRC_FILE)
configure-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile
compile-libgit2: $(LIBGIT2_OBJ_SOURCE)
check-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/checked
install-libgit2: $(LIBGIT2_OBJ_TARGET)
