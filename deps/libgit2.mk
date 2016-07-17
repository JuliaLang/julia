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
LIBGIT2_OPTS += -DCMAKE_C_FLAGS="-mincoming-stack-boundary=2"
endif
ifeq ($(BUILD_OS),WINNT)
LIBGIT2_OPTS += -G"MSYS Makefiles"
else
LIBGIT2_OPTS += -DBUILD_CLAR=OFF -DDLLTOOL=`which $(CROSS_COMPILE)dlltool`
LIBGIT2_OPTS += -DCMAKE_FIND_ROOT_PATH=/usr/$(XC_HOST) -DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY
endif
endif

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/CMakeLists.txt $(LIBSSH2_OBJ_TARGET)
ifeq ($(OS),Linux)
	-cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p1 -f < $(SRCDIR)/patches/libgit2-require-openssl.patch
endif
	-cd $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR) && patch -p0 -f < $(SRCDIR)/patches/libgit2-ssh.patch
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
		[ ! -z "$$LIB_PATH" ] && cp -v "$$LIB_PATH" $(build_shlibdir); \
	done
endif
	touch -c $@

clean-libgit2:
	-$(MAKE) -C $(BUILDDIR)/$(LIBGIT2_SRC_DIR) clean
	-rm -f $(LIBGIT2_OBJ_TARGET)

get-libgit2: $(LIBGIT2_SRC_FILE)
configure-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile
compile-libgit2: $(LIBGIT2_OBJ_SOURCE)
check-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/checked
install-libgit2: $(LIBGIT2_OBJ_TARGET)
