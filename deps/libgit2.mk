## libgit2

LIBGIT2_GIT_URL := git://github.com/wildart/libgit2.git
LIBGIT2_TAR_URL = https://api.github.com/repos/wildart/libgit2/tarball/$1
$(eval $(call git-external,libgit2,LIBGIT2,CMakeLists.txt,build/libgit2.$(SHLIB_EXT),$(SRCDIR)/srccache))

LIBGIT2_OBJ_SOURCE := $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/libgit2.$(SHLIB_EXT)
LIBGIT2_OBJ_TARGET := $(build_shlibdir)/libgit2.$(SHLIB_EXT)

LIBGIT2_OPTS := $(CMAKE_COMMON) -DUSE_OPENSSL=OFF -DTHREADSAFE=ON -DCMAKE_PREFIX_PATH=$(build_prefix) -DCMAKE_INSTALL_RPATH=$(build_prefix) -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE -DCMAKE_BUILD_TYPE=RelWithDebInfo

$(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile: $(SRCDIR)/srccache/$(LIBGIT2_SRC_DIR)/CMakeLists.txt
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
	$(call make-install,$(LIBGIT2_SRC_DIR),)
	touch -c $@

clean-libgit2:
	-rm -rf $(BUILDDIR)/$(LIBGIT2_SRC_DIR)
	-rm -f $(LIBGIT2_OBJ_TARGET)

get-libgit2: $(LIBGIT2_SRC_FILE)
configure-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/Makefile
compile-libgit2: $(LIBGIT2_OBJ_SOURCE)
check-libgit2: $(BUILDDIR)/$(LIBGIT2_SRC_DIR)/checked
install-libgit2: $(LIBGIT2_OBJ_TARGET)
