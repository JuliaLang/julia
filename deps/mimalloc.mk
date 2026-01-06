
ifneq ($(USE_BINARYBUILDER_MIMALLOC), 1)
MIMALLOC_GIT_URL := https://github.com/microsoft/mimalloc.git
MIMALLOC_TAR_URL = https://api.github.com/repos/microsoft/mimalloc/tarball/$1
$(eval $(call git-external,mimalloc,MIMALLOC,,,$(SRCCACHE)))

# use Dynamic TLS because testing for musl is hard
MIMALLOC_BUILD_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release -DMI_BUILD_OBJECT=OFF
MIMALLOC_BUILD_OPTS += -DMI_INSTALL_TOPLEVEL=ON -DMI_BUILD_TESTS=OFF -DMI_OVERRIDE=OFF -DMI_LOCAL_DYNAMIC_TLS=ON

$(BUILDDIR)/$(MIMALLOC_SRC_DIR)/build-configured: $(SRCCACHE)/$(MIMALLOC_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && $(CMAKE) $(MIMALLOC_BUILD_OPTS) $(dir $<)
	echo 1 > $@

$(BUILDDIR)/$(MIMALLOC_SRC_DIR)/build-compiled: $(BUILDDIR)/$(MIMALLOC_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON)
	echo 1 > $@

$(eval $(call staged-install, \
	mimalloc,$(MIMALLOC_SRC_DIR), \
	MAKE_INSTALL,,, \
	$(INSTALL_NAME_CMD)libmimalloc.$(SHLIB_EXT) $(build_shlibdir)/libmimalloc.$(SHLIB_EXT)))

clean-mimalloc:
	-rm -f $(BUILDDIR)/$(MIMALLOC_SRC_DIR)/build-configured $(BUILDDIR)/$(MIMALLOC_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(MIMALLOC_SRC_DIR) clean

get-mimalloc: $(MIMALLOC_SRC_FILE)
extract-mimalloc: $(BUILDDIR)/$(MIMALLOC_SRC_DIR)/source-extracted
configure-mimalloc: $(BUILDDIR)/$(MIMALLOC_SRC_DIR)/build-configured
compile-mimalloc: $(BUILDDIR)/$(MIMALLOC_SRC_DIR)/build-compiled
fastcheck-mimalloc: check-mimalloc
check-mimalloc: compile-mimalloc

else # USE_BINARYBUILDER_MIMALLOC

$(eval $(call bb-install,mimalloc,MIMALLOC,false))

endif # USE_BINARYBUILDER_MIMALLOC
