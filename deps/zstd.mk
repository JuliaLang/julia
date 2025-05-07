## Zstd ##
ifneq ($(USE_BINARYBUILDER_ZSTD), 1)
ZSTD_GIT_URL := https://github.com/facebook/zstd.git
ZSTD_TAR_URL = https://api.github.com/repos/facebook/zstd/tarball/$1
$(eval $(call git-external,zstd,ZSTD,,,$(BUILDDIR)))

ZSTD_BUILD_OPTS := MOREFLAGS="-DZSTD_MULTITHREAD $(fPIC)"

$(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured: $(BUILDDIR)/$(ZSTD_SRC_DIR)/source-extracted
	echo 1 > $@

$(BUILDDIR)/$(ZSTD_SRC_DIR)/build-compiled: $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON) $(ZSTD_BUILD_OPTS)
	echo 1 > $@

ZSTD_INSTALL=$(MAKE_INSTALL)

$(eval $(call staged-install, \
	zstd,$(ZSTD_SRC_DIR), \
	ZSTD_INSTALL,$(ZSTD_BUILD_OPTS) MT=1,, \
	$(INSTALL_NAME_CMD)libzstd.$(SHLIB_EXT) $(build_shlibdir)/libzstd.$(SHLIB_EXT)))

clean-zstd:
	-rm -f $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(ZSTD_SRC_DIR) $(ZSTD_BUILD_OPTS) clean

get-zstd: $(ZSTD_SRC_FILE)
extract-zstd: $(BUILDDIR)/$(ZSTD_SRC_DIR)/source-extracted
configure-zstd: $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured
compile-zstd: $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-compiled
fastcheck-zstd: check-zstd
check-zstd: compile-zstd

else # USE_BINARYBUILDER_ZSTD

$(eval $(call bb-install,zstd,ZSTD,false))

endif # USE_BINARYBUILDER_ZSTD
