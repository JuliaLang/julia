## Zstd ##
ifneq ($(USE_BINARYBUILDER_ZSTD), 1)
ZSTD_GIT_URL := https://github.com/facebook/zstd.git
ZSTD_TAR_URL = https://api.github.com/repos/facebook/zstd/tarball/$1
$(eval $(call git-external,zstd,ZSTD,,,$(BUILDDIR)))

# Like `7z`, the `zstd` and `zstdmt` executables live in `build_bindir` and are
# copied to `private_libexecdir` by `make install`.
ZSTD_BUILD_OPTS := MOREFLAGS="-DZSTD_MULTITHREAD $(fPIC)" bindir=$(build_bindir) libdir=$(build_shlibdir)
ifeq ($(OS), WINNT)
# Zstd detects "Windows" not WINNT, ordinarily from the inherited $(OS), but it expects the
# override to be done using TARGET_SYSTEM.
ZSTD_BUILD_OPTS += TARGET_SYSTEM="Windows"
endif

$(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured: $(BUILDDIR)/$(ZSTD_SRC_DIR)/source-extracted
	echo 1 > $@

$(BUILDDIR)/$(ZSTD_SRC_DIR)/build-compiled: $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON) $(ZSTD_BUILD_OPTS)
	echo 1 > $@

$(eval $(call staged-install, \
	zstd,$(ZSTD_SRC_DIR), \
	MAKE_INSTALL,$(ZSTD_BUILD_OPTS) MT=1,, \
	$(INSTALL_NAME_CMD)libzstd.$(SHLIB_EXT) $(build_shlibdir)/libzstd.$(SHLIB_EXT)))

clean-zstd:
	-rm -f $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(ZSTD_SRC_DIR) $(MAKE_COMMON) $(ZSTD_BUILD_OPTS) clean

get-zstd: $(ZSTD_SRC_FILE)
extract-zstd: $(BUILDDIR)/$(ZSTD_SRC_DIR)/source-extracted
configure-zstd: $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured
compile-zstd: $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-compiled
fastcheck-zstd: check-zstd
check-zstd: compile-zstd

else # USE_BINARYBUILDER_ZSTD

$(eval $(call bb-install,zstd,ZSTD,false))

endif # USE_BINARYBUILDER_ZSTD
