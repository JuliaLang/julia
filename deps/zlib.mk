## Zlib ##
ifneq ($(USE_BINARYBUILDER_ZLIB), 1)
ZLIB_GIT_URL := https://github.com/madler/zlib.git
ZLIB_TAR_URL = https://api.github.com/repos/madler/zlib/tarball/$1
$(eval $(call git-external,zlib,ZLIB,,,$(SRCCACHE)))

# use `-DUNIX=true` to ensure that it is always named `libz`
ZLIB_BUILD_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release -DUNIX=true
ZLIB_BUILD_OPTS += -DCMAKE_POSITION_INDEPENDENT_CODE=ON

$(BUILDDIR)/$(ZLIB_SRC_DIR)/build-configured: $(SRCCACHE)/$(ZLIB_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && $(CMAKE) $(ZLIB_BUILD_OPTS) $(dir $<)
	echo 1 > $@

$(BUILDDIR)/$(ZLIB_SRC_DIR)/build-compiled: $(BUILDDIR)/$(ZLIB_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON)
	echo 1 > $@

$(eval $(call staged-install, \
	zlib,$(ZLIB_SRC_DIR), \
	MAKE_INSTALL,,, \
	$(INSTALL_NAME_CMD)libz.$(SHLIB_EXT) $(build_shlibdir)/libz.$(SHLIB_EXT)))

clean-zlib:
	-rm -f $(BUILDDIR)/$(ZLIB_SRC_DIR)/build-configured $(BUILDDIR)/$(ZLIB_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(ZLIB_SRC_DIR) clean

get-zlib: $(ZLIB_SRC_FILE)
extract-zlib: $(BUILDDIR)/$(ZLIB_SRC_DIR)/source-extracted
configure-zlib: $(BUILDDIR)/$(ZLIB_SRC_DIR)/build-configured
compile-zlib: $(BUILDDIR)/$(ZLIB_SRC_DIR)/build-compiled
fastcheck-zlib: check-zlib
check-zlib: compile-zlib

else # USE_BINARYBUILDER_ZLIB

$(eval $(call bb-install,zlib,ZLIB,false))

endif # USE_BINARYBUILDER_ZLIB
