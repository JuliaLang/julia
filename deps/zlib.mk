## Zlib ##
ifneq ($(USE_BINARYBUILDER_ZLIB), 1)
ZLIB_GIT_URL := git://github.com/madler/zlib.git
ZLIB_TAR_URL = https://api.github.com/repos/madler/zlib/tarball/$1
$(eval $(call git-external,zlib,ZLIB,,,$(SRCCACHE)))

$(BUILDDIR)/$(ZLIB_SRC_DIR)/build-configured: $(SRCCACHE)/$(ZLIB_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && $(CMAKE) -DCMAKE_INSTALL_PREFIX=$(abspath $(build_prefix)) -DCMAKE_BUILD_TYPE=Release -DUNIX=true $(dir $<)
	echo 1 > $@

$(BUILDDIR)/$(ZLIB_SRC_DIR)/build-compiled: $(BUILDDIR)/$(ZLIB_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON)
	echo 1 > $@

$(eval $(call staged-install, \
	zlib,$(ZLIB_SRC_DIR), \
	MAKE_INSTALL,,, \
	$(INSTALL_NAME_CMD)libz.$(SHLIB_EXT) $(build_shlibdir)/libz.$(SHLIB_EXT)))

clean-zlib:
	-rm $(BUILDDIR)/$(ZLIB_SRC_DIR)/build-compiled $(build_libdir)/libz.a* $(build_libdir)/libz.so* $(build_includedir)/zlib.h $(build_includedir)/zconf.h
	-$(MAKE) -C $(BUILDDIR)/$(ZLIB_SRC_DIR) distclean $(ZLIB_FLAGS)

get-zlib: $(ZLIB_SRC_FILE)
extract-zlib: $(BUILDDIR)/$(ZLIB_SRC_DIR)/source-extracted
configure-zlib: extract-zlib
compile-zlib: $(BUILDDIR)/$(ZLIB_SRC_DIR)/build-compiled
fastcheck-zlib: check-zlib
check-zlib: compile-zlib

else # USE_BINARYBUILDER_ZLIB

$(eval $(call bb-install,zlib,ZLIB,false))

endif # USE_BINARYBUILDER_ZLIB
