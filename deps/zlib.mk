ZLIB_GIT_URL := git://github.com/madler/zlib.git
ZLIB_TAR_URL = https://api.github.com/repos/madler/zlib/tarball/$1
$(eval $(call git-external,zlib,ZLIB,,,$(SRCCACHE)))

ifneq ($(USE_BINARYBUILDER_ZLIB), 1)
$(BUILDDIR)/$(ZLIB_SRC_DIR)/build-configured: $(SRCCACHE)/$(ZLIB_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && $(dir $<)/configure --prefix=$(abspath $(build_prefix)) --libdir=$(abspath $(build_libdir))
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

ZLIB_BB_URL_BASE := https://github.com/JuliaBinaryWrappers/Zlib_jll.jl/releases/download/Zlib-v$(ZLIB_VER)+$(ZLIB_BB_REL)
ZLIB_BB_NAME := Zlib.v$(ZLIB_VER)
$(eval $(call bb-install,zlib,ZLIB,false))

endif # USE_BINARYBUILDER_ZLIB
