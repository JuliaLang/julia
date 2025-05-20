## Zstd ##
ifneq ($(USE_BINARYBUILDER_ZSTD), 1)
ZSTD_GIT_URL := https://github.com/facebook/zstd.git
ZSTD_TAR_URL = https://api.github.com/repos/facebook/zstd/tarball/$1
$(eval $(call git-external,zstd,ZSTD,,,$(BUILDDIR)))

ZSTD_BUILD_OPTS := MOREFLAGS="-DZSTD_MULTITHREAD $(fPIC)" bindir=$(build_private_libexecdir)

$(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured: $(BUILDDIR)/$(ZSTD_SRC_DIR)/source-extracted
	echo 1 > $@

$(BUILDDIR)/$(ZSTD_SRC_DIR)/build-compiled: $(BUILDDIR)/$(ZSTD_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON) $(ZSTD_BUILD_OPTS)
	echo 1 > $@

$(eval $(call staged-install, \
	zstd,$(ZSTD_SRC_DIR), \
	MAKE_INSTALL,$(ZSTD_BUILD_OPTS) MT=1,, \
	$(INSTALL_NAME_CMD)libzstd.$(SHLIB_EXT) $(build_private_libexecdir)/libzstd.$(SHLIB_EXT)))

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
# move from bindir to shlibdir, where we expect to install it
install-zstd: post-install-zstd
uninstall-zstd: pre-uninstall-zstd
post-install-zstd: $(build_prefix)/manifest/zstd $(PATCHELF_MANIFEST)
	mkdir -p $(build_private_libexecdir)/
	[ ! -e $(build_bindir)/zstdmt$(EXE) ] || mv $(build_bindir)/zstdmt$(EXE) $(build_private_libexecdir)/zstdmt$(EXE)
	[ ! -e $(build_bindir)/zstd$(EXE) ] || mv $(build_bindir)/zstd$(EXE) $(build_private_libexecdir)/zstd$(EXE)
	[ -e $(build_private_libexecdir)/zstd$(EXE) ]
	[ -e $(build_private_libexecdir)/zstdmt$(EXE) ]
ifeq ($(OS), Darwin)
	for j in zstd zstdmt ; do \
		[ -L $(build_private_libexecdir)/$$j ] && continue; \
		install_name_tool -rpath @executable_path/$(reverse_build_private_libexecdir_rel) @loader_path/$(build_libdir_rel) $(build_private_libexecdir)/$$j 2>/dev/null || true; \
		install_name_tool -rpath @loader_path/$(build_libdir_rel) @executable_path/$(reverse_build_private_libexecdir_rel) $(build_private_libexecdir)/$$j || exit 1; \
	done
else ifneq (,$(findstring $(OS),Linux FreeBSD))
	for j in zstd zstdmt ; do \
		[ -L $(build_private_libexecdir)/$$j ] && continue; \
		$(PATCHELF) $(PATCHELF_SET_RPATH_ARG) '$$ORIGIN/$(reverse_build_private_libexecdir_rel)' $(build_private_libexecdir)/$$j || exit 1; \
	done
endif

pre-uninstall-zstd:
	-rm -f $(build_private_libexecdir)/zstd$(EXE) $(build_private_libexecdir)/zstdmt$(EXE)

endif # USE_BINARYBUILDER_ZSTD
