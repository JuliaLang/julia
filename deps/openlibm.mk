## openlibm ##
OPENLIBM_GIT_URL := git://github.com/JuliaLang/openlibm.git
OPENLIBM_TAR_URL = https://api.github.com/repos/JuliaLang/openlibm/tarball/$1
$(eval $(call git-external,openlibm,OPENLIBM,,,$(BUILDDIR)))

OPENLIBM_FLAGS := ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC)

$(BUILDDIR)/$(OPENLIBM_SRC_DIR)/build-compiled: $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/source-extracted
	$(MAKE) -C $(dir $<) $(OPENLIBM_FLAGS) $(MAKE_COMMON)
	echo 1 > $@

$(build_prefix)/manifest/openlibm: $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/build-compiled
	$(call make-install,$(OPENLIBM_SRC_DIR),$(OPENLIBM_FLAGS))
	$(INSTALL_NAME_CMD)libopenlibm.$(SHLIB_EXT) $(build_shlibdir)/libopenlibm.$(SHLIB_EXT)
	echo $(OPENLIBM_SHA1) > $@

clean-openlibm:
	-rm $(build_prefix)/manifest/openlibm $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/build-compiled $(build_libdir)/libopenlibm.a
	-$(MAKE) -C $(BUILDDIR)/$(OPENLIBM_SRC_DIR) distclean $(OPENLIBM_FLAGS)

get-openlibm: $(OPENLIBM_SRC_FILE)
extract-openlibm: $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/source-extracted
configure-openlibm: extract-openlibm
compile-openlibm: $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/build-compiled
check-openlibm: compile-openlibm
install-openlibm: $(build_prefix)/manifest/openlibm
