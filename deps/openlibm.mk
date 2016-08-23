## openlibm ##
OPENLIBM_GIT_URL := git://github.com/JuliaLang/openlibm.git
OPENLIBM_TAR_URL = https://api.github.com/repos/JuliaLang/openlibm/tarball/$1
$(eval $(call git-external,openlibm,OPENLIBM,Makefile,libopenlibm.$(SHLIB_EXT),$(BUILDDIR)))

OPENLIBM_OBJ_TARGET := $(build_shlibdir)/libopenlibm.$(SHLIB_EXT) $(build_libdir)/libopenlibm.a
OPENLIBM_OBJ_SOURCE := $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/libopenlibm.$(SHLIB_EXT)
OPENLIBM_FLAGS := CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC)

$(OPENLIBM_OBJ_SOURCE): $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<) $(OPENLIBM_FLAGS) $(MAKE_COMMON)
	touch -c $@
$(build_shlibdir)/libopenlibm%$(SHLIB_EXT) $(build_libdir)/libopenlibm%a: $(OPENLIBM_OBJ_SOURCE)
	$(call make-install,$(OPENLIBM_SRC_DIR),$(OPENLIBM_FLAGS))
	$(INSTALL_NAME_CMD)libopenlibm.$(SHLIB_EXT) $(build_shlibdir)/libopenlibm.$(SHLIB_EXT)
	touch -c $(OPENLIBM_OBJ_TARGET)

clean-openlibm:
	-$(MAKE) -C $(BUILDDIR)/$(OPENLIBM_SRC_DIR) distclean $(OPENLIBM_FLAGS)
	-rm -f $(OPENLIBM_OBJ_TARGET)

get-openlibm: $(OPENLIBM_SRC_FILE)
configure-openlibm: $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/Makefile
compile-openlibm: $(OPENLIBM_OBJ_SOURCE)
check-openlibm: compile-openlibm
install-openlibm: $(OPENLIBM_OBJ_TARGET)
