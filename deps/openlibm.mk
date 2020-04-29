## openlibm ##


OPENLIBM_GIT_URL := git://github.com/JuliaMath/openlibm.git
OPENLIBM_TAR_URL = https://api.github.com/repos/JuliaMath/openlibm/tarball/$1
$(eval $(call git-external,openlibm,OPENLIBM,,,$(BUILDDIR)))

OPENLIBM_FLAGS := ARCH="$(ARCH)" REAL_ARCH="$(MARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC)

ifneq ($(USE_BINARYBUILDER_OPENLIBM), 1)
$(BUILDDIR)/$(OPENLIBM_SRC_DIR)/build-compiled: $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/source-extracted
	$(MAKE) -C $(dir $<) $(OPENLIBM_FLAGS) $(MAKE_COMMON)
	echo 1 > $@

$(eval $(call staged-install, \
	openlibm,$$(OPENLIBM_SRC_DIR), \
	MAKE_INSTALL,$$(OPENLIBM_FLAGS),, \
	$(INSTALL_NAME_CMD)libopenlibm.$(SHLIB_EXT) $(build_shlibdir)/libopenlibm.$(SHLIB_EXT)))

clean-openlibm:
	-rm $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/build-compiled $(build_libdir)/libopenlibm.a
	-$(MAKE) -C $(BUILDDIR)/$(OPENLIBM_SRC_DIR) distclean $(OPENLIBM_FLAGS)


get-openlibm: $(OPENLIBM_SRC_FILE)
extract-openlibm: $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/source-extracted
configure-openlibm: extract-openlibm
compile-openlibm: $(BUILDDIR)/$(OPENLIBM_SRC_DIR)/build-compiled
fastcheck-openlibm: check-openlibm
check-openlibm: compile-openlibm

else # USE_BINARYBUILDER_OPENLIBM

OPENLIBM_BB_URL_BASE := https://github.com/JuliaBinaryWrappers/OpenLibm_jll.jl/releases/download/OpenLibm-v$(OPENLIBM_VER)+$(OPENLIBM_BB_REL)
OPENLIBM_BB_NAME := OpenLibm.v$(OPENLIBM_VER)

$(eval $(call bb-install,openlibm,OPENLIBM,false))
endif
