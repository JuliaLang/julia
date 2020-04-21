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

# If we built our own openlibm, we need to generate a fake OpenLibm_jll package to load it in:
$(eval $(call jll-generate,OpenLibm_jll,openlibm=\"libopenlibm\",,05823500-19ac-5b8b-9628-191a04bc5112,))

else # USE_BINARYBUILDER_OPENLIBM

# Install OpenLibm_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,OpenLibm_jll))
endif
