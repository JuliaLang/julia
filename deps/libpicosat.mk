## libpicosat ##
include $(SRCDIR)/libpicosat.version

ifneq ($(USE_BINARYBUILDER_LIBPICOSAT), 1)

LIBPICOSAT_SRC_FILE := $(SRCCACHE)/picosat-$(LIBPICOSAT_VER).tar.gz
LIBPICOSAT_SRC_DIR := picosat-$(LIBPICOSAT_VER)

$(LIBPICOSAT_SRC_FILE): | $(SRCCACHE)
	$(JLDOWNLOAD) $@ $(LIBPICOSAT_URL)

$(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/source-extracted: $(LIBPICOSAT_SRC_FILE)
	$(JLCHECKSUM) $<
	mkdir -p $(BUILDDIR)
	$(TAR) -C $(BUILDDIR) -xf $<
	touch $@

$(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-configured: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $<) && ./configure.sh --shared
ifeq ($(OS),Darwin)
	# Fix macOS linker options
	cd $(dir $<) && sed -i.bak 's/-Xlinker -soname -Xlinker/-install_name/' makefile
endif
	echo 1 > $@

$(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON)
	echo 1 > $@

$(eval $(call staged-install, \
	libpicosat,$(LIBPICOSAT_SRC_DIR), \
	MAKE_INSTALL,,, \
	$(INSTALL_NAME_CMD)libpicosat.$(SHLIB_EXT) $(build_shlibdir)/libpicosat.$(SHLIB_EXT)))

clean-libpicosat:
	-rm -f $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR) clean

get-libpicosat: $(LIBPICOSAT_SRC_FILE)
extract-libpicosat: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/source-extracted
configure-libpicosat: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-configured
compile-libpicosat: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-compiled
fastcheck-libpicosat: check-libpicosat
check-libpicosat: compile-libpicosat

else # USE_BINARYBUILDER_LIBPICOSAT

$(eval $(call bb-install,libpicosat,LIBPICOSAT,false))

endif # USE_BINARYBUILDER_LIBPICOSAT
