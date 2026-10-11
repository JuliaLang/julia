## libpicosat ##
include $(SRCDIR)/libpicosat.version

ifneq ($(USE_BINARYBUILDER_LIBPICOSAT),1)

LIBPICOSAT_GIT_URL := https://github.com/JuliaLang/PicoSAT.git
LIBPICOSAT_TAR_URL = https://api.github.com/repos/JuliaLang/PicoSAT/tarball/$1
$(eval $(call git-external,libpicosat,LIBPICOSAT,,,$(BUILDDIR)))

# the flags libpicosat_jll is built with
LIBPICOSAT_CFLAGS := $(CFLAGS) $(fPIC) -DNDEBUG -O3 -DTRACE -DNGETRUSAGE -DNALLSIGNALS -shared $(SANITIZE_OPTS)

# PicoSAT's `configure.sh` and `mkconfig.sh` generate `config.h` from its own
# makefile and git checkout; only the version is needed, so write it directly
$(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-configured: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/source-extracted
	cd $(dir $@) && printf '%s\n' \
		'#define PICOSAT_CC ""' \
		'#define PICOSAT_CFLAGS ""' \
		'#define PICOSAT_VERSION "$(LIBPICOSAT_VER)"' > config.h
	echo 1 > $@

$(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-configured
	cd $(dir $@) && \
	$(CC) $(CPPFLAGS) $(LIBPICOSAT_CFLAGS) $(LDFLAGS) picosat.c version.c -o libpicosat.$(SHLIB_EXT)
	echo 1 > $@

$(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-checked: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-compiled
	echo 1 > $@

define LIBPICOSAT_INSTALL
	mkdir -p $2/$$(build_includedir)
	mkdir -p $2/$$(build_shlibdir)
	cp $1/picosat.h $2/$$(build_includedir)
	cp $1/libpicosat.$$(SHLIB_EXT) $2/$$(build_shlibdir)
endef
$(eval $(call staged-install, \
	libpicosat,$(LIBPICOSAT_SRC_DIR), \
	LIBPICOSAT_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libpicosat.$$(SHLIB_EXT) $$(build_shlibdir)/libpicosat.$$(SHLIB_EXT)))

clean-libpicosat:
	-rm -f $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-compiled
	-rm -f $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/config.h $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/libpicosat.$(SHLIB_EXT)

get-libpicosat: $(LIBPICOSAT_SRC_FILE)
extract-libpicosat: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/source-extracted
configure-libpicosat: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-configured
compile-libpicosat: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-compiled
fastcheck-libpicosat: check-libpicosat
check-libpicosat: $(BUILDDIR)/$(LIBPICOSAT_SRC_DIR)/build-checked

else

$(eval $(call bb-install,libpicosat,LIBPICOSAT,false))

endif # USE_BINARYBUILDER_LIBPICOSAT
