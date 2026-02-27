## MIMALLOC ##
ifneq ($(USE_BINARYBUILDER_MIMALLOC), 1)
MIMALLOC_GIT_URL := https://github.com/microsoft/mimalloc.git
MIMALLOC_TAR_URL = https://api.github.com/repos/microsoft/mimalloc/tarball/$1
$(eval $(call git-external,mimalloc,MIMALLOC,,,$(SRCCACHE)))

MIMALLOC_BUILDDIR := $(BUILDDIR)/$(MIMALLOC_SRC_DIR)

# Build only the static library, use dynamic TLS for musl compatibility
MIMALLOC_BUILD_OPTS := $(CMAKE_COMMON) -DCMAKE_BUILD_TYPE=Release
MIMALLOC_BUILD_OPTS += -DMI_BUILD_SHARED=OFF -DMI_BUILD_STATIC=ON -DMI_BUILD_OBJECT=OFF
MIMALLOC_BUILD_OPTS += -DMI_INSTALL_TOPLEVEL=ON -DMI_BUILD_TESTS=OFF -DMI_OVERRIDE=OFF -DMI_LOCAL_DYNAMIC_TLS=ON

$(MIMALLOC_BUILDDIR)/build-configured: $(SRCCACHE)/$(MIMALLOC_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && $(CMAKE) $(MIMALLOC_BUILD_OPTS) $(dir $<)
	echo 1 > $@

$(MIMALLOC_BUILDDIR)/build-compiled: $(MIMALLOC_BUILDDIR)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON)
	echo 1 > $@

# On Windows, rename libmimalloc.a to libmimalloc-static.a for consistency with the JLL
ifeq ($(OS),WINNT)
define MIMALLOC_INSTALL
	$(MAKE) -C $1 install
	mv $2/$$(build_libdir)/libmimalloc.a $2/$$(build_libdir)/libmimalloc-static.a
endef
$(eval $(call staged-install, \
	mimalloc,$(MIMALLOC_SRC_DIR), \
	MIMALLOC_INSTALL,,,))
else
$(eval $(call staged-install, \
	mimalloc,$(MIMALLOC_SRC_DIR), \
	MAKE_INSTALL,,,))
endif

clean-mimalloc:
	-rm -f $(MIMALLOC_BUILDDIR)/build-configured $(MIMALLOC_BUILDDIR)/build-compiled
	-$(MAKE) -C $(MIMALLOC_BUILDDIR) clean

get-mimalloc: $(MIMALLOC_SRC_FILE)
extract-mimalloc: $(SRCCACHE)/$(MIMALLOC_SRC_DIR)/source-extracted
configure-mimalloc: $(MIMALLOC_BUILDDIR)/build-configured
compile-mimalloc: $(MIMALLOC_BUILDDIR)/build-compiled
fastcheck-mimalloc: check-mimalloc
check-mimalloc: compile-mimalloc

else # USE_BINARYBUILDER_MIMALLOC

$(eval $(call bb-install,mimalloc,MIMALLOC,false))

endif # USE_BINARYBUILDER_MIMALLOC
