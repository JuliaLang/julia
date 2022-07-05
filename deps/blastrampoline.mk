## blastrampoline ##

ifneq ($(USE_BINARYBUILDER_BLASTRAMPOLINE),1)

BLASTRAMPOLINE_GIT_URL := https://github.com/JuliaLinearAlgebra/libblastrampoline.git
BLASTRAMPOLINE_TAR_URL = https://api.github.com/repos/JuliaLinearAlgebra/libblastrampoline/tarball/$1
$(eval $(call git-external,blastrampoline,BLASTRAMPOLINE,,,$(BUILDDIR)))

BLASTRAMPOLINE_BUILD_OPTS := $(MAKE_COMMON) CC="$(CC)" CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)"
BLASTRAMPOLINE_BUILD_OPTS += ARCH="$(ARCH)" OS="$(OS)"

$(BUILDDIR)/$(BLASTRAMPOLINE_SRC_DIR)/build-configured: $(BUILDDIR)/$(BLASTRAMPOLINE_SRC_DIR)/source-extracted
	mkdir -p $(dir $@)
	echo 1 > $@

BLASTRAMPOLINE_BUILD_ROOT := $(BUILDDIR)/$(BLASTRAMPOLINE_SRC_DIR)/src
$(BUILDDIR)/$(BLASTRAMPOLINE_SRC_DIR)/build-compiled: $(BUILDDIR)/$(BLASTRAMPOLINE_SRC_DIR)/build-configured
	cd $(dir $@)/src && $(MAKE) $(BLASTRAMPOLINE_BUILD_OPTS)
ifeq ($(OS), WINNT)
	# Windows doesn't like soft link, use hard link
	cd $(BLASTRAMPOLINE_BUILD_ROOT)/build/ && \
		cp -f --dereference --link libblastrampoline.dll libblastrampoline.dll
endif
	echo 1 > $@

define BLASTRAMPOLINE_INSTALL
	$(MAKE) -C $(BLASTRAMPOLINE_BUILD_ROOT) install $(BLASTRAMPOLINE_BUILD_OPTS) DESTDIR="$2"
endef
$(eval $(call staged-install, \
	blastrampoline,$(BLASTRAMPOLINE_SRC_DIR), \
	BLASTRAMPOLINE_INSTALL,, \
	$$(BLASTRAMPOLINE_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libblastrampoline.$$(SHLIB_EXT) $$(build_shlibdir)/libblastrampoline.$$(SHLIB_EXT)))

clean-blastrampoline:
	-$(MAKE) -C $(BLASTRAMPOLINE_BUILD_ROOT) clean
	-$(RM) $(BUILDDIR)/$(BLASTRAMPOLINE_SRC_DIR)/build-compiled \
		$(BUILDDIR)/$(BLASTRAMPOLINE_SRC_DIR)/build-configured

get-blastrampoline: $(BLASTRAMPOLINE_SRC_FILE)
extract-blastrampoline: $(BUILDDIR)/$(BLASTRAMPOLINE_SRC_DIR)/source-extracted
configure-blastrampoline: extract-blastrampoline
compile-blastrampoline: $(BUILDDIR)/$(BLASTRAMPOLINE_SRC_DIR)/build-compiled
fastcheck-blastrampoline: check-blastrampoline
check-blastrampoline: compile-blastrampoline

else

$(eval $(call bb-install,blastrampoline,BLASTRAMPOLINE,false))

endif # USE_BINARYBUILDER_BLASTRAMPOLINE
