## openspecfun ##
OPENSPECFUN_GIT_URL := git://github.com/JuliaLang/openspecfun.git
OPENSPECFUN_TAR_URL = https://api.github.com/repos/JuliaLang/openspecfun/tarball/$1
$(eval $(call git-external,openspecfun,OPENSPECFUN,,,$(BUILDDIR)))

# issue 8799
OPENSPECFUN_CFLAGS := -O3 -std=c99
ifeq ($(USEICC),1)
  OPENSPECFUN_CFLAGS += -fp-model precise
endif

OPENSPECFUN_FLAGS := ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" \
	USECLANG=$(USECLANG) USEGCC=$(USEGCC) FFLAGS="$(JFFLAGS)" \
	CFLAGS="$(CFLAGS) $(OPENSPECFUN_CFLAGS)" LDFLAGS="$(LDFLAGS) $(RPATH_ESCAPED_ORIGIN)"

ifeq ($(USE_SYSTEM_LIBM),0)
  OPENSPECFUN_FLAGS += USE_OPENLIBM=1
$(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/build-compiled: $(build_prefix)/manifest/openlibm
endif

$(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/build-compiled: $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/source-extracted
	$(MAKE) -C $(dir $<) $(OPENSPECFUN_FLAGS) $(MAKE_COMMON)
	echo 1 > $@

$(build_prefix)/manifest/openspecfun: $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/build-compiled
	$(call make-install,$(OPENSPECFUN_SRC_DIR),$(OPENSPECFUN_FLAGS))
	$(INSTALL_NAME_CMD)libopenspecfun.$(SHLIB_EXT) $(build_shlibdir)/libopenspecfun.$(SHLIB_EXT)
	echo $(OPENSPECFUN_SHA1) > $@

clean-openspecfun:
	-rm $(build_prefix)/manifest/openspecfun $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/build-compiled $(build_libdir)/libopenspecfun.a
	-$(MAKE) -C $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR) distclean $(OPENSPECFUN_FLAGS)

get-openspecfun: $(OPENSPECFUN_SRC_FILE)
extract-openspecfun: $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/source-extracted
configure-openspecfun: extract-openspecfun
compile-openspecfun: $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/build-compiled
check-openspecfun: compile-openspecfun
install-openspecfun: $(build_prefix)/manifest/openspecfun
