## openspecfun ##
OPENSPECFUN_GIT_URL := git://github.com/JuliaLang/openspecfun.git
OPENSPECFUN_TAR_URL = https://api.github.com/repos/JuliaLang/openspecfun/tarball/$1
$(eval $(call git-external,openspecfun,OPENSPECFUN,Makefile,libopenspecfun.$(SHLIB_EXT),$(BUILDDIR)))

# issue 8799
OPENSPECFUN_CFLAGS := -O3 -std=c99
ifeq ($(USEICC),1)
  OPENSPECFUN_CFLAGS += -fp-model precise
endif

OPENSPECFUN_OBJ_TARGET := $(build_shlibdir)/libopenspecfun.$(SHLIB_EXT)
OPENSPECFUN_OBJ_SOURCE := $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/libopenspecfun.$(SHLIB_EXT)
OPENSPECFUN_FLAGS := ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) FFLAGS="$(JFFLAGS)" CFLAGS="$(CFLAGS) $(OPENSPECFUN_CFLAGS)" LDFLAGS="$(RPATH_ESCAPED_ORIGIN)"

ifeq ($(USE_SYSTEM_LIBM),0)
	OPENSPECFUN_FLAGS += USE_OPENLIBM=1
$(OPENSPECFUN_OBJ_SOURCE): $(OPENLIBM_OBJ_TARGET)
endif

$(OPENSPECFUN_OBJ_SOURCE): $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/Makefile
	$(MAKE) -C $(dir $<) $(OPENSPECFUN_FLAGS) $(MAKE_COMMON)
	touch -c $@
$(OPENSPECFUN_OBJ_TARGET): $(OPENSPECFUN_OBJ_SOURCE)
	$(call make-install,$(OPENSPECFUN_SRC_DIR),$(OPENSPECFUN_FLAGS))
	$(INSTALL_NAME_CMD)libopenspecfun.$(SHLIB_EXT) $@
	touch -c $@

clean-openspecfun:
	-$(MAKE) -C $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR) distclean $(OPENSPECFUN_FLAGS)
	-rm $(OPENSPECFUN_OBJ_TARGET)
	-rm $(build_libdir)/libopenspecfun.a

get-openspecfun: $(OPENSPECFUN_SRC_FILE)
configure-openspecfun: $(BUILDDIR)/$(OPENSPECFUN_SRC_DIR)/Makefile
compile-openspecfun: $(OPENSPECFUN_OBJ_SOURCE)
check-openspecfun: compile-openspecfun
install-openspecfun: $(OPENSPECFUN_OBJ_TARGET)
