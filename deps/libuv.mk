## LIBUV ##
LIBUV_GIT_URL:=git://github.com/JuliaLang/libuv.git
LIBUV_TAR_URL=https://api.github.com/repos/JuliaLang/libuv/tarball/$1
$(eval $(call git-external,libuv,LIBUV,configure,,$(SRCCACHE)))

UV_CFLAGS := -O2
ifeq ($(USEMSVC), 1)
UV_CFLAGS += -DBUILDING_UV_SHARED
endif
ifeq ($(USEICC), 1)
UV_CFLAGS += -static-intel
endif

UV_FLAGS := LDFLAGS="$(LDFLAGS) $(CLDFLAGS) -v"
ifneq ($(UV_CFLAGS),)
UV_FLAGS += CFLAGS="$(CFLAGS) $(UV_CFLAGS)"
endif
ifeq ($(USEMSVC), 1)
UV_FLAGS += --disable-shared
endif

ifneq ($(VERBOSE), 0)
UV_MFLAGS += V=1
endif


$(BUILDDIR)/$(LIBUV_SRC_DIR)/build-configured: $(SRCCACHE)/$(LIBUV_SRC_DIR)/source-extracted
	touch -c $(SRCCACHE)/$(LIBUV_SRC_DIR)/aclocal.m4 # touch a few files to prevent autogen from getting called
	touch -c $(SRCCACHE)/$(LIBUV_SRC_DIR)/Makefile.in
	touch -c $(SRCCACHE)/$(LIBUV_SRC_DIR)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure --with-pic $(CONFIGURE_COMMON) $(UV_FLAGS)
	echo 1 > $@

$(BUILDDIR)/$(LIBUV_SRC_DIR)/build-compiled: $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-configured
	$(MAKE) -C $(dir $<) $(UV_MFLAGS)
	echo 1 > $@

$(BUILDDIR)/$(LIBUV_SRC_DIR)/build-checked: $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	-$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	libuv,$$(LIBUV_SRC_DIR), \
	MAKE_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libuv.$$(SHLIB_EXT) $$(build_shlibdir)/libuv.$$(SHLIB_EXT)))

clean-libuv:
	-rm -rf $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-configured $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/$(LIBUV_SRC_DIR) clean


get-libuv: $(LIBUV_SRC_FILE)
extract-libuv: $(SRCCACHE)/$(LIBUV_SRC_DIR)/source-extracted
configure-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-configured
compile-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-compiled
fastcheck-libuv: #none
check-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-checked
