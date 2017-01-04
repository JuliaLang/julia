## LIBUV ##
LIBUV_GIT_URL:=git://github.com/JuliaLang/libuv.git
LIBUV_TAR_URL=https://api.github.com/repos/JuliaLang/libuv/tarball/$1
$(eval $(call git-external,libuv,LIBUV,configure,,$(SRCDIR)/srccache))

UV_CFLAGS := -D_GNU_SOURCE
ifeq ($(USEMSVC), 1)
UV_CFLAGS += -DBUILDING_UV_SHARED
endif
ifeq ($(USEICC), 1)
UV_CFLAGS += -static-intel
endif

UV_MFLAGS += LDFLAGS="$(LDFLAGS) $(CLDFLAGS) -v"
ifneq ($(UV_CFLAGS),)
UV_MFLAGS += CFLAGS="$(CFLAGS) $(UV_CFLAGS)"
endif
ifneq ($(VERBOSE), 0)
UV_MFLAGS += V=1
endif
ifneq ($(USEMSVC), 1)
UV_FLAGS := $(UV_MFLAGS)
else
UV_FLAGS := --disable-shared $(UV_MFLAGS)
endif

$(BUILDDIR)/$(LIBUV_SRC_DIR)/build-configured: $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/source-extracted
	touch -c $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/aclocal.m4 # touch a few files to prevent autogen from getting called
	touch -c $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/Makefile.in
	touch -c $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/configure
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
extract-libuv: $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/source-extracted
configure-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-configured
compile-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-compiled
fastcheck-libuv: #none
check-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/build-checked
