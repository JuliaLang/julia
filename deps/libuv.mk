## LIBUV ##
LIBUV_GIT_URL:=git://github.com/JuliaLang/libuv.git
LIBUV_TAR_URL=https://api.github.com/repos/JuliaLang/libuv/tarball/$1
$(eval $(call git-external,libuv,LIBUV,configure,,$(SRCCACHE)))

ifneq ($(USE_BINARYBUILDER_LIBUV),1)

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

LIBUV_BUILDDIR := $(BUILDDIR)/$(LIBUV_SRC_DIR)

$(LIBUV_BUILDDIR)/build-configured: $(SRCCACHE)/$(LIBUV_SRC_DIR)/source-extracted
	touch -c $(SRCCACHE)/$(LIBUV_SRC_DIR)/aclocal.m4 # touch a few files to prevent autogen from getting called
	touch -c $(SRCCACHE)/$(LIBUV_SRC_DIR)/Makefile.in
	touch -c $(SRCCACHE)/$(LIBUV_SRC_DIR)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure --with-pic $(CONFIGURE_COMMON) $(UV_FLAGS)
	echo 1 > $@

$(LIBUV_BUILDDIR)/build-compiled: $(LIBUV_BUILDDIR)/build-configured
	$(MAKE) -C $(dir $<) $(UV_MFLAGS)
	echo 1 > $@

$(LIBUV_BUILDDIR)/build-checked: $(LIBUV_BUILDDIR)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	libuv,$$(LIBUV_SRC_DIR), \
	MAKE_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libuv.$$(SHLIB_EXT) $$(build_shlibdir)/libuv.$$(SHLIB_EXT)))

clean-libuv:
	-rm -rf $(LIBUV_BUILDDIR)/build-configured $(LIBUV_BUILDDIR)/build-compiled
	-$(MAKE) -C $(LIBUV_BUILDDIR) clean


get-libuv: $(LIBUV_SRC_FILE)
extract-libuv: $(SRCCACHE)/$(LIBUV_SRC_DIR)/source-extracted
configure-libuv: $(LIBUV_BUILDDIR)/build-configured
compile-libuv: $(LIBUV_BUILDDIR)/build-compiled
fastcheck-libuv: #none
check-libuv: $(LIBUV_BUILDDIR)/build-checked

# If we built our own libuv, we need to generate a fake LibUV_jll package to load it in:
# Note that since we only build the static library above, we don't bother to generate any library products here.
$(eval $(call jll-generate,LibUV_jll,,,183b4373-6708-53ba-ad28-60e28bb38547,))

else # USE_BINARYBUILDER_LIBUV

# Install LibUV_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,LibUV_jll))
endif
