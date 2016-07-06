## LIBUV ##
LIBUV_GIT_URL:=git://github.com/JuliaLang/libuv.git
LIBUV_TAR_URL=https://api.github.com/repos/JuliaLang/libuv/tarball/$1
$(eval $(call git-external,libuv,LIBUV,configure,.libs/libuv.la,$(SRCDIR)/srccache))

UV_SRC_TARGET := $(BUILDDIR)/$(LIBUV_SRC_DIR)/.libs/libuv.la
UV_OBJ_TARGET := $(build_libdir)/libuv.la

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

$(BUILDDIR)/$(LIBUV_SRC_DIR)/config.status: $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/configure
	touch -c $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/aclocal.m4 # touch a few files to prevent autogen from getting called
	touch -c $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/Makefile.in
	touch -c $(SRCDIR)/srccache/$(LIBUV_SRC_DIR)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< --with-pic $(CONFIGURE_COMMON) $(UV_FLAGS)
	touch -c $@
$(UV_SRC_TARGET): $(BUILDDIR)/$(LIBUV_SRC_DIR)/config.status
	$(MAKE) -C $(dir $<) $(UV_MFLAGS)
	touch -c $@
$(BUILDDIR)/$(LIBUV_SRC_DIR)/checked: $(UV_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	-$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@
$(UV_OBJ_TARGET): $(UV_SRC_TARGET)
	$(call make-install,$(LIBUV_SRC_DIR),)
	$(INSTALL_NAME_CMD)libuv.$(SHLIB_EXT) $(build_shlibdir)/libuv.$(SHLIB_EXT)
	touch -c $@

clean-libuv:
	-$(MAKE) -C $(BUILDDIR)/$(LIBUV_SRC_DIR) clean
	-rm -rf $(build_libdir)/libuv.a $(build_libdir)/libuv.la $(build_includedir)/libuv.h $(build_includedir)/libuv-private

get-libuv: $(LIBUV_SRC_FILE)
configure-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/config.status
compile-libuv: $(UV_SRC_TARGET)
check-libuv: $(BUILDDIR)/$(LIBUV_SRC_DIR)/checked
install-libuv: $(UV_OBJ_TARGET)
