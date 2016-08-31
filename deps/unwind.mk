## UNWIND ##

LIBUNWIND_TARGET_OBJ := $(build_libdir)/libunwind.a
LIBUNWIND_TARGET_SOURCE := $(BUILDDIR)/libunwind-$(UNWIND_VER)/src/.libs/libunwind.a
LIBUNWIND_CFLAGS := -U_FORTIFY_SOURCE $(fPIC)
LIBUNWIND_CPPFLAGS :=

$(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://s3.amazonaws.com/julialang/src/libunwind-$(UNWIND_VER).tar.gz
$(SRCDIR)/srccache/libunwind-$(UNWIND_VER)/configure: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) xfz $<
	touch -c $@
$(BUILDDIR)/libunwind-$(UNWIND_VER)/config.status: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) CPPFLAGS="$(CPPFLAGS) $(LIBUNWIND_CPPFLAGS)" CFLAGS="$(CFLAGS) $(LIBUNWIND_CFLAGS)" --disable-shared --disable-minidebuginfo
	touch -c $@
$(LIBUNWIND_TARGET_SOURCE): $(BUILDDIR)/libunwind-$(UNWIND_VER)/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/libunwind-$(UNWIND_VER)/checked: $(LIBUNWIND_TARGET_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@
#todo: libunwind tests are known to fail, so they aren't run
$(LIBUNWIND_TARGET_OBJ): $(LIBUNWIND_TARGET_SOURCE)
	$(call make-install,libunwind-$(UNWIND_VER),)
ifneq (,$(filter $(ARCH), powerpc64le ppc64le))
	@# workaround for configure script bug
	mv $(build_prefix)/lib64/libunwind*.a $(build_libdir)
endif
	touch $@

clean-unwind:
	-$(MAKE) -C $(BUILDDIR)/libunwind-$(UNWIND_VER) clean
	-rm -f $(LIBUNWIND_TARGET_OBJ) $(LIBUNWIND_TARGET_SOURCE)
distclean-unwind:
	-rm -rf $(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz \
		$(SRCDIR)/srccache/libunwind-$(UNWIND_VER) \
		$(BUILDDIR)/libunwind-$(UNWIND_VER)

get-unwind: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz
configure-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/config.status
compile-unwind: $(LIBUNWIND_TARGET_SOURCE)
check-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/checked
install-unwind: $(LIBUNWIND_TARGET_OBJ)

## OS X Unwind ##

OSXUNWIND_FLAGS := ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) CFLAGS="$(CFLAGS) -ggdb3 -O0" CXXFLAGS="$(CXXFLAGS) -ggdb3 -O0" SFLAGS="-ggdb3" LDFLAGS="$(LDFLAGS) -Wl,-macosx_version_min,10.7"

OSXUNWIND_OBJ_TARGET := $(build_shlibdir)/libosxunwind.$(SHLIB_EXT)
OSXUNWIND_OBJ_SOURCE := $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/libosxunwind.$(SHLIB_EXT)

$(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://github.com/JuliaLang/libosxunwind/archive/v$(OSXUNWIND_VER).tar.gz

$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/Makefile: $(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(BUILDDIR)
	cd $(BUILDDIR) && $(TAR) xfz $<
	touch -c $@

$(OSXUNWIND_OBJ_SOURCE): $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/Makefile
	$(MAKE) -C $(dir $<) $(OSXUNWIND_FLAGS)
	touch -c $@
$(OSXUNWIND_OBJ_TARGET): $(OSXUNWIND_OBJ_SOURCE) | $(build_shlibdir)
	cp $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/libosxunwind.a $(build_libdir)/libosxunwind.a
	cp $< $@
	cp -R $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/include/* $(build_includedir)
	$(INSTALL_NAME_CMD)libosxunwind.$(SHLIB_EXT) $@

clean-osxunwind:
	-$(MAKE) -C $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER) clean $(OSXUNWIND_FLAGS)
	-rm $(OSXUNWIND_OBJ_TARGET)
distclean-osxunwind:
	-rm -rf $(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz \
		$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)


get-osxunwind: $(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz
configure-osxunwind: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/Makefile
compile-osxunwind: $(OSXUNWIND_OBJ_SOURCE)
check-osxunwind: compile-osxunwind
install-osxunwind: $(OSXUNWIND_OBJ_TARGET)
