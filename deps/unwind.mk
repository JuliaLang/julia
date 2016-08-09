## UNWIND ##

LIBUNWIND_CFLAGS := -U_FORTIFY_SOURCE $(fPIC)
LIBUNWIND_CPPFLAGS :=

$(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://s3.amazonaws.com/julialang/src/libunwind-$(UNWIND_VER).tar.gz

$(SRCDIR)/srccache/libunwind-$(UNWIND_VER)/source-extracted: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) xfz $<
	touch -c $(SRCDIR)/srccache/libunwind-$(UNWIND_VER)/configure # old target
	echo 1 > $@

$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) CPPFLAGS="$(CPPFLAGS) $(LIBUNWIND_CPPFLAGS)" CFLAGS="$(CFLAGS) $(LIBUNWIND_CFLAGS)" --disable-shared --disable-minidebuginfo
	echo 1 > $@

$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-checked: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

define LIBUNWIND_INSTALL
	$(call MAKE_INSTALL,$1,$2,$3)
ifneq (,$(filter $(ARCH), powerpc64le ppc64le))
	@# workaround for configure script bug
	mv $2/$$(build_prefix)/lib64/libunwind*.a $2/$$(build_libdir)
  endif
endef
$(eval $(call staged-install, \
	unwind,libunwind-$(UNWIND_VER), \
	LIBUNWIND_INSTALL,,,))

clean-unwind:
	-rm $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/libunwind-$(UNWIND_VER) clean

distclean-unwind:
	-rm -rf $(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz \
		$(SRCDIR)/srccache/libunwind-$(UNWIND_VER) \
		$(BUILDDIR)/libunwind-$(UNWIND_VER)

get-unwind: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER).tar.gz
extract-unwind: $(SRCDIR)/srccache/libunwind-$(UNWIND_VER)/source-extracted
configure-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured
compile-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled
#todo: libunwind tests are known to fail, so they aren't run
fastcheck-unwind: #none
check-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-checked


## OS X Unwind ##

OSXUNWIND_FLAGS := ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) CFLAGS="$(CFLAGS) -ggdb3 -O0" CXXFLAGS="$(CXXFLAGS) -ggdb3 -O0" SFLAGS="-ggdb3" LDFLAGS="$(LDFLAGS) -Wl,-macosx_version_min,10.7"

$(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://github.com/JuliaLang/libosxunwind/archive/v$(OSXUNWIND_VER).tar.gz

$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/source-extracted: $(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(BUILDDIR)
	cd $(BUILDDIR) && $(TAR) xfz $<
	echo 1 > $@

$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/build-compiled: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/source-extracted
	$(MAKE) -C $(dir $<) $(OSXUNWIND_FLAGS)
	echo 1 > $@

$(build_prefix)/manifest/osxunwind: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/build-compiled | $(build_shlibdir) $(build_prefix)/manifest
	cp $(dir $<)/libosxunwind.a $(build_libdir)/libosxunwind.a
	cp $(dir $<)/libosxunwind.$(SHLIB_EXT) $(build_shlibdir)/libosxunwind.$(SHLIB_EXT)
	cp -R $(dir $<)/include/* $(build_includedir)
	$(INSTALL_NAME_CMD)libosxunwind.$(SHLIB_EXT) $(build_shlibdir)/libosxunwind.$(SHLIB_EXT)
	echo $(OSXUNWIND_VER) > $(build_prefix)/manifest/osxunwind

clean-osxunwind:
	-rm $(build_prefix)/manifest/libuv $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/build-compiled
	-rm -r $(build_libdir)/libosxunwind.a $(build_shlibdir)/libosxunwind.$(SHLIB_EXT) \
		$(build_includedir)/mach-o/ $(build_includedir)/unwind.h $(build_includedir)/libunwind.h
	-$(MAKE) -C $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER) clean $(OSXUNWIND_FLAGS)

distclean-osxunwind:
	-rm -rf $(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz \
		$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)


get-osxunwind: $(SRCDIR)/srccache/libosxunwind-$(OSXUNWIND_VER).tar.gz
extract-osxunwind: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/source-extracted
configure-osxunwind: extract-osxunwind
compile-osxunwind: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/build-compiled
fastcheck-osxunwind: check-osxunwind
check-osxunwind: compile-osxunwind
install-osxunwind: $(build_prefix)/manifest/osxunwind
