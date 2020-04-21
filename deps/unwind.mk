## UNWIND ##

ifneq ($(USE_BINARYBUILDER_LIBUNWIND),1)
LIBUNWIND_CFLAGS := -U_FORTIFY_SOURCE $(fPIC)
LIBUNWIND_CPPFLAGS :=

$(SRCCACHE)/libunwind-$(UNWIND_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/libunwind/libunwind/releases/download/v$(UNWIND_VER)/libunwind-$(UNWIND_VER).tar.gz

$(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted: $(SRCCACHE)/libunwind-$(UNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) xfz $<
	touch -c $(SRCCACHE)/libunwind-$(UNWIND_VER)/configure # old target
	echo 1 > $@

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-prefer-extbl.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p1 -f < $(SRCDIR)/patches/libunwind-prefer-extbl.patch
	echo 1 > $@

$(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-static-arm.patch-applied: $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-prefer-extbl.patch-applied
	cd $(SRCCACHE)/libunwind-$(UNWIND_VER) && patch -p1 -f < $(SRCDIR)/patches/libunwind-static-arm.patch
	echo 1 > $@

$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured: $(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted $(SRCCACHE)/libunwind-$(UNWIND_VER)/libunwind-static-arm.patch-applied
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) CPPFLAGS="$(CPPFLAGS) $(LIBUNWIND_CPPFLAGS)" CFLAGS="$(CFLAGS) $(LIBUNWIND_CFLAGS)" --disable-shared --disable-minidebuginfo --disable-tests
	echo 1 > $@

$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/libunwind-$(UNWIND_VER)/build-checked: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	unwind,libunwind-$(UNWIND_VER), \
	MAKE_INSTALL,,,))

clean-unwind:
	-rm $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/libunwind-$(UNWIND_VER) clean

distclean-unwind:
	-rm -rf $(SRCCACHE)/libunwind-$(UNWIND_VER).tar.gz \
		$(SRCCACHE)/libunwind-$(UNWIND_VER) \
		$(BUILDDIR)/libunwind-$(UNWIND_VER)

get-unwind: $(SRCCACHE)/libunwind-$(UNWIND_VER).tar.gz
extract-unwind: $(SRCCACHE)/libunwind-$(UNWIND_VER)/source-extracted
configure-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-configured
compile-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-compiled
#todo: libunwind tests are known to fail, so they aren't run
fastcheck-unwind: #none
check-unwind: $(BUILDDIR)/libunwind-$(UNWIND_VER)/build-checked


## OS X Unwind ##

OSXUNWIND_FLAGS := ARCH="$(ARCH)" CC="$(CC)" FC="$(FC)" AR="$(AR)" OS="$(OS)" USECLANG=$(USECLANG) USEGCC=$(USEGCC) CFLAGS="$(CFLAGS) -ggdb3 -O0" CXXFLAGS="$(CXXFLAGS) -ggdb3 -O0" SFLAGS="-ggdb3" LDFLAGS="$(LDFLAGS) -Wl,-macosx_version_min,10.7"

$(SRCCACHE)/libosxunwind-$(OSXUNWIND_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/JuliaLang/libosxunwind/archive/v$(OSXUNWIND_VER).tar.gz

$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/source-extracted: $(SRCCACHE)/libosxunwind-$(OSXUNWIND_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(BUILDDIR)
	cd $(BUILDDIR) && $(TAR) xfz $<
	echo 1 > $@

$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/build-compiled: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/source-extracted
	$(MAKE) -C $(dir $<) $(OSXUNWIND_FLAGS)
	echo 1 > $@

$(build_prefix)/manifest/osxunwind: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/build-compiled | $(build_libdir) $(build_shlibdir) $(build_includedir) $(build_prefix)/manifest
	cp $(dir $<)/libosxunwind.a $(build_libdir)/libosxunwind.a
	cp $(dir $<)/libosxunwind.$(SHLIB_EXT) $(build_shlibdir)/libosxunwind.$(SHLIB_EXT)
	cp -R $(dir $<)/include/* $(build_includedir)
	$(INSTALL_NAME_CMD)libosxunwind.$(SHLIB_EXT) $(build_shlibdir)/libosxunwind.$(SHLIB_EXT)
	echo $(OSXUNWIND_VER) > $(build_prefix)/manifest/osxunwind

clean-osxunwind:
	-rm $(build_prefix)/manifest/osxunwind $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/build-compiled
	-rm -r $(build_libdir)/libosxunwind.a $(build_shlibdir)/libosxunwind.$(SHLIB_EXT) \
		$(build_includedir)/mach-o/ $(build_includedir)/unwind.h $(build_includedir)/libunwind.h
	-$(MAKE) -C $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER) clean $(OSXUNWIND_FLAGS)

distclean-osxunwind:
	-rm -rf $(SRCCACHE)/libosxunwind-$(OSXUNWIND_VER).tar.gz \
		$(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)


get-osxunwind: $(SRCCACHE)/libosxunwind-$(OSXUNWIND_VER).tar.gz
extract-osxunwind: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/source-extracted
configure-osxunwind: extract-osxunwind
compile-osxunwind: $(BUILDDIR)/libosxunwind-$(OSXUNWIND_VER)/build-compiled
fastcheck-osxunwind: check-osxunwind
check-osxunwind: compile-osxunwind
install-osxunwind: $(build_prefix)/manifest/osxunwind

# If we built our own libunwind/libosxunwind, we need to generate a fake LibUnwind_jll/LibOSXUnwind_jll package to load it in:
$(eval $(call jll-generate,LibUnwind_jll,libunwind=\"libunwind\",,745a5e78-f969-53e9-954f-d19f2f74f4e3,))
$(eval $(call jll-generate,LibOSXUnwind_jll,libosxunwind=\"libosxunwind\",,a83860b7-747b-57cf-bf1f-3e79990d037f,))

else # USE_BINARYBUILDER_LIBUNWIND

# Install LibUnwind_jll/LibOSXUnwind_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,LibUnwind_jll))
$(eval $(call install-jll-and-artifact,LibOSXUnwind_jll))

# Fix naming mismatches (since the JLL packages have "Lib" in front)
$(eval $(call fix-artifact-naming-mismatch,unwind,LibUnwind_jll))
$(eval $(call fix-artifact-naming-mismatch,osxunwind,LibOSXUnwind_jll))

endif
