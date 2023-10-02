## libffi ##
include $(SRCDIR)/libffi.version

$(SRCCACHE)/libffi-$(LIBFFI_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/libffi/libffi/releases/download/v$(LIBFFI_VER)/libffi-$(LIBFFI_VER).tar.gz

$(SRCCACHE)/libffi-$(LIBFFI_VER)/source-extracted: $(SRCCACHE)/libffi-$(LIBFFI_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir $(dir $@)
	cd $(dir $@) && $(TAR) xf $< --strip-components=1
	touch -c $(SRCCACHE)/libffi-$(LIBFFI_VER)/configure # old target
	echo 1 > $@

checksum-libffi: $(SRCCACHE)/libffi-$(LIBFFI_VER).tar.gz
	$(JLCHECKSUM) $<

$(BUILDDIR)/libffi-$(LIBFFI_VER)/build-configured: CC:=$(HOSTCC)
$(BUILDDIR)/libffi-$(LIBFFI_VER)/build-configured: CXX:=$(HOSTCXX)
$(BUILDDIR)/libffi-$(LIBFFI_VER)/build-configured: XC_HOST:=$(BUILD_MACHINE)
$(BUILDDIR)/libffi-$(LIBFFI_VER)/build-configured: $(SRCCACHE)/libffi-$(LIBFFI_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) LDFLAGS="$(CXXLDFLAGS)" CPPFLAGS="$(CPPFLAGS)"
	echo 1 > $@

$(BUILDDIR)/libffi-$(LIBFFI_VER)/build-compiled: $(BUILDDIR)/libffi-$(LIBFFI_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/libffi-$(LIBFFI_VER)/build-checked: $(BUILDDIR)/libffi-$(LIBFFI_VER)/build-compiled
	echo 1 > $@

$(eval $(call staged-install, \
	libffi,libffi-$(LIBFFI_VER), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD),,))

clean-libffi:
	-rm -f $(BUILDDIR)/libffi-$(LIBFFI_VER)/build-configured \
		$(BUILDDIR)/libffi-$(LIBFFI_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/libffi-$(LIBFFI_VER) clean

distclean-libffi:
	rm -rf $(SRCCACHE)/libffi-$(LIBFFI_VER).tar.bz2 \
		$(SRCCACHE)/libffi-$(LIBFFI_VER) \
		$(BUILDDIR)/libffi-$(LIBFFI_VER)


get-libffi: $(SRCCACHE)/libffi-$(LIBFFI_VER).tar.bz2
extract-libffi: $(SRCCACHE)/libffi-$(LIBFFI_VER)/source-extracted
configure-libffi: $(BUILDDIR)/libffi-$(LIBFFI_VER)/build-configured
compile-libffi: $(BUILDDIR)/libffi-$(LIBFFI_VER)/build-compiled
check-libffi: $(BUILDDIR)/libffi-$(LIBFFI_VER)/build-checked
