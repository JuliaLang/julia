## patchelf ##

$(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/NixOS/patchelf/releases/download/$(PATCHELF_VER)/patchelf-$(PATCHELF_VER).tar.bz2

$(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-extracted: $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2
	$(JLCHECKSUM) $<
	mkdir $(dir $@)
	cd $(dir $@) && $(TAR) jxf $< --strip-components=1
	touch -c $(SRCCACHE)/patchelf-$(PATCHELF_VER)/configure # old target
	echo 1 > $@

checksum-patchelf: $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2
	$(JLCHECKSUM) $<

$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: CC:=$(HOSTCC)
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: CXX:=$(HOSTCXX)
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: XC_HOST:=$(BUILD_MACHINE)
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: $(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) LDFLAGS="$(CXXLDFLAGS)" CPPFLAGS="$(CPPFLAGS)"
	echo 1 > $@

$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-checked: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	# disabled due to bug in v0.6
	#$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	patchelf,patchelf-$(PATCHELF_VER), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD),,))

clean-patchelf:
	-rm -f $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/patchelf-$(PATCHELF_VER) clean

distclean-patchelf:
	rm -rf $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2 \
		$(SRCCACHE)/patchelf-$(PATCHELF_VER) \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)


get-patchelf: $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2
extract-patchelf: $(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-extracted
configure-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured
compile-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled
check-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-checked
