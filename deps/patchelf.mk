## patchelf ##

$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://nixos.org/releases/patchelf/patchelf-$(PATCHELF_VER)/patchelf-$(PATCHELF_VER).tar.gz

$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER)/source-extracted: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
	touch -c $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER)/configure # old target
	echo 1 > $@

$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER)/source-extracted | $(LIBCXX_DEPENDENCY)
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
	-rm $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/patchelf-$(PATCHELF_VER) clean

distclean-patchelf:
	-rm -rf $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz \
		$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER) \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)


get-patchelf: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz
extract-patchelf: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER)/source-extracted
configure-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured
compile-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled
check-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-checked
