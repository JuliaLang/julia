## patchelf ##

$(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://nixos.org/releases/patchelf/patchelf-$(PATCHELF_VER)/patchelf-$(PATCHELF_VER).tar.gz

$(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-extracted: $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
	touch -c $(SRCCACHE)/patchelf-$(PATCHELF_VER)/configure # old target
	echo 1 > $@

$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: $(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-extracted | $(LIBCXX_DEPENDENCY)
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
	-rm -rf $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.gz \
		$(SRCCACHE)/patchelf-$(PATCHELF_VER) \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)


get-patchelf: $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.gz
extract-patchelf: $(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-extracted
configure-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured
compile-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled
check-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-checked
