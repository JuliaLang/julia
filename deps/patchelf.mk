## patchelf ##

$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://nixos.org/releases/patchelf/patchelf-$(PATCHELF_VER)/patchelf-$(PATCHELF_VER).tar.gz

$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER)/source-extracted: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
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

$(build_prefix)/manifest/patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled | $(build_prefix)/manifest
	$(call make-install,patchelf-$(PATCHELF_VER),)
	echo $(PATCHELF_VER) > $@

clean-patchelf:
	-rm -f $(build_prefix)/manifest/patchelf \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured \
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
install-patchelf: $(build_prefix)/manifest/patchelf
