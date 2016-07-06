## patchelf ##

PATCHELF_SOURCE := $(BUILDDIR)/patchelf-$(PATCHELF_VER)/src/patchelf
PATCHELF_TARGET := $(build_depsbindir)/patchelf

$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://nixos.org/releases/patchelf/patchelf-$(PATCHELF_VER)/patchelf-$(PATCHELF_VER).tar.gz
$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER)/configure: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
	touch -c $@
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/config.status: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER)/configure | $(LIBCXX_DEPENDENCY)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) LDFLAGS="$(CXXLDFLAGS)" CPPFLAGS="$(CPPFLAGS)"
	touch -c $@
$(PATCHELF_SOURCE): $(BUILDDIR)/patchelf-$(PATCHELF_VER)/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/checked: $(PATCHELF_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	# disabled due to bug in v0.6
	#$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@
$(PATCHELF_TARGET): $(PATCHELF_SOURCE)
	$(call make-install,patchelf-$(PATCHELF_VER),)
	touch -c $@

clean-patchelf:
	-$(MAKE) -C $(BUILDDIR)/patchelf-$(PATCHELF_VER) clean
	-rm -f $(PATCHELF_OBJ_TARGET)
distclean-patchelf:
	-rm -rf $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz \
		$(SRCDIR)/srccache/patchelf-$(PATCHELF_VER) \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)

get-patchelf: $(SRCDIR)/srccache/patchelf-$(PATCHELF_VER).tar.gz
configure-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/config.status
compile-patchelf: $(PATCHELF_SOURCE)
check-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/checked
install-patchelf: $(PATCHELF_TARGET)
