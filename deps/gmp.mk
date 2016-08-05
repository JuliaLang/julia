## GMP ##

ifeq ($(SANITIZE),1)
GMP_CONFIGURE_OPTS += --disable-assembly
endif

ifeq ($(BUILD_OS),WINNT)
GMP_CONFIGURE_OPTS += --srcdir="$(subst \,/,$(call mingw_to_dos,$(SRCDIR)/srccache/gmp-$(GMP_VER)))"
endif

$(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://gmplib.org/download/gmp/$(notdir $@)

$(SRCDIR)/srccache/gmp-$(GMP_VER)/source-extracted: $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $<
	echo 1 > $@

$(SRCDIR)/srccache/gmp-$(GMP_VER)/patched: $(SRCDIR)/srccache/gmp-$(GMP_VER)/source-extracted
	cd $(dir $@) && patch < $(SRCDIR)/patches/gmp-exception.patch
	echo 1 > $@

$(BUILDDIR)/gmp-$(GMP_VER)/build-configured: $(SRCDIR)/srccache/gmp-$(GMP_VER)/source-extracted $(SRCDIR)/srccache/gmp-$(GMP_VER)/patched
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) F77= --enable-shared --disable-static $(GMP_CONFIGURE_OPTS)
	echo 1 > $@

$(BUILDDIR)/gmp-$(GMP_VER)/build-compiled: $(BUILDDIR)/gmp-$(GMP_VER)/build-configured
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	echo 1 > $@

$(BUILDDIR)/gmp-$(GMP_VER)/build-checked: $(BUILDDIR)/gmp-$(GMP_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(LIBTOOL_CCLD) check
endif
	echo 1 > $@

$(build_prefix)/manifest/gmp: $(BUILDDIR)/gmp-$(GMP_VER)/build-compiled | $(build_shlibdir) $(build_includedir) $(build_prefix)/manifest
ifeq ($(BUILD_OS),WINNT)
	-mv $(BUILDDIR)/gmp-$(GMP_VER)/.libs/gmp.dll $(BUILDDIR)/gmp-$(GMP_VER)/.libs/libgmp.dll
endif
	$(INSTALL_M) $(BUILDDIR)/gmp-$(GMP_VER)/.libs/libgmp.*$(SHLIB_EXT)* $(build_shlibdir)
	$(INSTALL_F) $(BUILDDIR)/gmp-$(GMP_VER)/gmp.h $(build_includedir)
	$(INSTALL_NAME_CMD)libgmp.$(SHLIB_EXT) $(build_shlibdir)/libgmp.$(SHLIB_EXT)
	echo $(GMP_VER) > $@

clean-gmp:
	-rm -f $(build_prefix)/manifest/gmp $(BUILDDIR)/gmp-$(GMP_VER)/build-configured $(BUILDDIR)/gmp-$(GMP_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/gmp-$(GMP_VER) clean

distclean-gmp:
	-rm -rf $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2 \
		$(SRCDIR)/srccache/gmp-$(GMP_VER) \
		$(BUILDDIR)/gmp-$(GMP_VER)

get-gmp: $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2
extract-gmp: $(SRCDIR)/srccache/gmp-$(GMP_VER)/source-extracted
configure-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/build-configured
compile-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/build-compiled
check-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/build-checked
install-gmp: $(build_prefix)/manifest/gmp
