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
	cd $(dir $<) && $(TAR) -jxf $<
	touch -c $(SRCDIR)/srccache/gmp-$(GMP_VER)/configure # old target
	echo 1 > $@

$(SRCDIR)/srccache/gmp-$(GMP_VER)/build-patched: $(SRCDIR)/srccache/gmp-$(GMP_VER)/source-extracted
	cd $(dir $@) && patch < $(SRCDIR)/patches/gmp-exception.patch
	echo 1 > $@

$(BUILDDIR)/gmp-$(GMP_VER)/build-configured: $(SRCDIR)/srccache/gmp-$(GMP_VER)/source-extracted $(SRCDIR)/srccache/gmp-$(GMP_VER)/build-patched
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

define GMP_INSTALL
	mkdir -p $2/$(build_shlibdir) $2/$(build_includedir)
ifeq ($(BUILD_OS),WINNT)
	-mv $1/.libs/gmp.dll $1/.libs/libgmp.dll
endif
	$(INSTALL_M) $1/.libs/libgmp.*$(SHLIB_EXT)* $2/$(build_shlibdir)
	$(INSTALL_F) $1/gmp.h $2/$(build_includedir)
endef
$(eval $(call staged-install, \
	gmp,gmp-$(GMP_VER), \
	GMP_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libgmp.$$(SHLIB_EXT) $$(build_shlibdir)/libgmp.$$(SHLIB_EXT)))

clean-gmp:
	-rm $(BUILDDIR)/gmp-$(GMP_VER)/build-configured $(BUILDDIR)/gmp-$(GMP_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/gmp-$(GMP_VER) clean

distclean-gmp:
	-rm -rf $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2 \
		$(SRCDIR)/srccache/gmp-$(GMP_VER) \
		$(BUILDDIR)/gmp-$(GMP_VER)

get-gmp: $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2
extract-gmp: $(SRCDIR)/srccache/gmp-$(GMP_VER)/source-extracted
configure-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/build-configured
compile-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/build-compiled
fastcheck-gmp: check-gmp
check-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/build-checked
