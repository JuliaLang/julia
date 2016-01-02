## GMP ##

GMP_SRC_TARGET := $(BUILDDIR)/gmp-$(GMP_VER)/.libs/libgmp.$(SHLIB_EXT)
GMP_OBJ_TARGET := $(build_shlibdir)/libgmp.$(SHLIB_EXT)

ifeq ($(SANITIZE),1)
GMP_CONFIGURE_OPTS += --disable-assembly
endif

ifeq ($(BUILD_OS),WINNT)
GMP_CONFIGURE_OPTS += --srcdir="$(subst \,/,$(call mingw_to_dos,$(SRCDIR)/srccache/gmp-$(GMP_VER)))"
endif

$(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://gmplib.org/download/gmp/$(notdir $@)
$(SRCDIR)/srccache/gmp-$(GMP_VER)/configure: $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $<
	touch -c $@
$(SRCDIR)/srccache/gmp-$(GMP_VER)/patched: $(SRCDIR)/srccache/gmp-$(GMP_VER)/configure
	cd $(dir $@) && patch < $(SRCDIR)/patches/gmp-exception.patch
	echo 1 > $@
$(BUILDDIR)/gmp-$(GMP_VER)/config.status: $(SRCDIR)/srccache/gmp-$(GMP_VER)/configure $(SRCDIR)/srccache/gmp-$(GMP_VER)/patched
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) F77= --enable-shared --disable-static $(GMP_CONFIGURE_OPTS)
	touch -c $@
$(GMP_SRC_TARGET): $(BUILDDIR)/gmp-$(GMP_VER)/config.status
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	touch -c $@
$(BUILDDIR)/gmp-$(GMP_VER)/checked: $(GMP_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(LIBTOOL_CCLD) check
endif
	echo 1 > $@
$(GMP_OBJ_TARGET): $(GMP_SRC_TARGET) | $(build_shlibdir) $(build_includedir)
ifeq ($(BUILD_OS),WINNT)
	-mv $(BUILDDIR)/gmp-$(GMP_VER)/.libs/gmp.dll $(BUILDDIR)/gmp-$(GMP_VER)/.libs/libgmp.dll
endif
	$(INSTALL_M) $(BUILDDIR)/gmp-$(GMP_VER)/.libs/libgmp.*$(SHLIB_EXT)* $(build_shlibdir)
	$(INSTALL_F) $(BUILDDIR)/gmp-$(GMP_VER)/gmp.h $(build_includedir)
	$(INSTALL_NAME_CMD)libgmp.$(SHLIB_EXT) $@
	touch -c $@

clean-gmp:
	-$(MAKE) -C $(BUILDDIR)/gmp-$(GMP_VER) clean
	-rm -f $(GMP_OBJ_TARGET)
distclean-gmp:
	-rm -rf $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2 \
		$(SRCDIR)/srccache/gmp-$(GMP_VER) \
		$(BUILDDIR)/gmp-$(GMP_VER)

get-gmp: $(SRCDIR)/srccache/gmp-$(GMP_VER).tar.bz2
configure-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/config.status
compile-gmp: $(GMP_SRC_TARGET)
check-gmp: $(BUILDDIR)/gmp-$(GMP_VER)/checked
install-gmp: $(GMP_OBJ_TARGET)
