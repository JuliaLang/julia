## FFTW ##

ifneq ($(PATCHELF),patchelf)
# actually required by the stage-fftw target, but there's no easy way to hook into that
$(BUILDDIR)/fftw-$(FFTW_VER)-single/build-compiled: | $(build_prefix)/manifest/patchelf
$(BUILDDIR)/fftw-$(FFTW_VER)-double/build-compiled: | $(build_prefix)/manifest/patchelf
endif

FFTW_CONFIG := --enable-shared --disable-fortran --disable-mpi --enable-threads
ifneq (,$(findstring arm,$(ARCH)))
  FFTW_CONFIG +=
else ifeq ($(ARCH), ppc)
  FFTW_CONFIG += --enable-altivec
else ifeq ($(ARCH), x86_64)
  FFTW_CONFIG += --enable-sse2 --enable-fma
endif
ifeq ($(OS),WINNT)
FFTW_CONFIG += --with-our-malloc --with-combined-threads
ifneq ($(ARCH),x86_64)
FFTW_CONFIG += --with-incoming-stack-boundary=2
endif
endif
FFTW_ENABLE_single := --enable-single
FFTW_ENABLE_double :=

$(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.fftw.org/$(notdir $@)

$(SRCDIR)/srccache/fftw-$(FFTW_VER)/source-extracted: $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	touch -c $(SRCDIR)/srccache/fftw-$(FFTW_VER)/configure # old target
	echo 1 > $@

$(SRCDIR)/srccache/fftw-$(FFTW_VER)/fftw-win32-intrin-h.patch-applied: $(SRCDIR)/srccache/fftw-$(FFTW_VER)/source-extracted
	cd $(SRCDIR)/srccache/fftw-$(FFTW_VER) && patch -p1 -f < $(SRCDIR)/patches/fftw-win32-intrin-h.patch
	echo 1 > $@

$(BUILDDIR)/fftw-$(FFTW_VER)-%/build-configured: $(SRCDIR)/srccache/fftw-$(FFTW_VER)/source-extracted $(SRCDIR)/srccache/fftw-$(FFTW_VER)/fftw-win32-intrin-h.patch-applied
	mkdir -p $(dir $@)
	@# try to configure with avx support. if that fails, try again without it
	cd $(dir $@) && \
	($(dir $<)/configure $(CONFIGURE_COMMON) $(FFTW_CONFIG) $(FFTW_ENABLE_$*) --enable-avx || \
	  $(dir $<)/configure $(CONFIGURE_COMMON) $(FFTW_CONFIG) $(FFTW_ENABLE_$*))
	$(MAKE) -C $(dir $@) clean
	echo 1 > $@

$(BUILDDIR)/fftw-$(FFTW_VER)-%/build-compiled: $(BUILDDIR)/fftw-$(FFTW_VER)-%/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/fftw-$(FFTW_VER)-%/build-checked: $(BUILDDIR)/fftw-$(FFTW_VER)-%/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

define FFTW_INSTALL
	$(call MAKE_INSTALL,$1,$2,)
ifeq ($$(OS), WINNT)
	mv $2/$$(build_shlibdir)/$3-3.dll $2/$$(build_shlibdir)/$3.dll
endif
ifeq ($$(OS), Linux)
	for filename in $2/$$(build_shlibdir)/$3_threads.so* ; do \
		[ -L $$$$filename ] || $$(PATCHELF_BIN) --set-rpath '$$$$ORIGIN' $$$$filename ;\
	done
endif
endef

$(eval $(call staged-install, \
	fftw-single,fftw-$(FFTW_VER)-single, \
	FFTW_INSTALL,libfftw3f,, \
	$$(INSTALL_NAME_CMD)libfftw3f.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3f.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libfftw3f_threads.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3f_threads.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) $$(build_shlibdir)/libfftw3f.3.$$(SHLIB_EXT) $$(INSTALL_NAME_ID_DIR)libfftw3f.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3f_threads.$$(SHLIB_EXT)))
$(eval $(call staged-install, \
	fftw-double,fftw-$(FFTW_VER)-double, \
	FFTW_INSTALL,libfftw3,, \
	$$(INSTALL_NAME_CMD)libfftw3.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CMD)libfftw3_threads.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3_threads.$$(SHLIB_EXT) && \
	$$(INSTALL_NAME_CHANGE_CMD) $$(build_shlibdir)/libfftw3.3.$$(SHLIB_EXT) $$(INSTALL_NAME_ID_DIR)libfftw3.$$(SHLIB_EXT) $$(build_shlibdir)/libfftw3_threads.$$(SHLIB_EXT)))

clean-fftw: clean-fftw-single clean-fftw-double
clean-fftw-%:
	-rm $(BUILDDIR)/fftw-$(FFTW_VER)-$*/build-compiled $(BUILDDIR)/fftw-$(FFTW_VER)-$*/build-configured
	-$(MAKE) -C $(BUILDDIR)/fftw-$(FFTW_VER)-$* clean

distclean-fftw: distclean-fftw-single distclean-fftw-double
	-rm -rf $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz $(SRCDIR)/srccache/fftw-$(FFTW_VER)
distclean-fftw-single:
	-rm -rf $(BUILDDIR)/fftw-$(FFTW_VER)-single
distclean-fftw-double:
	-rm -rf $(BUILDDIR)/fftw-$(FFTW_VER)-double

get-fftw: get-fftw-single get-fftw-double
extract-fftw: extract-fftw-single extract-fftw-double
configure-fftw: configure-fftw-single configure-fftw-double
compile-fftw: compile-fftw-single compile-fftw-double
fastcheck-fftw: fastcheck-fftw-single fastcheck-fftw-double
check-fftw: check-fftw-single check-fftw-double
stage-fftw: stage-fftw-single stage-fftw-double
install-fftw: install-fftw-single install-fftw-double
uninstall-fftw: uninstall-fftw-single uninstall-fftw-double
reinstall-fftw: reinstall-fftw-single reinstall-fftw-double


get-fftw-single: $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz
extract-fftw-single: $(SRCDIR)/srccache/fftw-$(FFTW_VER)/source-extracted
configure-fftw-single: $(BUILDDIR)/fftw-$(FFTW_VER)-single/build-configured
compile-fftw-single: $(BUILDDIR)/fftw-$(FFTW_VER)-single/build-compiled
fastcheck-fftw-single: #none
check-fftw-single: $(BUILDDIR)/fftw-$(FFTW_VER)-single/build-checked

get-fftw-double: $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz
extract-fftw-double: $(SRCDIR)/srccache/fftw-$(FFTW_VER)/source-extracted
configure-fftw-double: $(BUILDDIR)/fftw-$(FFTW_VER)-double/build-configured
compile-fftw-double: $(BUILDDIR)/fftw-$(FFTW_VER)-double/build-compiled
fastcheck-fftw-double: #none
check-fftw-double: $(BUILDDIR)/fftw-$(FFTW_VER)-double/build-checked
