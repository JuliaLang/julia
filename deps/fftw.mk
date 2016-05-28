## FFTW ##
ifeq ($(OS),WINNT)
FFTW_SINGLE_SRC_TARGET := $(BUILDDIR)/fftw-$(FFTW_VER)-single/.libs/libfftw3f-3.$(SHLIB_EXT)
FFTW_DOUBLE_SRC_TARGET := $(BUILDDIR)/fftw-$(FFTW_VER)-double/.libs/libfftw3-3.$(SHLIB_EXT)
else
FFTW_SINGLE_SRC_TARGET := $(BUILDDIR)/fftw-$(FFTW_VER)-single/.libs/libfftw3f.$(SHLIB_EXT)
FFTW_DOUBLE_SRC_TARGET := $(BUILDDIR)/fftw-$(FFTW_VER)-double/.libs/libfftw3.$(SHLIB_EXT)
endif
FFTW_SINGLE_OBJ_TARGET := $(build_shlibdir)/libfftw3f.$(SHLIB_EXT)
FFTW_DOUBLE_OBJ_TARGET := $(build_shlibdir)/libfftw3.$(SHLIB_EXT)

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
$(SRCDIR)/srccache/fftw-$(FFTW_VER)/configure: $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	touch -c $@
$(BUILDDIR)/fftw-$(FFTW_VER)-%/config.status: $(SRCDIR)/srccache/fftw-$(FFTW_VER)/configure
	mkdir -p $(dir $@)
	@# try to configure with avx support. if that fails, try again without it
	cd $(dir $@) && \
	($< $(CONFIGURE_COMMON) $(FFTW_CONFIG) $(FFTW_ENABLE_$*) --enable-avx || \
	  $< $(CONFIGURE_COMMON) $(FFTW_CONFIG) $(FFTW_ENABLE_$*))
	$(MAKE) -C $(dir $@) clean
	touch -c $@

$(FFTW_SINGLE_SRC_TARGET): $(BUILDDIR)/fftw-$(FFTW_VER)-single/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/fftw-$(FFTW_VER)-single/checked: $(FFTW_SINGLE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@
$(FFTW_SINGLE_OBJ_TARGET): $(FFTW_SINGLE_SRC_TARGET)
	$(call make-install,fftw-$(FFTW_VER)-single,)
ifeq ($(OS), Darwin)
	$(INSTALL_NAME_CMD)libfftw3f.$(SHLIB_EXT) $(build_shlibdir)/libfftw3f.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libfftw3f_threads.$(SHLIB_EXT) $(build_shlibdir)/libfftw3f_threads.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) $(build_shlibdir)/libfftw3f.3.$(SHLIB_EXT) $(INSTALL_NAME_ID_DIR)libfftw3f.$(SHLIB_EXT) $(build_shlibdir)/libfftw3f_threads.$(SHLIB_EXT)
else ifeq ($(OS), WINNT)
	mv -f $(build_shlibdir)/libfftw3f-3.dll $@
else ifeq ($(OS), Linux)
	for filename in $(build_shlibdir)/libfftw3f_threads.so* ; do \
		[ -L $$filename ] || $(PATCHELF_BIN) --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch -c $@

$(FFTW_DOUBLE_SRC_TARGET): $(BUILDDIR)/fftw-$(FFTW_VER)-double/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/fftw-$(FFTW_VER)-double/checked: $(FFTW_DOUBLE_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@
$(FFTW_DOUBLE_OBJ_TARGET): $(FFTW_DOUBLE_SRC_TARGET)
	$(call make-install,fftw-$(FFTW_VER)-double,)
ifeq ($(OS), Darwin)
	$(INSTALL_NAME_CMD)libfftw3.$(SHLIB_EXT) $(build_shlibdir)/libfftw3.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libfftw3_threads.$(SHLIB_EXT) $(build_shlibdir)/libfftw3_threads.$(SHLIB_EXT)
	$(INSTALL_NAME_CHANGE_CMD) $(build_shlibdir)/libfftw3.3.$(SHLIB_EXT) $(INSTALL_NAME_ID_DIR)libfftw3.$(SHLIB_EXT) $(build_shlibdir)/libfftw3_threads.$(SHLIB_EXT)
else ifeq ($(OS), WINNT)
	mv -f $(build_shlibdir)/libfftw3-3.dll $@
else ifeq ($(OS), Linux)
	for filename in $(build_shlibdir)/libfftw3_threads.so* ; do \
		[ -L $$filename ] || $(PATCHELF_BIN) --set-rpath '$$ORIGIN' $$filename ;\
	done
endif
	touch -c $@

ifneq ($(PATCHELF),patchelf)
$(FFTW_DOUBLE_OBJ_TARGET): $(PATCHELF)
$(FFTW_SINGLE_OBJ_TARGET): $(PATCHELF)
endif

clean-fftw: clean-fftw-single clean-fftw-double
clean-fftw-single:
	-$(MAKE) -C $(BUILDDIR)/fftw-$(FFTW_VER)-single clean
	-rm -f $(FFTW_SINGLE_OBJ_TARGET)
clean-fftw-double:
	-$(MAKE) -C $(BUILDDIR)/fftw-$(FFTW_VER)-double clean
	-rm -f $(FFTW_DOUBLE_OBJ_TARGET)
distclean-fftw: distclean-fftw-single distclean-fftw-double
	-rm -rf $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz $(SRCDIR)/srccache/fftw-$(FFTW_VER)
distclean-fftw-single:
	-rm -rf $(BUILDDIR)/fftw-$(FFTW_VER)-single
distclean-fftw-double:
	-rm -rf $(BUILDDIR)/fftw-$(FFTW_VER)-double

get-fftw: get-fftw-single get-fftw-double
configure-fftw: configure-fftw-single configure-fftw-double
compile-fftw: compile-fftw-single compile-fftw-double
check-fftw: check-fftw-single check-fftw-double
install-fftw: install-fftw-single install-fftw-double

get-fftw-single: $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz
configure-fftw-single: $(BUILDDIR)/fftw-$(FFTW_VER)-single/config.status
compile-fftw-single: $(FFTW_SINGLE_SRC_TARGET)
check-fftw-single: $(BUILDDIR)/fftw-$(FFTW_VER)-single/checked
install-fftw-single: $(FFTW_SINGLE_OBJ_TARGET)

get-fftw-double: $(SRCDIR)/srccache/fftw-$(FFTW_VER).tar.gz
configure-fftw-double: $(BUILDDIR)/fftw-$(FFTW_VER)-double/config.status
compile-fftw-double: $(FFTW_DOUBLE_SRC_TARGET)
check-fftw-double: $(BUILDDIR)/fftw-$(FFTW_VER)-double/checked
install-fftw-double: $(FFTW_DOUBLE_OBJ_TARGET)
