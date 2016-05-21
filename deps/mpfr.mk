ifeq ($(USE_SYSTEM_GMP), 0)
MPFR_DEPS := $(GMP_OBJ_TARGET)
endif

## MPFR ##

ifeq ($(USE_SYSTEM_MPFR), 0)
ifeq ($(USE_SYSTEM_GMP), 0)
MPFR_OPTS := --with-gmp-include=$(abspath $(build_includedir)) --with-gmp-lib=$(abspath $(build_shlibdir))
endif
endif
ifeq ($(BUILD_OS),WINNT)
ifeq ($(OS),WINNT)
MPFR_OPTS += --disable-thread-safe CFLAGS="$(CFLAGS) -DNPRINTF_L -DNPRINTF_T -DNPRINTF_J"
endif
endif


MPFR_SRC_TARGET := $(BUILDDIR)/mpfr-$(MPFR_VER)/src/.libs/libmpfr.$(SHLIB_EXT)
MPFR_OBJ_TARGET := $(build_shlibdir)/libmpfr.$(SHLIB_EXT)
ifeq ($(OS),Darwin)
MPFR_CHECK_MFLAGS := LDFLAGS="$(LDFLAGS) -Wl,-rpath,'$(build_libdir)'"
endif

ifeq ($(SANITIZE),1)
# Force generic C build
MPFR_OPTS += --host=none-unknown-linux
endif

$(SRCDIR)/srccache/mpfr-$(MPFR_VER).tar.bz2: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.mpfr.org/mpfr-$(MPFR_VER)/$(notdir $@)
$(SRCDIR)/srccache/mpfr-$(MPFR_VER)/configure: $(SRCDIR)/srccache/mpfr-$(MPFR_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) jxf $<
	touch -c $@
$(BUILDDIR)/mpfr-$(MPFR_VER)/config.status: $(SRCDIR)/srccache/mpfr-$(MPFR_VER)/configure | $(MPFR_DEPS)
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$< $(CONFIGURE_COMMON) $(MPFR_OPTS) F77= --enable-shared --disable-static
	touch -c $@
$(MPFR_SRC_TARGET): $(BUILDDIR)/mpfr-$(MPFR_VER)/config.status
	$(MAKE) -C $(dir $<) $(LIBTOOL_CCLD)
	touch -c $@
$(BUILDDIR)/mpfr-$(MPFR_VER)/checked: $(MPFR_SRC_TARGET)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) $(LIBTOOL_CCLD) check $(MPFR_CHECK_MFLAGS)
endif
	echo 1 > $@
$(MPFR_OBJ_TARGET): $(MPFR_SRC_TARGET)
	$(call make-install,mpfr-$(MPFR_VER),$(LIBTOOL_CCLD))
	$(INSTALL_NAME_CMD)libmpfr.$(SHLIB_EXT) $@
	touch -c $@

clean-mpfr:
	-$(MAKE) -C $(BUILDDIR)/mpfr-$(MPFR_VER) clean
	-rm -f $(MPFR_OBJ_TARGET)
distclean-mpfr:
	-rm -rf $(SRCDIR)/srccache/mpfr-$(MPFR_VER).tar.bz2 \
		$(SRCDIR)/srccache/mpfr-$(MPFR_VER) \
		$(BUILDDIR)/mpfr-$(MPFR_VER)

get-mpfr: $(SRCDIR)/srccache/mpfr-$(MPFR_VER).tar.bz2
configure-mpfr: $(BUILDDIR)/mpfr-$(MPFR_VER)/config.status
compile-mpfr: $(MPFR_SRC_TARGET)
check-mpfr: $(BUILDDIR)/mpfr-$(MPFR_VER)/checked
install-mpfr: $(MPFR_OBJ_TARGET)
