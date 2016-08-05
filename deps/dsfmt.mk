## DSFMT ##

DSFMT_CFLAGS := $(CFLAGS) -DNDEBUG -DDSFMT_MEXP=19937 $(fPIC) -DDSFMT_DO_NOT_USE_OLD_NAMES
ifneq ($(USEMSVC), 1)
DSFMT_CFLAGS += -O3 -finline-functions -fomit-frame-pointer -fno-strict-aliasing \
		--param max-inline-insns-single=1800 -Wmissing-prototypes -Wall  -std=c99 -shared
else
DSFMT_CFLAGS += -Wl,-dll,-def:../../libdSFMT.def
endif
ifeq ($(ARCH), x86_64)
DSFMT_CFLAGS += -msse2 -DHAVE_SSE2
endif

$(SRCDIR)/srccache/dsfmt-$(DSFMT_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz
	touch -c $@

$(BUILDDIR)/dsfmt-$(DSFMT_VER)/source-extracted: $(SRCDIR)/srccache/dsfmt-$(DSFMT_VER).tar.gz
	$(JLCHECKSUM) $<
	-rm -r $(dir $@)
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	cd $(dir $@) && patch < $(SRCDIR)/patches/dSFMT.h.patch
	cd $(dir $@) && patch < $(SRCDIR)/patches/dSFMT.c.patch
	echo 1 > $@

$(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-compiled: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/source-extracted
	cd $(dir $<) && \
	$(CC) $(CPPFLAGS) $(DSFMT_CFLAGS) $(LDFLAGS) dSFMT.c -o libdSFMT.$(SHLIB_EXT)
	echo 1 > $@

$(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-checked: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) std-check sse2-check
endif
	echo 1 > $@

$(build_prefix)/manifest/dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-compiled | $(build_includedir) $(build_shlibdir) $(build_prefix)/manifest
	cp $(dir $<)/dSFMT.h $(build_includedir)
	cp $(BUILDDIR)/dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT) $(build_shlibdir)/libdSFMT.$(SHLIB_EXT)
	$(INSTALL_NAME_CMD)libdSFMT.$(SHLIB_EXT) $(build_shlibdir)/libdSFMT.$(SHLIB_EXT)
	echo $(DSFMT_VER) > $@

clean-dsfmt:
	-rm -f $(build_prefix)/manifest/dsfmt $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-compiled $(BUILDDIR)/dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT)

distclean-dsfmt:
	-rm -rf $(SRCDIR)/srccache/dsfmt*.tar.gz $(SRCDIR)/srccache/dsfmt-$(DSFMT_VER) $(BUILDDIR)/dsfmt-$(DSFMT_VER)

get-dsfmt: $(SRCDIR)/srccache/dsfmt-$(DSFMT_VER).tar.gz
extract-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/source-extracted
configure-dsfmt: extract-dsfmt
compile-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-compiled
check-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-checked
install-dsfmt: $(build_prefix)/manifest/dsfmt
