## DSFMT ##

DSFMT_OBJ_TARGET := $(build_shlibdir)/libdSFMT.$(SHLIB_EXT) $(build_includedir)/dSFMT.h
DSFMT_OBJ_SOURCE := $(BUILDDIR)/dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT)

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
$(BUILDDIR)/dsfmt-$(DSFMT_VER)/config.status: $(SRCDIR)/srccache/dsfmt-$(DSFMT_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $< && \
	cd $(dir $@) && patch < $(SRCDIR)/patches/dSFMT.h.patch
	cd $(dir $@) && patch < $(SRCDIR)/patches/dSFMT.c.patch
	echo 1 > $@
$(DSFMT_OBJ_SOURCE): $(BUILDDIR)/dsfmt-$(DSFMT_VER)/config.status
	cd $(dir $<) && \
	$(CC) $(CPPFLAGS) $(DSFMT_CFLAGS) $(LDFLAGS) dSFMT.c -o libdSFMT.$(SHLIB_EXT)
$(BUILDDIR)/dsfmt-$(DSFMT_VER)/checked: $(DSFMT_OBJ_SOURCE)
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) std-check sse2-check
endif
	echo 1 > $@
$(build_shlibdir)/libdSFMT%$(SHLIB_EXT) $(build_includedir)/dSFMT%h: $(DSFMT_OBJ_SOURCE) | $(build_includedir) $(build_shlibdir)
	cp $(dir $<)/dSFMT.h $(build_includedir)
	cp $< $(build_shlibdir)/libdSFMT.$(SHLIB_EXT) && \
	$(INSTALL_NAME_CMD)libdSFMT.$(SHLIB_EXT) $(build_shlibdir)/libdSFMT.$(SHLIB_EXT)

clean-dsfmt:
	-rm -f $(BUILDDIR)/dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT)
distclean-dsfmt:
	-rm -rf $(SRCDIR)/srccache/dsfmt*.tar.gz $(SRCDIR)/srccache/dsfmt-$(DSFMT_VER) $(BUILDDIR)/dsfmt-$(DSFMT_VER)

get-dsfmt: $(SRCDIR)/srccache/dsfmt-$(DSFMT_VER).tar.gz
configure-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/config.status
compile-dsfmt: $(DSFMT_OBJ_SOURCE)
check-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/checked
install-dsfmt: $(DSFMT_OBJ_TARGET)
