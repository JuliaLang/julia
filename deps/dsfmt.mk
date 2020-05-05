## DSFMT ##

ifneq ($(USE_BINARYBUILDER_DSFMT),1)

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

$(SRCCACHE)/dsfmt-$(DSFMT_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-$(DSFMT_VER).tar.gz
	touch -c $@

$(BUILDDIR)/dsfmt-$(DSFMT_VER)/source-extracted: $(SRCCACHE)/dsfmt-$(DSFMT_VER).tar.gz
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

define DSFMT_INSTALL
	mkdir -p $2/$$(build_includedir)
	mkdir -p $2/$$(build_shlibdir)
	cp $1/dSFMT.h $2/$$(build_includedir)
	cp $1/libdSFMT.$$(SHLIB_EXT) $2/$$(build_shlibdir)
endef
$(eval $(call staged-install, \
	dsfmt,dsfmt-$(DSFMT_VER), \
	DSFMT_INSTALL,, \
	$$(DSFMT_OBJ_TARGET), \
	$$(INSTALL_NAME_CMD)libdSFMT.$$(SHLIB_EXT) $$(build_shlibdir)/libdSFMT.$$(SHLIB_EXT)))

clean-dsfmt:
	-rm $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-compiled
	-rm $(BUILDDIR)/dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT)

distclean-dsfmt:
	-rm -rf $(SRCCACHE)/dsfmt*.tar.gz $(SRCCACHE)/dsfmt-$(DSFMT_VER) $(BUILDDIR)/dsfmt-$(DSFMT_VER)

get-dsfmt: $(SRCCACHE)/dsfmt-$(DSFMT_VER).tar.gz
extract-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/source-extracted
configure-dsfmt: extract-dsfmt
compile-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-compiled
fastcheck-dsfmt: check-dsfmt
check-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-checked

else

DSFMT_BB_URL_BASE := https://github.com/JuliaPackaging/Yggdrasil/releases/download/dSFMT-v$(DSFMT_VER)-$(DSFMT_BB_REL)
DSFMT_BB_NAME := dSFMT.v$(DSFMT_VER)
$(eval $(call bb-install,dsfmt,DSFMT,false))

endif # USE_BINARYBUILDER_DSFMT
