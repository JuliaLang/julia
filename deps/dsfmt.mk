## DSFMT ##

ifneq ($(USE_BINARYBUILDER_DSFMT),1)

DSFMT_CFLAGS := $(CFLAGS) -DNDEBUG -DDSFMT_MEXP=19937 $(fPIC) -DDSFMT_DO_NOT_USE_OLD_NAMES -DDSFMT_SHLIB
DSFMT_CFLAGS += -O3 -finline-functions -fomit-frame-pointer -fno-strict-aliasing \
		--param max-inline-insns-single=1800 -Wall  -std=c99 -shared
ifeq ($(ARCH), x86_64)
DSFMT_CFLAGS += -msse2 -DHAVE_SSE2
endif

$(SRCCACHE)/dsfmt-$(DSFMT_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/MersenneTwister-Lab/dSFMT/archive/v$(DSFMT_VER).tar.gz
	touch -c $@

$(BUILDDIR)/dsfmt-$(DSFMT_VER)/source-extracted: $(SRCCACHE)/dsfmt-$(DSFMT_VER).tar.gz
	$(JLCHECKSUM) $<
	rm -rf $(dir $@)
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	echo 1 > $@

checksum-dsfmt: $(SRCCACHE)/dsfmt-$(DSFMT_VER).tar.gz
	$(JLCHECKSUM) $<

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
	-rm -f $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-compiled
	-rm -f $(BUILDDIR)/dsfmt-$(DSFMT_VER)/libdSFMT.$(SHLIB_EXT)

distclean-dsfmt:
	rm -rf $(SRCCACHE)/dsfmt*.tar.gz $(SRCCACHE)/dsfmt-$(DSFMT_VER) $(BUILDDIR)/dsfmt-$(DSFMT_VER)

get-dsfmt: $(SRCCACHE)/dsfmt-$(DSFMT_VER).tar.gz
extract-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/source-extracted
configure-dsfmt: extract-dsfmt
compile-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-compiled
fastcheck-dsfmt: check-dsfmt
check-dsfmt: $(BUILDDIR)/dsfmt-$(DSFMT_VER)/build-checked

else

$(eval $(call bb-install,dsfmt,DSFMT,false))

endif # USE_BINARYBUILDER_DSFMT
