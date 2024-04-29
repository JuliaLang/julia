## nghttp2
include $(SRCDIR)/nghttp2.version

ifneq ($(USE_BINARYBUILDER_NGHTTP2), 1)

$(SRCCACHE)/nghttp2-$(NGHTTP2_VER).tar.bz2: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/nghttp2/nghttp2/releases/download/v$(NGHTTP2_VER)/$(notdir $@)

$(SRCCACHE)/nghttp2-$(NGHTTP2_VER)/source-extracted: $(SRCCACHE)/nghttp2-$(NGHTTP2_VER).tar.bz2
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) -jxf $<
	touch -c $(SRCCACHE)/nghttp2-$(NGHTTP2_VER)/configure # old target
	echo 1 > $@

checksum-nghttp2: $(SRCCACHE)/nghttp2-$(NGHTTP2_VER).tar.bz2
	$(JLCHECKSUM) $<

$(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-configured: $(SRCCACHE)/nghttp2-$(NGHTTP2_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) --enable-lib-only
	echo 1 > $@

$(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-compiled: $(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-checked: $(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	$(MAKE) -C $(dir $@) check $(NGHTTP2_CHECK_MFLAGS)
endif
	echo 1 > $@

$(eval $(call staged-install, \
	nghttp2,nghttp2-$(NGHTTP2_VER), \
	MAKE_INSTALL,,, \
	$$(INSTALL_NAME_CMD)libnghttp2.$$(SHLIB_EXT) $$(build_shlibdir)/libnghttp2.$$(SHLIB_EXT)))

clean-nghttp2:
	-rm -f $(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-configured $(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/nghttp2-$(NGHTTP2_VER) clean

distclean-nghttp2:
	rm -rf $(SRCCACHE)/nghttp2-$(NGHTTP2_VER).tar.bz2 \
		$(SRCCACHE)/nghttp2-$(NGHTTP2_VER) \
		$(BUILDDIR)/nghttp2-$(NGHTTP2_VER)

get-nghttp2: $(SRCCACHE)/nghttp2-$(NGHTTP2_VER).tar.bz2
extract-nghttp2: $(SRCCACHE)/nghttp2-$(NGHTTP2_VER)/source-extracted
configure-nghttp2: $(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-configured
compile-nghttp2: $(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-compiled
fastcheck-nghttp2: check-nghttp2
check-nghttp2: $(BUILDDIR)/nghttp2-$(NGHTTP2_VER)/build-checked

else

$(eval $(call bb-install,nghttp2,NGHTTP2,false))

endif
