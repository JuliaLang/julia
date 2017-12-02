## cilkrts ##
$(SRCCACHE)/cilkrts-$(CILKRTS_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://www.cilkplus.org/sites/default/files/runtime_source/cilkplus-rtl-$(CILKRTS_VER).tgz

$(SRCCACHE)/cilkrts-$(CILKRTS_VER)/source-extracted: $(SRCCACHE)/cilkrts-$(CILKRTS_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@) && \
	$(TAR) -C $(dir $@) --strip-components 1 -xf $< 
	touch -c $(SRCCACHE)/cilkrts-$(CILKRTS_VER)/configure # old target
	echo 1 > $@

$(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-configured: $(SRCCACHE)/cilkrts-$(CILKRTS_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $<) && \
	libtoolize && aclocal && automake --add-missing && autoconf
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) LDFLAGS="$(CXXLDFLAGS)" CPPFLAGS="$(CPPFLAGS)"
	echo 1 > $@

$(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-compiled: $(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-checked: $(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	#$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	cilkrts,cilkrts-$(CILKRTS_VER), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD),,))

clean-cilkrts:
	-rm $(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-configured \
		$(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/cilkrts-$(CILKRTS_VER) clean

distclean-cilkrts:
	-rm -rf $(SRCCACHE)/cilkrts-$(CILKRTS_VER).tar.gz \
		$(SRCCACHE)/cilkrts-$(CILKRTS_VER) \
		$(BUILDDIR)/cilkrts-$(CILKRTS_VER)


get-cilkrts: $(SRCCACHE)/cilkrts-$(CILKRTS_VER).tar.gz
extract-cilkrts: $(SRCCACHE)/cilkrts-$(CILKRTS_VER)/source-extracted
configure-cilkrts: $(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-configured
compile-cilkrts: $(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-compiled
check-cilkrts: $(BUILDDIR)/cilkrts-$(CILKRTS_VER)/build-checked
