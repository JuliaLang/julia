## hwloc ##

$(SRCCACHE)/hwloc-$(HWLOC_VER).tar.bz2: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://download.open-mpi.org/release/hwloc/v$(HWLOC_VER_SHORT)/hwloc-$(HWLOC_VER).tar.bz2

$(SRCCACHE)/hwloc-$(HWLOC_VER)/source-extracted: $(SRCCACHE)/hwloc-$(HWLOC_VER).tar.bz2
	$(JLCHECKSUM) $<
	mkdir $(dir $@)
	cd $(dir $@) && $(TAR) jxf $< --strip-components=1
	touch -c $(SRCCACHE)/hwloc-$(HWLOC_VER)/configure # old target
	echo 1 > $@

checksum-hwloc: $(SRCCACHE)/hwloc-$(HWLOC_VER).tar.bz2
	$(JLCHECKSUM) $<

# We could set a symbol prefix as well
# ideally we would use `--embedded-mode` but that leads to nothing being installed
CONFIGURE_HWLOC := --disable-io --disable-shared --enable-static

$(BUILDDIR)/hwloc-$(HWLOC_VER)/build-configured: CC:=$(HOSTCC)
$(BUILDDIR)/hwloc-$(HWLOC_VER)/build-configured: CXX:=$(HOSTCXX)
$(BUILDDIR)/hwloc-$(HWLOC_VER)/build-configured: XC_HOST:=$(BUILD_MACHINE)
$(BUILDDIR)/hwloc-$(HWLOC_VER)/build-configured: $(SRCCACHE)/hwloc-$(HWLOC_VER)/source-extracted
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) $(CONFIGURE_HWLOC) LDFLAGS="$(CXXLDFLAGS)" CPPFLAGS="$(CPPFLAGS)"
	echo 1 > $@

$(BUILDDIR)/hwloc-$(HWLOC_VER)/build-compiled: $(BUILDDIR)/hwloc-$(HWLOC_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/hwloc-$(HWLOC_VER)/build-checked: $(BUILDDIR)/hwloc-$(HWLOC_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	# disabled due to bug in v0.6
	#$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	hwloc,hwloc-$(HWLOC_VER), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD),,))

clean-hwloc:
	-rm $(BUILDDIR)/hwloc-$(HWLOC_VER)/build-configured \
		$(BUILDDIR)/hwloc-$(HWLOC_VER)/build-compiled
	-$(MAKE) -C $(BUILDDIR)/hwloc-$(HWLOC_VER) clean

distclean-hwloc:
	-rm -rf $(SRCCACHE)/hwloc-$(HWLOC_VER).tar.bz2 \
		$(SRCCACHE)/hwloc-$(HWLOC_VER) \
		$(BUILDDIR)/hwloc-$(HWLOC_VER)


get-hwloc: $(SRCCACHE)/hwloc-$(HWLOC_VER).tar.bz2
extract-hwloc: $(SRCCACHE)/hwloc-$(HWLOC_VER)/source-extracted
configure-hwloc: $(BUILDDIR)/hwloc-$(HWLOC_VER)/build-configured
compile-hwloc: $(BUILDDIR)/hwloc-$(HWLOC_VER)/build-compiled
check-hwloc: $(BUILDDIR)/hwloc-$(HWLOC_VER)/build-checked
