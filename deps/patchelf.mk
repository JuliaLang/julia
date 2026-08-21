## patchelf ##
include $(SRCDIR)/patchelf.version

$(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/NixOS/patchelf/releases/download/$(PATCHELF_VER)/patchelf-$(PATCHELF_VER).tar.bz2

$(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-extracted: $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2
	$(JLCHECKSUM) $<
	mkdir $(dir $@)
	cd $(dir $@) && $(TAR) -jxf $< --strip-components=1
	touch -c $(SRCCACHE)/patchelf-$(PATCHELF_VER)/configure # old target
	echo 1 > $@

checksum-patchelf: $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2
	$(JLCHECKSUM) $<

# Backport of https://github.com/NixOS/patchelf/pull/469 (in patchelf 0.18.0): without it,
# growing an rpath can produce a binary whose first two PT_LOAD segments share a page,
# which FreeBSD's execve() rejects (the process dies with a silent SIGABRT).
$(SRCCACHE)/patchelf-$(PATCHELF_VER)/patchelf-overlapping-segments.patch-applied: $(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-extracted
	cd $(dir $@) && \
		patch -p1 -f < $(SRCDIR)/patches/patchelf-overlapping-segments.patch
	echo 1 > $@

$(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-patched: $(SRCCACHE)/patchelf-$(PATCHELF_VER)/patchelf-overlapping-segments.patch-applied
	echo 1 > $@

$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: CC:=$(HOSTCC)
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: CXX:=$(HOSTCXX)
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: XC_HOST:=$(BUILD_MACHINE)
$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured: $(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-patched
	mkdir -p $(dir $@)
	cd $(dir $@) && \
	$(dir $<)/configure $(CONFIGURE_COMMON) LDFLAGS="$(CXXLDFLAGS)" CPPFLAGS="$(CPPFLAGS)" MAKE=$(MAKE)
	echo 1 > $@

$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured
	$(MAKE) -C $(dir $<)
	echo 1 > $@

$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-checked: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled
ifeq ($(OS),$(BUILD_OS))
	# disabled due to bug in v0.6
	#$(MAKE) -C $(dir $@) check
endif
	echo 1 > $@

$(eval $(call staged-install, \
	patchelf,patchelf-$(PATCHELF_VER), \
	MAKE_INSTALL,$$(LIBTOOL_CCLD),,))

clean-patchelf:
	-rm -f $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled
	-if [ -d $(BUILDDIR)/patchelf-$(PATCHELF_VER) ]; then $(MAKE) -C $(BUILDDIR)/patchelf-$(PATCHELF_VER) clean; fi

distclean-patchelf:
	rm -rf $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2 \
		$(SRCCACHE)/patchelf-$(PATCHELF_VER) \
		$(BUILDDIR)/patchelf-$(PATCHELF_VER)


get-patchelf: $(SRCCACHE)/patchelf-$(PATCHELF_VER).tar.bz2
extract-patchelf: $(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-extracted
patch-patchelf: $(SRCCACHE)/patchelf-$(PATCHELF_VER)/source-patched
configure-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-configured
compile-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-compiled
check-patchelf: $(BUILDDIR)/patchelf-$(PATCHELF_VER)/build-checked
