## objconv ##
include $(SRCDIR)/objconv.version

ifneq ($(USE_BINARYBUILDER_OBJCONV),1)

$(SRCCACHE)/objconv-$(OBJCONV_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/staticfloat/objconv/archive/refs/tags/v$(OBJCONV_VER).tar.gz

$(BUILDDIR)/objconv/source-extracted: $(SRCCACHE)/objconv-$(OBJCONV_VER).tar.gz
	cd $(dir $<) && $(TAR) -zxf $(notdir $<)
	echo 1 > $@

$(BUILDDIR)/objconv/build-compiled: $(BUILDDIR)/objconv/source-extracted
	cd $(dir $<) && $(CXX) -o objconv -O2 src/*.cpp
	echo 1 > $@

$(eval $(call staged-install, \
	objconv,objconv, \
	BINFILE_INSTALL,$$(BUILDDIR)/objconv/objconv,,))

clean-objconv:
	-rm -f $(BUILDDIR)/objconv/build-compiled $(build_depsbindir)/objconv

distclean-objconv:
	rm -rf $(SRCCACHE)/objconv-$(OBJCONV_VER).tar.gz $(BUILDDIR)/objconv

get-objconv: $(SRCCACHE)/objconv-$(OBJCONV_VER).tar.gz
extract-objconv: $(BUILDDIR)/objconv/source-extracted
configure-objconv: extract-objconv
compile-objconv: $(BUILDDIR)/objconv/build-compiled
fastcheck-objconv: check-objconv
check-objconv: compile-objconv

else

$(eval $(call bb-install,objconv,OBJCONV,false))

endif
