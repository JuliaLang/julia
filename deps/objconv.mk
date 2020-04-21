## objconv ##

ifneq ($(USE_BINARYBUILDER_OBJCONV),1)

$(SRCCACHE)/objconv.zip: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ http://www.agner.org/optimize/objconv.zip

$(BUILDDIR)/objconv/source-extracted: $(SRCCACHE)/objconv.zip
	-rm -r $(dir $@)
	mkdir -p $(BUILDDIR)
	unzip -d $(dir $@) $<
	cd $(dir $@) && unzip source.zip
	echo 1 > $@

$(BUILDDIR)/objconv/build-compiled: $(BUILDDIR)/objconv/source-extracted
	cd $(dir $<) && $(CXX) -o objconv -O2 *.cpp
	echo 1 > $@

$(eval $(call staged-install, \
	objconv,objconv, \
	BINFILE_INSTALL,$$(BUILDDIR)/objconv/objconv,,))

clean-objconv:
	-rm $(BUILDDIR)/objconv/build-compiled $(build_depsbindir)/objconv

distclean-objconv:
	-rm -rf $(SRCCACHE)/objconv.zip $(BUILDDIR)/objconv


get-objconv: $(SRCCACHE)/objconv.zip
extract-objconv: $(BUILDDIR)/objconv/source-extracted
configure-objconv: extract-objconv
compile-objconv: $(BUILDDIR)/objconv/build-compiled
fastcheck-objconv: check-objconv
check-objconv: compile-objconv

else

$(eval $(call install-jll-and-artifact,Objconv_jll))

endif
