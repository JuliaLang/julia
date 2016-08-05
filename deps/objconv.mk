## objconv ##

$(SRCDIR)/srccache/objconv.zip: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.agner.org/optimize/objconv.zip

$(BUILDDIR)/objconv/source-extracted: $(SRCDIR)/srccache/objconv.zip
	-rm -r $(dir $@)
	mkdir -p $(BUILDDIR)
	unzip -d $(dir $@) $<
	cd $(dir $@) && unzip source.zip
	echo 1 > $@

$(BUILDDIR)/objconv/build-compiled: $(BUILDDIR)/objconv/source-extracted
	cd $(dir $<) && $(CXX) -o objconv -O2 *.cpp
	echo 1 > $@

$(build_prefix)/manifest/objconv: $(BUILDDIR)/objconv/build-compiled | $(build_depsbindir) $(build_prefix)/manifest
	cp -f $(BUILDDIR)/objconv/objconv $(build_depsbindir)/objconv
	echo 2.42 > $@

clean-objconv:
	-rm -f $(build_prefix)/manifest/objconv $(BUILDDIR)/objconv/build-compiled $(build_depsbindir)/objconv

distclean-objconv:
	-rm -rf $(SRCDIR)/srccache/objconv.zip $(BUILDDIR)/objconv

get-objconv: $(SRCDIR)/srccache/objconv.zip
extract-objconv: $(BUILDDIR)/objconv/source-extracted
configure-objconv: extract-objconv
compile-objconv: $(BUILDDIR)/objconv/build-compiled
check-objconv: compile-objconv
install-objconv: $(build_prefix)/manifest/objconv
