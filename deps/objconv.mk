## objconv ##

OBJCONV_SOURCE := $(BUILDDIR)/objconv/objconv
OBJCONV_TARGET := $(build_bindir)/objconv

$(SRCDIR)/srccache/objconv.zip: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ http://www.agner.org/optimize/objconv.zip
$(BUILDDIR)/objconv/config.status: $(SRCDIR)/srccache/objconv.zip
	-rm -r $(dir $@)
	unzip -d $(dir $@) $<
	cd $(dir $@) && unzip source.zip
	echo 1 > $@
$(OBJCONV_SOURCE): $(BUILDDIR)/objconv/config.status
	cd $(dir $<) && $(CXX) -o objconv -O2 *.cpp
$(OBJCONV_TARGET): $(OBJCONV_SOURCE) | $(build_bindir)
	cp -f $< $@

clean-objconv:
	-rm -f $(OBJCONV_TARGET)
distclean-objconv:
	-rm -rf $(SRCDIR)/srccache/objconv.zip $(BUILDDIR)/objconv

get-objconv: $(SRCDIR)/srccache/objconv.zip
configure-objconv: $(BUILDDIR)/objconv/config.status
compile-objconv: $(OBJCONV_SOURCE)
check-objconv: compile-objconv
install-objconv: $(OBJCONV_TARGET)
