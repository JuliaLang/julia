## p7zip ##

ifneq ($(USE_BINARYBUILDER_P7ZIP),1)

$(SRCCACHE)/p7zip-$(P7ZIP_VER).tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/jinfeihan57/p7zip/archive/refs/tags/v$(P7ZIP_VER).tar.gz

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/source-extracted: $(SRCCACHE)/p7zip-$(P7ZIP_VER).tar.gz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@)
	cd $(dir $@) && $(TAR) --strip-components 1 -zxf $<
	echo 1 > $@

checksum-p7zip: $(SRCCACHE)/p7zip-$(P7ZIP_VER).tar.gz
	$(JLCHECKSUM) $<

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/source-extracted
$(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-compiled: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON) CC="$(CC)" CXX="$(CXX)" 7za
	echo 1 > $@

define P7ZIP_INSTALL
	mkdir -p $2/$$(build_bindir)
	cp -a $1/bin/7za $2/$$(build_bindir)/7z
endef
$(eval $(call staged-install, \
	p7zip,p7zip-$(P7ZIP_VER), \
	P7ZIP_INSTALL,,,))

clean-p7zip:
	-rm -f $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-compiled
	-rm -f $(build_bindir)/7za
	-$(MAKE) -C $(BUILDDIR)/p7zip-$(P7ZIP_VER) clean

distclean-p7zip:
	rm -rf $(SRCCACHE)/p7zip-$(P7ZIP_VER).tar.gz $(SRCCACHE)/p7zip-$(P7ZIP_VER) $(BUILDDIR)/p7zip-$(P7ZIP_VER)


get-p7zip: $(SRCCACHE)/p7zip-$(P7ZIP_VER).tar.gz
extract-p7zip: $(SRCCACHE)/p7zip-$(P7ZIP_VER)/source-extracted
configure-p7zip: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured
compile-p7zip: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-compiled
fastcheck-p7zip: check-p7zip
check-p7zip: compile-p7zip


else # USE_BINARYBUILDER_P7ZIP

$(eval $(call bb-install,p7zip,P7ZIP,false))

endif
