## p7zip ##
include $(SRCDIR)/p7zip.version

ifneq ($(USE_BINARYBUILDER_P7ZIP),1)

$(SRCCACHE)/7z$(subst .,,$(P7ZIP_VER))-src.tar.xz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://downloads.sourceforge.net/project/sevenzip/7-Zip/$(P7ZIP_VER)/7z$(subst .,,$(P7ZIP_VER))-src.tar.xz

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/source-extracted: $(SRCCACHE)/7z$(subst .,,$(P7ZIP_VER))-src.tar.xz
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@)
	cd $(dir $@) && $(TAR) -Jxf $<
	echo 1 > $@

checksum-p7zip: $(SRCCACHE)/7z$(subst .,,$(P7ZIP_VER))-src.tar.xz
	$(JLCHECKSUM) $<

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/source-extracted
$(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-compiled: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured
	$(MAKE) -C $(dir $<)CPP/7zip/Bundles/Alone -f makefile.gcc
	echo 1 > $@

define P7ZIP_INSTALL
	mkdir -p $2/$$(build_bindir)
	cp -a $1/CPP/7zip/Bundles/Alone/_o/7za$(EXE) $2/$$(build_bindir)/7z$(EXE)
endef
$(eval $(call staged-install, \
	p7zip,p7zip-$(P7ZIP_VER), \
	P7ZIP_INSTALL,,,))

clean-p7zip:
	-rm -f $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-compiled
	-rm -f $(build_bindir)/7za
	-$(MAKE) -C $(BUILDDIR)/p7zip-$(P7ZIP_VER) clean

distclean-p7zip:
	rm -rf $(SRCCACHE)/7z$(subst .,,$(P7ZIP_VER))-src.tar.xz $(SRCCACHE)/p7zip-$(P7ZIP_VER) $(BUILDDIR)/p7zip-$(P7ZIP_VER)


get-p7zip: $(SRCCACHE)/7z$(subst .,,$(P7ZIP_VER))-src.tar.xz
extract-p7zip: $(SRCCACHE)/p7zip-$(P7ZIP_VER)/source-extracted
configure-p7zip: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured
compile-p7zip: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-compiled
fastcheck-p7zip: check-p7zip
check-p7zip: compile-p7zip


else # USE_BINARYBUILDER_P7ZIP

$(eval $(call bb-install,p7zip,P7ZIP,false))

endif
