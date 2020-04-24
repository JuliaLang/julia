## p7zip ##

ifneq ($(USE_BINARYBUILDER_P7ZIP),1)
# Force optimization for P7ZIP flags (Issue #11668)
$(SRCCACHE)/p7zip-$(P7ZIP_VER).tar.bz2: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://downloads.sourceforge.net/project/p7zip/p7zip/16.02/p7zip_16.02_src_all.tar.bz2

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/source-extracted: $(SRCCACHE)/p7zip-$(P7ZIP_VER).tar.bz2
	$(JLCHECKSUM) $<
	mkdir -p $(dir $@)
	cd $(dir $@) && $(TAR) --strip-components 1 -jxf $<
	echo $1 > $@

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/p7zip-12-CVE-2016-9296.patch-applied: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/source-extracted
	cd $(dir $@) && patch -p1 -f < $(SRCDIR)/patches/p7zip-12-CVE-2016-9296.patch
	echo 1 > $@

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/p7zip-13-CVE-2017-17969.patch-applied: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/p7zip-12-CVE-2016-9296.patch-applied
	cd $(dir $@) && patch -p1 -f < $(SRCDIR)/patches/p7zip-13-CVE-2017-17969.patch
	echo 1 > $@

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/p7zip-15-Enhanced-encryption-strength.patch-applied: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/p7zip-13-CVE-2017-17969.patch-applied
	cd $(dir $@) && patch -p4 -f < $(SRCDIR)/patches/p7zip-15-Enhanced-encryption-strength.patch
	echo 1 > $@

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/p7zip-Windows_ErrorMsg.patch-applied: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/p7zip-15-Enhanced-encryption-strength.patch-applied
	cd $(dir $@) && patch -p0 -f < $(SRCDIR)/patches/p7zip-Windows_ErrorMsg.patch
	echo 1 > $@

$(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/p7zip-Windows_ErrorMsg.patch-applied
$(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-compiled: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured
	$(MAKE) -C $(dir $<) $(MAKE_COMMON) CC="$(CC)" CXX="$(CXX)" 7za
	echo 1 > $@

define P7ZIP_INSTALL
	mkdir -p $2/$$(build_libexecdir)
	cp -a $1/bin/7za $2/$$(build_libexecdir)/7z
endef
$(eval $(call staged-install, \
	p7zip,p7zip-$(P7ZIP_VER), \
	P7ZIP_INSTALL,,,))

clean-p7zip:
	-rm $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-compiled
	-rm $(build_bindir)/7za
	-$(MAKE) -C $(BUILDDIR)/p7zip-$(P7ZIP_VER) clean

distclean-p7zip:
	-rm -rf $(SRCCACHE)/p7zip-$(P7ZIP_VER).tar.bz2 $(SRCCACHE)/p7zip-$(P7ZIP_VER) $(BUILDDIR)/p7zip-$(P7ZIP_VER)


get-p7zip: $(SRCCACHE)/p7zip-$(P7ZIP_VER).tar.bz2
extract-p7zip: $(SRCCACHE)/p7zip-$(P7ZIP_VER)/source-extracted
configure-p7zip: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-configured
compile-p7zip: $(BUILDDIR)/p7zip-$(P7ZIP_VER)/build-compiled
fastcheck-p7zip: check-p7zip
check-p7zip: compile-p7zip

# If we built our own p7zip, we need to generate a fake p7zip_jll package to load it in:
$(eval $(call jll-generate,p7zip_jll,,p7zip=\"7z$(EXE)\",05ff407c-b0c1-5878-9df8-858cc2e60c36,))

else # USE_BINARYBUILDER_P7ZIP

# Install p7zip_jll into our stdlib folder
$(eval $(call install-jll-and-artifact,p7zip_jll))

endif
