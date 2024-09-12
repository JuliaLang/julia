## TERMINFO-DB ##
include $(SRCDIR)/terminfo.version

$(SRCCACHE)/TermInfoDB-v$(TERMINFO_VER).any.tar.gz: | $(SRCCACHE)
	$(JLDOWNLOAD) $@ https://github.com/JuliaBinaryWrappers/TermInfoDB_jll.jl/releases/download/$(TERMINFO_TAG)/TermInfoDB.v$(TERMINFO_VER).any.tar.gz
	touch -c $@

$(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)/source-extracted: $(SRCCACHE)/TermInfoDB-v$(TERMINFO_VER).any.tar.gz
	$(JLCHECKSUM) $<
	rm -rf $(dir $@)
	mkdir -p $(dir $@)
	$(TAR) -C $(dir $@) --strip-components 1 -xf $<
	echo 1 > $@

checksum-terminfo: $(SRCCACHE)/TermInfoDB-v$(TERMINFO_VER).any.tar.gz
	$(JLCHECKSUM) $<

$(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)/build-compiled: $(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)/source-extracted
	echo 1 > $@

$(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)/build-checked: $(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)/build-compiled
	echo 1 > $@

define TERMINFO_INSTALL
	mkdir -p $2/$$(build_datarootdir)
	cp -R $1/terminfo $2/$$(build_datarootdir)
endef
$(eval $(call staged-install, \
	terminfo,TermInfoDB-v$(TERMINFO_VER), \
	TERMINFO_INSTALL,,,,))

clean-terminfo:
	-rm -f $(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)/build-compiled

distclean-terminfo:
	rm -rf $(SRCCACHE)/TermInfoDB*.tar.gz $(SRCCACHE)/TermInfoDB-v$(TERMINFO_VER) $(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)

get-terminfo: $(SRCCACHE)/TermInfoDB-v$(TERMINFO_VER).any.tar.gz
extract-terminfo: $(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)/source-extracted
configure-terminfo: extract-terminfo
compile-terminfo: $(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)/build-compiled
fastcheck-terminfo: check-terminfo
check-terminfo: $(BUILDDIR)/TermInfoDB-v$(TERMINFO_VER)/build-checked
