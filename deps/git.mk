## Git
# only used for the mac binaries in contrib/mac/app/Makefile

GIT_SOURCE := $(BUILDDIR)/git-$(GIT_VER)/git
GIT_TARGET := $(build_libexecdir)/git

$(SRCDIR)/srccache/git-$(GIT_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://www.kernel.org/pub/software/scm/git/git-$(GIT_VER).tar.gz
$(BUILDDIR)/git-$(GIT_VER)/configure: $(SRCDIR)/srccache/git-$(GIT_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(BUILDDIR) && $(TAR) zxf $<
	touch -c $@
$(BUILDDIR)/git-$(GIT_VER)/config.status: $(BUILDDIR)/git-$(GIT_VER)/configure
	cd $(dir $@) && \
	./configure $(CONFIGURE_COMMON) --bindir="$(build_libexecdir)"
	touch -c $@
$(GIT_SOURCE): $(BUILDDIR)/git-$(GIT_VER)/config.status
	$(MAKE) -C $(dir $<)
	touch -c $@
$(BUILDDIR)/git-$(GIT_VER)/checked: $(GIT_SOURCE)
	echo 1 > $@
$(GIT_TARGET): $(GIT_SOURCE)
	$(call make-install,git-$(GIT_VER),NO_INSTALL_HARDLINKS=1 bindir="$(build_libexecdir)")
	touch -c $@

clean-git:
	-$(MAKE) -C $(BUILDDIR)/git-$(GIT_VER) clean
	-rm -f $(GIT_OBJ_TARGET)
distclean-git:
	-rm -rf $(SRCDIR)/srccache/git-$(GIT_VER).tar.gz \
		$(BUILDDIR)/git-$(GIT_VER)

get-git: $(SRCDIR)/srccache/git-$(GIT_VER).tar.gz
configure-git: $(BUILDDIR)/git-$(GIT_VER)/config.status
compile-git: $(GIT_SOURCE)
check-git: $(BUILDDIR)/git-$(GIT_VER)/checked
install-git: $(GIT_TARGET)
