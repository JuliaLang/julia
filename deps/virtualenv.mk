## virtualenv

VIRTUALENV_SOURCE := $(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER)/virtualenv.py
VIRTUALENV_TARGET := $(BUILDDIR)/julia-env

$(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER).tar.gz: | $(SRCDIR)/srccache
	$(JLDOWNLOAD) $@ https://pypi.python.org/packages/source/v/virtualenv/$(notdir $@)
$(VIRTUALENV_SOURCE): $(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER).tar.gz
	$(JLCHECKSUM) $<
	cd $(dir $<) && $(TAR) zxf $<
	touch -c $@
$(VIRTUALENV_TARGET): $(VIRTUALENV_SOURCE)
	"$(shell $(SRCDIR)/tools/find_python2)" $< $@
ifeq ($(BUILD_OS), WINNT)
	-[ -e $@/Scripts ] && ! [ -e $@/bin ] && cmd //C mklink //J `echo $@/bin $@/Scripts | sed -e 's#/#\\\\#g'`
endif
	touch -c $@

clean-virtualenv:
	-rm -rf $(VIRTUALENV_TARGET)
distclean-virtualenv: clean-virtualenv
	-rm -rf $(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER).tar.gz \
		$(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER)

get-virtualenv: $(SRCDIR)/srccache/virtualenv-$(VIRTUALENV_VER).tar.gz
configure-virtualenv: get-virtualenv
compile-virtualenv: $(VIRTUALENV_SOURCE)
check-virtualenv: compile-virtualenv
install-virtualenv: $(VIRTUALENV_TARGET)
