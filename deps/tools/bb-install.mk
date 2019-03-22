define bb-install
# If the user has signified that this is a GCC-multiversioned tarball, then generate the proper tarball
ifeq ($(3),true)
$(2)_BB_TRIPLET := $(shell python $(call cygpath_w,$(JULIAHOME)/contrib/normalize_triplet.py) $(or $(XC_HOST),$(XC_HOST),$(BUILD_MACHINE)) "$(shell $(FC) --version | head -1)")
else
$(2)_BB_TRIPLET := $(shell python $(call cygpath_w,$(JULIAHOME)/contrib/normalize_triplet.py) $(or $(XC_HOST),$(XC_HOST),$(BUILD_MACHINE)))
endif
$(2)_BB_URL := $$($(2)_BB_URL_BASE)/$$($(2)_BB_NAME).$$($(2)_BB_TRIPLET).tar.gz
$(2)_BB_BASENAME := $$(shell basename $$($(2)_BB_URL))

$$(BUILDDIR)/$$($(2)_BB_NAME):
	mkdir -p $$@

$$(SRCCACHE)/$$($(2)_BB_BASENAME): | $$(SRCCACHE)
	$$(JLDOWNLOAD) $$@ $$($(2)_BB_URL)

$$(BUILDDIR)/$$($(2)_BB_NAME)/build-compiled: $$(BUILDDIR)/$$($(2)_BB_NAME) | $$(SRCCACHE)/$$($(2)_BB_BASENAME)
	$$(JLCHECKSUM) $$(SRCCACHE)/$$($(2)_BB_BASENAME)
	echo 1 > $$@

$$(eval $$(call staged-install,$(1),$$($(2)_BB_NAME),,,,))

#Override provision of stage tarball
$$(build_staging)/$$($(2)_BB_NAME).tgz: $$(SRCCACHE)/$$($(2)_BB_BASENAME) | $$(build_staging)
	cp $$< $$@

clean-bb-$(1):
	rm -f $$(build_staging)/$$($(2)_BB_BASENAME)
	rm -f $$(BUILDDIR)/$$($(2)_BB_NAME)/build-compiled

clean-bb-download-$(1):
	rm -f $$(SRCCACHE)/$$($(2)_BB_BASENAME)

clean-$(1): clean-bb-$(1)
distclean-$(1): clean-bb-$(1) clean-bb-download-$(1)
get-$(1): $$(SRCCACHE)/$$($(2)_BB_BASENAME)
extract-$(1):
configure-$(1):
compile-$(1): $$(BUILDDIR)/$$($(2)_BB_NAME)/build-compiled
install-$(1): compile-$(1)
fastcheck-$(1):
check-$(1):

.PHONY: clean-bb-$(1)
endef
