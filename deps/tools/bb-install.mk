define bb-install
# If the user has signified that this is a GCC-multiversioned tarball, then generate the proper tarball
ifeq ($(3),true)
$(2)_BB_TRIPLET := $(shell python $(call cygpath_w,$(JULIAHOME)/contrib/normalize_triplet.py) $(or $(XC_HOST),$(XC_HOST),$(BUILD_MACHINE)) $(lastword $(shell $(FC) --version | head -1)))
else
$(2)_BB_TRIPLET := $(shell python $(call cygpath_w,$(JULIAHOME)/contrib/normalize_triplet.py) $(or $(XC_HOST),$(XC_HOST),$(BUILD_MACHINE)))
endif
$(2)_BB_URL := $$($(2)_BB_URL_BASE)/$$($(2)_BB_NAME).$$($(2)_BB_TRIPLET).tar.gz

$$(BUILDDIR)/$(1)-$$($(2)_BB_NAME):
	mkdir -p $$@

$$(BUILDDIR)/$(1)-$$($(2)_BB_NAME)/$(2).$$($(2)_BB_TRIPLET).tar.gz: | $$(BUILDDIR)/$(1)-$$($(2)_BB_NAME)
	$$(JLDOWNLOAD) $$@ $$($(2)_BB_URL)

$$(BUILDDIR)/$(1)-$$($(2)_BB_NAME)/build-compiled: | $$(BUILDDIR)/$(1)-$$($(2)_BB_NAME)/$(2).$$($(2)_BB_TRIPLET).tar.gz
	echo 1 > $$@

$$(eval $$(call staged-install,$(1),$(1)-$$$$($(2)_BB_NAME),,,,))

#Override provision of stage tarball
$$(build_staging)/$(1)-$$($(2)_BB_NAME).tgz: $$(BUILDDIR)/$(1)-$$($(2)_BB_NAME)/$(2).$$($(2)_BB_TRIPLET).tar.gz | $$(build_staging)
	cp $$< $$@

clean-$(1):
distclean-$(1):
get-$(1): $$(BUILDDIR)/$(1)-$$($(2)_BB_NAME)/$(2).$$($(2)_BB_TRIPLET).tar.gz
extract-$(1):
configure-$(1):
compile-$(1):
fastcheck-$(1):
check-$(1):
endef
