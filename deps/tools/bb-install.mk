#$(call bb-install, \
#    1 target, \               # name (lowercase)
#    2 variable, \             # name (uppercase)
#    3 gfortran, \             # signifies a GCC ABI (e.g. libgfortran version) dependency
#    4 cxx11)                  # signifies a cxx11 ABI dependency

# Auto-detect triplet once, create different versions that we use as defaults below for each BB install target
BB_TRIPLET_LIBGFORTRAN_CXXABI := $(shell $(call invoke_python,$(JULIAHOME)/contrib/normalize_triplet.py) $(or $(XC_HOST),$(XC_HOST),$(BUILD_MACHINE)) "$(shell $(FC) --version | head -1)" "$(or $(shell echo '\#include <string>' | $(CXX) $(CXXFLAGS) -x c++ -dM -E - | grep _GLIBCXX_USE_CXX11_ABI | awk '{ print $$3 }' ),1)")
BB_TRIPLET_LIBGFORTRAN := $(subst $(SPACE),-,$(filter-out cxx%,$(subst -,$(SPACE),$(BB_TRIPLET_LIBGFORTRAN_CXXABI))))
BB_TRIPLET_CXXABI := $(subst $(SPACE),-,$(filter-out libgfortran%,$(subst -,$(SPACE),$(BB_TRIPLET_LIBGFORTRAN_CXXABI))))
BB_TRIPLET := $(subst $(SPACE),-,$(filter-out cxx%,$(filter-out libgfortran%,$(subst -,$(SPACE),$(BB_TRIPLET_LIBGFORTRAN_CXXABI)))))

define bb-install
TRIPLET_VAR := BB_TRIPLET
ifeq ($(3),true)
TRIPLET_VAR := $$(TRIPLET_VAR)_LIBGFORTRAN
endif
ifeq ($(4),true)
TRIPLET_VAR := $$(TRIPLET_VAR)_CXXABI
endif
$(2)_BB_TRIPLET := $$($$(TRIPLET_VAR))
$(2)_BB_URL := $$($(2)_BB_URL_BASE)/$$($(2)_BB_NAME).$$($(2)_BB_TRIPLET).tar.gz
$(2)_BB_BASENAME := $$($(2)_BB_NAME)-$$($(2)_BB_REL).$$($(2)_BB_TRIPLET).tar.gz

$$(BUILDDIR)/$$($(2)_BB_NAME):
	mkdir -p $$@

$$(SRCCACHE)/$$($(2)_BB_BASENAME): | $$(SRCCACHE)
	$$(JLDOWNLOAD) $$@ $$($(2)_BB_URL)

stage-$(strip $1): $$(SRCCACHE)/$$($(2)_BB_BASENAME)
install-$(strip $1): $$(build_prefix)/manifest/$(strip $1)

reinstall-$(strip $1):
	+$$(MAKE) uninstall-$(strip $1)
	+$$(MAKE) stage-$(strip $1)
	+$$(MAKE) install-$(strip $1)

UNINSTALL_$(strip $1) := $$($(2)_BB_BASENAME:.tar.gz=) bb-uninstaller

$$(build_prefix)/manifest/$(strip $1): $$(SRCCACHE)/$$($(2)_BB_BASENAME) | $(build_prefix)/manifest
	-+[ ! -e $$@ ] || $$(MAKE) uninstall-$(strip $1)
	$$(JLCHECKSUM) $$<
	mkdir -p $$(build_prefix)
	$(UNTAR) $$< -C $$(build_prefix)
	echo '$$(UNINSTALL_$(strip $1))' > $$@

clean-bb-download-$(1):
	rm -f $$(SRCCACHE)/$$($(2)_BB_BASENAME)

clean-$(1):
distclean-$(1): clean-bb-download-$(1)
get-$(1): $$(SRCCACHE)/$$($(2)_BB_BASENAME)
extract-$(1):
configure-$(1):
compile-$(1): get-$(1)
fastcheck-$(1):
check-$(1):

.PHONY: clean-bb-$(1)

endef

define bb-uninstaller
uninstall-$(strip $1):
	-cd $$(build_prefix) && rm -fdv -- $$$$($$(TAR) -tzf $$(SRCCACHE)/$2.tar.gz --exclude './$$$$')
	-rm $$(build_prefix)/manifest/$(strip $1)
endef
