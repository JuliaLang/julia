default: get
SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
SRCCACHE := $(abspath $(SRCDIR)/srccache)
BUILDDIR := $(SRCCACHE)

include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/tools/common.mk
include $(JULIAHOME)/deps/tools/git-external.mk

PKG3_GIT_URL := https://github.com/JuliaLang/Pkg3.jl
PKG3_TAR_URL = https://api.github.com/repos/JuliaLang/Pkg3.jl/tarball/$1
$(eval $(call git-external,Pkg3,PKG3,,,$(BUILDDIR)))

extract-pkg3: $(BUILDDIR)/$(PKG3_SRC_DIR)/source-extracted
Pkg3: $(BUILDDIR)/$(PKG3_SRC_DIR)/source-extracted
	cp -r $(BUILDDIR)/$(PKG3_SRC_DIR) $@
get-pkg3: Pkg3
clean-pkg3:
	-rm -rf Pkg3
	-rm -rf $(BUILDDIR)/$(PKG3_SRC_DIR)

SHA_GIT_URL := https://github.com/staticfloat/SHA.jl
SHA_TAR_URL = https://api.github.com/repos/staticfloat/SHA.jl/tarball/$1
$(eval $(call git-external,SHA,SHA,,,$(BUILDDIR)))

extract-sha: $(BUILDDIR)/$(SHA_SRC_DIR)/source-extracted
SHA: $(BUILDDIR)/$(SHA_SRC_DIR)/source-extracted
	cp -r $(BUILDDIR)/$(SHA_SRC_DIR) $@
get-sha: SHA
clean-sha:
	-rm -rf SHA
	-rm -rf $(BUILDDIR)/$(SHA_SRC_DIR)

COMPAT_GIT_URL := https://github.com/JuliaLang/Compat.jl
COMPAT_TAR_URL = https://api.github.com/repos/JuliaLang/Compat.jl/tarball/$1
$(eval $(call git-external,Compat,COMPAT,,,$(BUILDDIR)))

extract-compat: $(BUILDDIR)/$(COMPAT_SRC_DIR)/source-extracted
Compat: $(BUILDDIR)/$(COMPAT_SRC_DIR)/source-extracted
	cp -r $(BUILDDIR)/$(COMPAT_SRC_DIR) $@
get-compat: Compat
clean-compat:
	-rm -rf Compat
	-rm -rf $(BUILDDIR)/$(COMPAT_SRC_DIR)
get: get-pkg3 get-sha get-compat
clean: clean-pkg3 clean-sha clean-compat

.PHONY: default get clean
