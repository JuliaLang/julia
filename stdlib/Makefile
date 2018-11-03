default: install

.PHONY: default extract-pkg get-pkg clean-pkg getall install

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
SRCCACHE := $(abspath $(SRCDIR)/srccache)
BUILDDIR := $(SRCCACHE)

include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/tools/common.mk
include $(JULIAHOME)/deps/tools/git-external.mk

VERSDIR := v`cut -d. -f1-2 < $(JULIAHOME)/VERSION`

# Download and extract Pkg
PKG := https://github.com/JuliaLang/Pkg.jl
PKG_TAR_URL = https://api.github.com/repos/JuliaLang/Pkg.jl/tarball/$1
$(eval $(call git-external,Pkg,PKG,,,$(BUILDDIR)))

extract-pkg: $(BUILDDIR)/$(PKG_SRC_DIR)/source-extracted
Pkg: $(BUILDDIR)/$(PKG_SRC_DIR)/source-extracted
	rm -rf $@
	cp -r $(BUILDDIR)/$(PKG_SRC_DIR) $@
get-pkg: Pkg
clean-pkg:
	-rm -rf Pkg
	-rm -rf $(BUILDDIR)/$(PKG_SRC_DIR)

# Generate symlinks to all stdlibs from usr/share/julia/stdlib/vX.Y/
STDLIBS = Base64 CRC32c Dates DelimitedFiles Distributed FileWatching \
          Future InteractiveUtils Libdl LibGit2 LinearAlgebra Logging \
          Markdown Mmap Printf Profile Random REPL Serialization SHA \
          SharedArrays Sockets SparseArrays Statistics SuiteSparse Test Unicode UUIDs

$(foreach pkg, $(STDLIBS), $(eval $(call symlink_target,$(JULIAHOME)/stdlib/$(pkg),$(build_datarootdir)/julia/stdlib/$(shell echo $(VERSDIR)),$(pkg))))
$(eval $(call symlink_target,$(BUILDROOT)/stdlib/Pkg,$(build_datarootdir)/julia/stdlib/$(shell echo $(VERSDIR)),Pkg))

$(build_datarootdir)/julia/stdlib/$(shell echo $(VERSDIR)):
	mkdir -p $@

STDLIBS_LINK_TARGETS = $(addprefix $(build_datarootdir)/julia/stdlib/$(shell echo $(VERSDIR))/, $(STDLIBS) Pkg)

install: Pkg $(STDLIBS_LINK_TARGETS)
getall: get-pkg

