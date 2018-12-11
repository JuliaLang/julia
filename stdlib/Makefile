default: install

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
SRCCACHE := $(abspath $(SRCDIR)/srccache)
BUILDDIR := .

include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/tools/common.mk
include $(JULIAHOME)/deps/tools/git-external.mk

VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)

$(build_datarootdir)/julia/stdlib/$(VERSDIR):
	mkdir -p $@

STDLIBS = Base64 CRC32c Dates DelimitedFiles Distributed FileWatching \
          Future InteractiveUtils Libdl LibGit2 LinearAlgebra Logging \
          Markdown Mmap Printf Profile Random REPL Serialization SHA \
          SharedArrays Sockets SparseArrays Statistics SuiteSparse Test Unicode UUIDs
STDLIBS-EXT = Pkg

# Download and extract Pkg
PKG_GIT_URL := git://github.com/JuliaLang/Pkg.jl.git
PKG_TAR_URL = https://api.github.com/repos/JuliaLang/Pkg.jl/tarball/$1
$(eval $(call git-external,Pkg,PKG,,,$(BUILDDIR)))
$(BUILDDIR)/$(PKG_SRC_DIR)/build-compiled: $(BUILDDIR)/$(PKG_SRC_DIR)/source-extracted
	@# no build steps
	echo 1 > $@
$(eval $(call symlink_install,Pkg,$$(PKG_SRC_DIR),$$(build_datarootdir)/julia/stdlib/$$(VERSDIR)))
clean-Pkg:
	-rm $(BUILDDIR)/$(PKG_SRC_DIR)/build-compiled
get-Pkg: $(PKG_SRC_FILE)
extract-Pkg: $(BUILDDIR)/$(PKG_SRC_DIR)/source-extracted
configure-Pkg: extract-Pkg
compile-Pkg: $(BUILDDIR)/$(PKG_SRC_DIR)/build-compiled


# Generate symlinks to all stdlibs at usr/share/julia/stdlib/vX.Y/
$(foreach module, $(STDLIBS), $(eval $(call symlink_target,$$(JULIAHOME)/stdlib/$(module),$$(build_datarootdir)/julia/stdlib/$$(VERSDIR),$(module))))

STDLIBS_LINK_TARGETS := $(addprefix $(build_datarootdir)/julia/stdlib/$(VERSDIR)/,$(STDLIBS))

getall get: get-Pkg
install: install-Pkg $(STDLIBS_LINK_TARGETS)
clean: clean-Pkg $(CLEAN_TARGETS)
distclean: distclean-Pkg clean
