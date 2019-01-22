default: install

SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
SRCCACHE := $(abspath $(SRCDIR)/srccache)
BUILDDIR := .

include $(JULIAHOME)/Make.inc
include $(JULIAHOME)/deps/tools/common.mk
include $(JULIAHOME)/deps/tools/stdlib-external.mk

VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)

$(build_datarootdir)/julia/stdlib/$(VERSDIR):
	mkdir -p $@

STDLIBS = Base64 CRC32c Dates DelimitedFiles Distributed FileWatching \
          Future InteractiveUtils Libdl LibGit2 LinearAlgebra Logging \
          Markdown Mmap Printf Profile Random REPL Serialization SHA JSON PlotlyBase \
          SharedArrays Sockets SparseArrays Statistics SuiteSparse Test Unicode UUIDs
STDLIBS_EXT = Pkg
PKG_GIT_URL := git://github.com/JuliaLang/Pkg.jl.git
PKG_TAR_URL = https://api.github.com/repos/JuliaLang/Pkg.jl/tarball/$1

$(foreach module, $(STDLIBS_EXT), $(eval $(call stdlib-external,$(module),$(shell echo $(module) | tr a-z A-Z))))

# Generate symlinks to all stdlibs at usr/share/julia/stdlib/vX.Y/
$(foreach module, $(STDLIBS), $(eval $(call symlink_target,$$(JULIAHOME)/stdlib/$(module),$$(build_datarootdir)/julia/stdlib/$$(VERSDIR),$(module))))

STDLIBS_LINK_TARGETS := $(addprefix $(build_datarootdir)/julia/stdlib/$(VERSDIR)/,$(STDLIBS))

getall get: $(addprefix get-, $(STDLIBS_EXT))
install: $(addprefix install-, $(STDLIBS_EXT)) $(STDLIBS_LINK_TARGETS)
clean: $(addprefix clean-, $(STDLIBS_EXT)) $(CLEAN_TARGETS)
distclean: $(addprefix distclean-, $(STDLIBS_EXT)) clean
