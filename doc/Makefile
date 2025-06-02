# Makefile for building documentation

default: html

# You can set these variables from the command line.
SRCDIR           := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME        := $(abspath $(SRCDIR)/..)
SRCCACHE         := $(abspath $(JULIAHOME)/deps/srccache)
include $(JULIAHOME)/Make.inc
JULIA_EXECUTABLE := $(call spawn,$(build_bindir)/julia) --startup-file=no

.PHONY: help clean cleanall html pdf deps deploy

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "  html  to make standalone HTML files"
	@echo "  pdf   to make standalone PDF file"
	@echo
	@echo "To run linkcheck, use 'make <target> linkcheck=true'"
	@echo "To run doctests, use 'make <target> doctest=true'"
	@echo "To fix outdated doctests, use 'make <target> doctest=fix'"
	@echo "To run doctests using Revise (to test changes without rebuilding the sysimage), use 'make <target> doctest=true revise=true'"

VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)
DOCUMENTER_OPTIONS := linkcheck=$(linkcheck) doctest=$(doctest) buildroot=$(call cygpath_w,$(BUILDROOT)) \
    texplatform=$(texplatform) revise=$(revise) stdlibdir=$(call cygpath_w,$(build_datarootdir)/julia/stdlib/$(VERSDIR)/)

UNICODE_DATA_VERSION=13.0.0
$(SRCCACHE)/UnicodeData-$(UNICODE_DATA_VERSION).txt:
	@mkdir -p "$(SRCCACHE)"
	$(JLDOWNLOAD) "$@" https://www.unicode.org/Public/$(UNICODE_DATA_VERSION)/ucd/UnicodeData.txt

# NEWS.md and stdlib are in-tree build artifacts - don't link them for oot builds.
DOC_FILES=$(filter-out NEWS.md stdlib,$(notdir $(wildcard $(SRCDIR)/src/*)))
src/%:
	@mkdir -p src
	ln -s $(SRCDIR)/src/$* $@
src: $(addprefix src/,$(DOC_FILES))

deps: $(SRCCACHE)/UnicodeData-$(UNICODE_DATA_VERSION).txt
	$(JLCHECKSUM) "$<"
	cp "$<" UnicodeData.txt

alldeps: deps
	$(JULIA_EXECUTABLE) --color=yes $(call cygpath_w,$(SRCDIR)/make.jl) deps $(DOCUMENTER_OPTIONS)

checksum-unicodedata: $(SRCCACHE)/UnicodeData-$(UNICODE_DATA_VERSION).txt
	$(JLCHECKSUM) "$<"

clean:
	rm -rf _build/* deps/* docbuild.log UnicodeData.txt

cleanall: clean

html: deps
	@echo "Building HTML documentation."
	$(JULIA_EXECUTABLE) --color=yes $(call cygpath_w,$(SRCDIR)/make.jl) $(DOCUMENTER_OPTIONS)
	@echo "Build finished. The HTML pages are in _build/html."

pdf: deps
	@echo "Building PDF documentation."
	$(JULIA_EXECUTABLE) --color=yes $(call cygpath_w,$(SRCDIR)/make.jl) -- pdf $(DOCUMENTER_OPTIONS)
	@echo "Build finished."

# The deploy target should only be called in CI builds
deploy: deps
	@echo "Deploying HTML documentation."
	$(JULIA_EXECUTABLE) --color=yes $(call cygpath_w,$(SRCDIR)/make.jl) -- deploy $(DOCUMENTER_OPTIONS)
	@echo "Build & deploy of docs finished."

update-documenter:
	@echo "Updating Documenter."
	JULIA_PKG_PRECOMPILE_AUTO=0 $(JULIA_EXECUTABLE) --project=$(call cygpath_w,$(SRCDIR)/../deps/jlutilities/documenter/) --color=yes -e 'using Pkg; Pkg.update("Documenter")'
