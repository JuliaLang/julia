# Makefile for building documentation

default: html

# You can set these variables from the command line.
SRCDIR           := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME        := $(abspath $(SRCDIR)/..)
include $(JULIAHOME)/Make.inc
JULIA_EXECUTABLE := $(call spawn,$(build_bindir)/julia)

.PHONY: help clean cleanall html pdf linkcheck doctest check

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "  html      to make standalone HTML files"
	@echo "  pdf       to make standalone PDF file"
	@echo "  linkcheck to check all external links for integrity"
	@echo "  doctest   to run all doctests embedded in the documentation"
	@echo "  check     to run linkcheck and doctests"

UnicodeData.txt:
	$(JLDOWNLOAD) http://www.unicode.org/Public/9.0.0/ucd/UnicodeData.txt

deps: UnicodeData.txt
	$(JLCHECKSUM) UnicodeData.txt

clean:
	-rm -rf _build/* deps/* docbuild.log UnicodeData.txt

cleanall: clean

html: deps
	@echo "Building HTML documentation."
ifneq ($(OS),WINNT)
	$(JULIA_EXECUTABLE) make.jl -- deploy
else
# work around issue #11727, windows output redirection breaking on buildbot
	$(JULIA_EXECUTABLE) make.jl -- deploy > docbuild.log 2>&1
	@cat docbuild.log
endif
	@echo "Build finished. The HTML pages are in _build/html."

pdf: deps
	@echo "Building PDF documentation."
	$(JULIA_EXECUTABLE) make.jl -- pdf
	@echo "Build finished."

linkcheck: deps
	@echo "Checking external documentation links."
	$(JULIA_EXECUTABLE) make.jl -- linkcheck
	@echo "Checks finished."

doctest: deps
	@echo "Running all embedded 'doctests'."
	$(JULIA_EXECUTABLE) --color=yes make.jl -- doctest
	@echo "Checks finished."

check: deps
	@echo "Running all embedded 'doctests' and checking external links."
	$(JULIA_EXECUTABLE) --color=yes make.jl -- doctest linkcheck
	@echo "Checks finished."

.PHONY: deps
