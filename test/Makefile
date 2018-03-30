SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR := .
STDLIBDIR := $(abspath $(JULIAHOME)/stdlib)
include $(JULIAHOME)/Make.inc
# TODO: this Makefile ignores BUILDDIR, except for computing JULIA_EXECUTABLE

TESTGROUPS = unicode strings
TESTS = all stdlib $(TESTGROUPS) \
        $(patsubst $(STDLIBDIR)/%/,%,$(dir $(wildcard $(STDLIBDIR)/*/.))) \
		$(filter-out TestHelpers runtests testdefs, \
			$(patsubst $(SRCDIR)/%.jl,%,$(wildcard $(SRCDIR)/*.jl))) \
		$(foreach group,$(TESTGROUPS), \
			$(patsubst $(SRCDIR)/%.jl,%,$(wildcard $(SRCDIR)/$(group)/*.jl)))
# run `make print-TESTS` to see a list of all tests that can be run

EMBEDDING_ARGS := "JULIA=$(JULIA_EXECUTABLE)" "BIN=$(SRCDIR)/embedding" "CC=$(CC)"

default: all

$(TESTS):
	@cd $(SRCDIR) && \
	$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --check-bounds=yes --startup-file=no ./runtests.jl $@)

embedding:
	@$(MAKE) -C $(SRCDIR)/$@ check $(EMBEDDING_ARGS)

clean:
	@$(MAKE) -C embedding $@ $(EMBEDDING_ARGS)

.PHONY: $(TESTS) embedding clean
