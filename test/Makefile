SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc
VERSDIR := v$(shell cut -d. -f1-2 < $(JULIAHOME)/VERSION)
STDLIBDIR := $(build_datarootdir)/julia/stdlib/$(VERSDIR)
# TODO: this Makefile ignores BUILDDIR, except for computing JULIA_EXECUTABLE

TESTGROUPS = unicode strings compiler
TESTS = all stdlib $(TESTGROUPS) \
		$(patsubst $(STDLIBDIR)/%/,%,$(dir $(wildcard $(STDLIBDIR)/*/.))) \
		$(filter-out runtests testdefs, \
			$(patsubst $(SRCDIR)/%.jl,%,$(wildcard $(SRCDIR)/*.jl))) \
		$(foreach group,$(TESTGROUPS), \
			$(patsubst $(SRCDIR)/%.jl,%,$(wildcard $(SRCDIR)/$(group)/*.jl)))
# run `make print-TESTS` to see a list of all tests that can be run

EMBEDDING_ARGS := "JULIA=$(JULIA_EXECUTABLE)" "BIN=$(SRCDIR)/embedding" "CC=$(CC)"

GCEXT_ARGS := "JULIA=$(JULIA_EXECUTABLE)" "BIN=$(SRCDIR)/gcext" "CC=$(CC)"

default: all

$(TESTS):
	@cd $(SRCDIR) && \
	$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --check-bounds=yes --startup-file=no ./runtests.jl $@)

$(addprefix revise-, $(TESTS)): revise-% :
	@cd $(SRCDIR) && \
    $(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --check-bounds=yes --startup-file=no ./runtests.jl --revise $*)

embedding:
	@$(MAKE) -C $(SRCDIR)/$@ check $(EMBEDDING_ARGS)

gcext:
	@$(MAKE) -C $(SRCDIR)/$@ check $(GCEXT_ARGS)

clean:
	@$(MAKE) -C embedding $@ $(EMBEDDING_ARGS)
	@$(MAKE) -C gcext $@ $(GCEXT_ARGS)

.PHONY: $(TESTS) $(addprefix revise-, $(TESTS)) embedding gcext clean
