SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/Make.inc

check: $(SRCDIR)

TESTS = $(patsubst $(SRCDIR)/%,%,$(wildcard $(SRCDIR)/*.jl $(SRCDIR)/*.ll))

$(SRCDIR) $(TESTS):
	PATH=$(build_bindir):$(build_depsbindir):$$PATH \
	LD_LIBRARY_PATH=${build_libdir}:$$LD_LIBRARY_PATH \
	$(build_depsbindir)/lit/lit.py -v $@

.PHONY: $(TESTS) $(SRCDIR) check all
