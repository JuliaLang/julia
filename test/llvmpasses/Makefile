SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/Make.inc

check: .

TESTS = $(patsubst $(SRCDIR)/%,%,$(wildcard $(SRCDIR)/*.jl $(SRCDIR)/*.ll))

. $(TESTS):
	PATH=$(build_bindir):$(build_depsbindir):$$PATH \
	LD_LIBRARY_PATH=${build_libdir}:$$LD_LIBRARY_PATH \
	$(build_depsbindir)/lit/lit.py -v $(addprefix $(SRCDIR)/,$@)

.PHONY: $(TESTS) check all .
