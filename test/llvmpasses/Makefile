SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/../..)
include $(JULIAHOME)/Make.inc
test:
	PATH=$(build_bindir):$(build_depsbindir):$$PATH $(build_depsbindir)/lit/lit.py -v $(SRCDIR)
.PHONY: test
